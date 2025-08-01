;;; julia-repl.el --- A minor mode for a Julia REPL -*- lexical-binding:t; no-byte-compile:t -*-

;; Copyright (C) 2016–2024 Tamas K. Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: languages
;; Version: 1.5.1
;; Package-Requires: ((emacs "29.1")(s "1.12"))
;; URL: https://github.com/tpapp/julia-repl

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-julia-repl")
;; (require 'julia-repl)

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; Run a julia REPL inside a terminal in Emacs.  In contrast to ESS, use
;; the Julia REPL facilities for interactive features, such readline,
;; help, debugging.

;;; Code:

(require 'term)
(require 'cl-generic)
(require 'cl-lib)
(require 'compile)
(require 's)
(require 'seq)
(require 'subr-x)

;;
;; customizations
;;

(defgroup julia-repl nil
  "A minor mode for a Julia REPL"
  :group 'julia)

(defcustom julia-repl-inferior-buffer-name-base "julia"
  "Prefix for the names for inferior REPL buffers.

See ‘julia-repl--inferior-buffer-name’."
  :type 'string
  :group 'julia-repl)

(defcustom julia-repl-hook nil
  "Hook to run after starting a Julia REPL term buffer. For all functions, it can be assumed that the current buffer is an inferior Julia buffer with a running REPL."
  :type 'hook
  :group 'julia-repl)

(defcustom julia-repl-captures (list (kbd "M-x"))
  "List of key sequences that are passed through (the global binding is used).

Note that this affects all buffers using the ‘ansi-term’ map."
  :type '(repeat key-sequence)
  :group 'julia-repl)

(defcustom julia-repl-compilation-mode t
  "When non-nil, highlight error locations using function ‘compilation-shell-minor-mode’."
  :type 'boolean
  :group 'julia-repl)

(defcustom julia-repl-save-buffer-on-send nil
  "When non-nil, save buffer without prompting on send."
  :type 'boolean
  :group 'julia-repl)

(defcustom julia-repl-pop-to-buffer t
  "When non-nil pop to julia repl"
  :type 'symbol
  :group 'julia-repl)

(defcustom julia-repl-path-rewrite-rules nil
  "A list of rewrite rules applied to paths sent with ‘include’ and similar.

Each rule should be a function that is called with a path, in the given order. If the function returns ‘nil’, the next one is tried, otherwise the result is used as the rewritten path.

This can be used a workaround, usually necessary on non-Unix systems. Eg for Cygwin-based Windows, use

   (setq julia-repl-path-rewrite-rules julia-repl-cygwin-path-rewrite-rules)

in your Emacs init file after loading this package."
  :type 'list
  :group 'julia-repl)

(defcustom julia-repl-path-cygwin-prefix "c:/cygwin64"
  "Prepended to paths by some Cygwin rewrite rules when no other information is available."
  :type 'string
  :group 'julia-repl)

(defcustom julia-repl-set-term-escape t
  "Set the escape char C-x globally for term. Useful for switching windows, but users who do not want this globally should set it to nil."
  :type 'boolean
  :group 'julia-repl)

(defcustom julia-repl-skip-comments nil
  "Make some send commands (currently `julia-repl-send-line' and `julia-repl-send-region-or-line') skip comments."
  :type 'boolean
  :group 'julia-repl)

;;;; utility functions

(defun julia-repl--add-earmuffs (buffer-name)
  "Add earmuffs (*'s) to BUFFER-NAME.

This matches the buffer name (eg created by ‘make-term’)."
  (concat "*" buffer-name "*"))

;;;; terminal backends

;;; generic api

(cl-defgeneric julia-repl--locate-live-buffer (terminal-backend name)
  "Return the inferior buffer with NAME if it has a running REPL, otherwise NIL.")

(cl-defgeneric julia-repl--make-buffer (terminal-backend name executable-path switches)
    "Make and return a new inferior buffer.

Buffer will be named with NAME (earmuffs added by this function), starting julia using EXECUTABLE-PATH with SWITCHES (a list of strings).")

(cl-defgeneric julia-repl--send-to-backend (terminal-backend buffer string paste-p ret-p)
  "Send a string to BUFFER using the given backend.

When PASTE-P, “bracketed paste” mode will be used. When RET-P, terminate with an extra newline.")

;;;; ansi-term

(cl-defstruct julia-repl--buffer-ansi-term
  "Terminal backend via ‘ansi-term’, available in Emacs.")

(cl-defmethod julia-repl--locate-live-buffer ((_terminal-backend julia-repl--buffer-ansi-term)
                                              name)
  (if-let ((inferior-buffer (get-buffer (julia-repl--add-earmuffs name))))
      (with-current-buffer inferior-buffer
        (cl-assert (eq major-mode 'term-mode) nil "Expected term-mode. Changed mode or backends?"))
      (when (term-check-proc inferior-buffer)
        inferior-buffer)))

(cl-defmethod julia-repl--make-buffer ((_terminal-backend julia-repl--buffer-ansi-term)
                                       name executable-path switches)
  (let ((inferior-buffer (apply #'make-term name executable-path nil switches)))
    (with-current-buffer inferior-buffer
      (mapc (lambda (k)
              (define-key term-raw-map k (global-key-binding k)))
            julia-repl-captures)
      (term-char-mode)
      (when julia-repl-set-term-escape
        (term-set-escape-char ?\C-x))      ; useful for switching windows
      (setq-local term-prompt-regexp "^(julia|shell|help\\?|(\\d+\\|debug ))>")
      (when (version< emacs-version "27")
        (setq-local term-suppress-hard-newline t))  ; reflow text
      (setq-local term-scroll-show-maximum-output t)
      ;; do I need this?
      (setq-local term-scroll-to-bottom-on-output t))
    inferior-buffer))

(cl-defmethod julia-repl--send-to-backend ((_terminal-backend julia-repl--buffer-ansi-term)
                                           buffer string paste-p ret-p)
  "Send a string to BUFFER using the given backend.

When PASTE-P, “bracketed paste” mode will be used. When RET-P, terminate with an extra newline."
  (with-current-buffer buffer
    (when paste-p                       ; bracketed paste start
      (term-send-raw-string "\e[200~"))
    (term-send-raw-string string)
    (when ret-p                         ; return
      (term-send-raw-string "\^M"))
    (when paste-p                       ; bracketed paste stop
      (term-send-raw-string "\e[201~"))))

;;; vterm

(with-eval-after-load 'vterm

  (defun julia-repl--next-error-function (n &optional reset)
    "A workaround for https://github.com/akermu/emacs-libvterm/issues/316."
    ;; NOTE remove when that issue is fixed
    (interactive "p")
    (goto-char (point))
    (compilation-next-error-function n reset))

  (cl-defstruct julia-repl--buffer-vterm
    "Terminal backend using ‘vterm’, which needs to be installed and loaded.")

  (cl-defmethod julia-repl--locate-live-buffer ((_terminal-backend julia-repl--buffer-vterm)
                                                name)
    (if-let ((inferior-buffer (get-buffer (julia-repl--add-earmuffs name))))
        (with-current-buffer inferior-buffer
          (cl-assert (eq major-mode 'vterm-mode) nil "Expected vterm-mode. Changed mode or backends?")
          (when (vterm-check-proc inferior-buffer)
            inferior-buffer))))

  (cl-defmethod julia-repl--make-buffer ((_terminal-backend julia-repl--buffer-vterm)
                                         name executable-path switches)
    (let ((vterm-buffer (get-buffer-create (julia-repl--add-earmuffs name)))
          (inhibit-read-only t))
      (with-current-buffer vterm-buffer
        (let ((vterm-shell (s-join " " (cons executable-path switches))))
          (vterm-mode)
          (local-set-key (kbd "C-c C-z") #'julia-repl--switch-back)
          ;; NOTE workaround for https://github.com/akermu/emacs-libvterm/issues/316, remove when fixed
          (add-hook 'compilation-shell-minor-mode-hook
                    ;; NOTE run *after* vterm's hook and overwrite `next-error-function'
                    (lambda () (setq next-error-function 'julia-repl--next-error-function))
                    t t)))
      vterm-buffer))

  (cl-defmethod julia-repl--send-to-backend ((_terminal-backend julia-repl--buffer-vterm)
                                             buffer string paste-p ret-p)
    (with-current-buffer buffer
      (vterm-send-string string paste-p)
      (when ret-p
        (vterm-send-return)))))

;;; eat term

(with-eval-after-load 'eat

  (cl-defstruct julia-repl--buffer-eat
    "Terminal backend using ‘eat’, which needs to be installed and loaded.")

  (cl-defmethod julia-repl--locate-live-buffer ((_terminal-backend julia-repl--buffer-eat)
						name)
    (if-let ((inferior-buffer (get-buffer (julia-repl--add-earmuffs name))))
	(with-current-buffer inferior-buffer
	  (cl-assert (eq major-mode 'eat-mode) nil "Expected eat-mode. Changed mode or backends?")
	  (when (eat-term-live-p inferior-buffer)
	    inferior-buffer))))

  (cl-defmethod julia-repl--make-buffer ((_terminal-backend julia-repl--buffer-eat)
					 name executable-path switches)
    (let ((inferior-buffer (apply #'eat-make name executable-path nil switches)))
      (with-current-buffer inferior-buffer
	(mapc (lambda (k)
		(define-key eat-semi-char-mode-map k (global-key-binding k)))
	      julia-repl-captures)
	(local-set-key (kbd "C-c C-z") #'julia-repl--switch-back))
      inferior-buffer))

  (cl-defmethod julia-repl--send-to-backend ((_terminal-backend julia-repl--buffer-eat)
					     buffer string paste-p ret-p)
    (with-current-buffer buffer
      (if paste-p
	  (eat-term-send-string-as-yank eat-terminal string)
        (eat-term-send-string eat-terminal string))
      (when ret-p
	(eat-term-send-string eat-terminal "\^M")))))

(defconst julia-repl--CR-at
   (rx "@" space
       (? (group (one-or-more (or  (any "._") alnum))) space)    ; group 1: module name
       (group (+ (not (any space ">" "<" "(" ")" "\t" "\n" "," "'" "\"" ";" ":")))) ; group 2: path
       ":"
       (group (+ num))                    ; group 3: line number
       )
   "Matches “@ Foo ~/code/Foo/src/Foo.jl:100”. This is what is used in Julia >= 1.6")

(defconst julia-repl--CR-filename
  (rx (one-or-more (not (any " ><()\t\n,'\";:"))))
  "An attempt to match filenames in error, info,  and warning messages printed by Julia.")

(defconst julia-repl--CR-load-error
  (rx
   "while loading "
   (group (regexp julia-repl--CR-filename))
   ", in expression starting on line "
   (group (one-or-more digit)))
  "Compilation regexp matching “while loading /tmp/Foo.jl, in expression starting on line 2”.")

(defconst julia-repl--CR-around
  (rx
   (or "around" "at" "Revise")
   " "
   (group (regexp julia-repl--CR-filename))
   ":"
   (group (one-or-more digit)))
  "Compilation regexp matching “around /tmp/Foo.jl:2”, also starting with “at or “Revise”")

;;
;; global variables
;;

(defvar julia-repl--terminal-backend
  (make-julia-repl--buffer-ansi-term)
  "Terminal backend, for internal use. Set using `julia-repl-set-terminal-backend'.")

(defun julia-repl-set-terminal-backend (backend)
  "Set terminal backend for `julia-repl'.

Valid backends are currently:

- ‘ansi-term’, using the ANSI terminal built into Emacs.

- ‘vterm’, which requires that vterm is installed. See URL ‘https://github.com/akermu/emacs-libvterm’.

- ‘eat’, which requires that eat is installed. See URL ‘https://codeberg.org/akib/emacs-eat’."
  (interactive "S")
  (cl-case backend
    (ansi-term
     (setq julia-repl--terminal-backend (make-julia-repl--buffer-ansi-term)))
    (vterm
     (require 'vterm)
     (setq julia-repl--terminal-backend (make-julia-repl--buffer-vterm))
     (add-to-list 'vterm-eval-cmds '("julia-repl--show" julia-repl--show)))
    (eat
     (require 'eat)
     (setq julia-repl--terminal-backend (make-julia-repl--buffer-eat)))
    (otherwise
     (error "Unrecognized backend “%s”." backend))))

(defvar julia-repl-executable-records
  '((default "julia"))
  "List of Julia executables.

Entries have the form

  (KEY EXECUTABLE-PATH :BASEDIR BASEDIR)

`executable-path' is invoked as is, so make sure that it is in the path available to Emacs.

A missing :BASEDIR will be completed automatically when first used, and it assumed to be the same afterwards.

This is used for key lookup for ‘julia-repl-executable-key’. The
first entry is the default.")

(defun julia-repl--default-executable-key ()
  "Return the default executable key."
  (let ((key (caar julia-repl-executable-records)))
    (cl-assert key nil "Could not find any key in JULIA-REPL-EXECUTABLE-RECORDS.")
    key))

(defvar julia-repl-inferior-buffer-name-suffix nil
  "Name for the Julia REPL buffer associated with a source code buffer.

Can be a symbol (with NIL being the default) or a number. See
‘julia-repl--inferior-buffer-name’ for details on how it is
used to generate a buffer name.")

(defvar-local julia-repl--inferior-buffer-suffix nil
  "Suffix for a specific inferior buffer.

These are used for offering choices when selecting new suffix.
For internal use only.")

(defvar-local julia-repl--script-buffer nil
  "Buffer active before calling `julia-repl'.")

(defvar julia-repl-executable-key nil
  "Key for the executable associated with the buffer.

Looked up in ‘julia-repl-executable-records’. When nil, the
first value is used.

See ‘julia-repl--inferior-buffer-name’ for how it is used to
generate a buffer name.")

(defun julia-repl--get-executable-key ()
  "Return the executable key, picking the first one if it was not set."
  (or julia-repl-executable-key (julia-repl--default-executable-key)))

(defvar julia-repl-switches nil
  "Command line switches for the Julia executable.

Valid values are NIL or a string. These take effect the next time
a new Julia process is started.")

(defvar julia-repl-compilation-location-legacy nil
  "Whether to include recognize various legacy error messages in compilation output.
 Mainly useful if you are using Julia <1.6.")

(defun julia-repl--compilation-regexp-alist ()
  "Return an alist suitable for use in `compilation-error-regexp-alist' for recognizing Julia error locations.

 Cf `julia-repl-compilation-location-legacy'."
  (let ((regexp-alist `((,julia-repl--CR-at 2 3))))
    (if julia-repl-compilation-location-legacy
        (cons regexp-alist
              `((,julia-repl--CR-load-error 1 2) (,julia-repl--CR-around 1 2)))
      regexp-alist)))

;;
;; REPL buffer creation and setup
;;

(cl-defun julia-repl--inferior-buffer-name
    (&optional (executable-key (julia-repl--get-executable-key))
               (suffix julia-repl-inferior-buffer-name-suffix))
  "Name for a Julia REPL inferior buffer.

The name is a string, constructed from JULIA-REPL-INFERIOR-NAME-BASE and EXECUTABLE-KEY (used only when different from the global default), and the SUFFIX.

An integer SUFFIX is formatted as “<SUFFIX>”, while a symbol is added as “-SUFFIX.”

Note that ‘make-term’ surrounds this string by *'s when converted to a buffer name. See ‘julia-repl--add-earmuffs’."
  (let* ((middle (if (eq executable-key (julia-repl--default-executable-key))
                     ""
                   (format "-%s" executable-key)))
         (last (cond
                ((null suffix) "")
                ((integerp suffix) (format "<%d>" suffix))
                ((symbolp suffix) (format "-%s" suffix))
                (t (error
                    "Inferior name suffix should be an integer or a symbol")))))
    (concat julia-repl-inferior-buffer-name-base middle last)))


(cl-defun julia-repl--capture-basedir (executable-path)
  "Attempt to obtain the Julia base directory by querying the Julia executable.

When NIL, this was unsuccessful. In this case, debug information
is printed to the *Messages* buffer."
  (let* ((prefix "OK") ; prefix is used to verify that there was no error and help with extraction
         (expr (concat "print(\"" prefix
                       "\" * normpath(joinpath(VERSION <= v\"0.7-\" ? JULIA_HOME : Sys.BINDIR, "
                       "Base.DATAROOTDIR, \"julia\", \"base\")))"))
         (switches " --history-file=no --startup-file=no -qe ")
         (maybe-basedir (shell-command-to-string
                         (concat executable-path switches (concat "'" expr "'")))))
    (if (string-prefix-p prefix maybe-basedir)
        (substring maybe-basedir (length prefix))
      (progn
        (message "Julia basedir query returned “%s”" maybe-basedir)
        nil))))

(defun julia-repl--executable-path (executable-record)
  "Retrun the Julia executable for the given EXECUTABLE-RECORD.

It is checked for being a string."
  (let ((executable-path (cl-second executable-record)))
    (cl-assert (stringp executable-path) nil "No valid executable path found in %s" executable-record)
    executable-path))

(defun julia-repl--complete-executable-record! (executable-record)
  "Complete EXECUTABLE-RECORD if necessary.

Queries and appends missing information if necessary.

Note: when cannot capture the base dir, it is set to NIL to
prevent further attempts."
  (unless (plist-member (cddr executable-record) :basedir)
    (let* ((executable-path (julia-repl--executable-path executable-record))
           (basedir (julia-repl--capture-basedir executable-path)))
      (nconc executable-record `(:basedir ,basedir))
      (unless basedir
        (warn "could not capture basedir for Julia executable %s"
              executable-path)))))

(defun julia-repl--setup-compilation-mode (inferior-buffer basedir)
  "Setup compilation mode for the the current buffer in INFERIOR-BUFFER.

BASEDIR is used for resolving relative paths."
  (with-current-buffer inferior-buffer
    (setq-local compilation-error-regexp-alist (julia-repl--compilation-regexp-alist))
    (when basedir
      (setq-local compilation-search-path (list basedir)))
    (compilation-shell-minor-mode 1)))

(defun julia-repl--run-hooks (inferior-buffer)
  "Run the hooks in ‘julia-repl-hook’ in INFERIOR-BUFFER."
  (with-current-buffer inferior-buffer
    (run-hooks 'julia-repl-hook)))

(cl-defun julia-repl--executable-record (executable-key)
  "Return the executable record for EXECUTABLE-KEY.

It is looked up in ‘julia-repl-executable-records’. An error is
raised if not found."
  (let ((executable-record (assq executable-key julia-repl-executable-records)))
    (unless executable-record
      (error "Could not find %s in JULIA-REPL-EXECUTABLE-RECORDS"
             executable-key))
    executable-record))

;;
;; prompting for executable-key and suffix
;;

(defun julia-repl--matching-inferior-buffers (executable-key)
  "A list of macthing inferior buffers for the current source buffer.

Matches the EXECUTABLE-KEY, without the suffix."
  (let* ((inferior-buffer-name-root
          (julia-repl--inferior-buffer-name executable-key nil))
         (inferior-buffer-name-regexp (concat "\\*"
                                              inferior-buffer-name-root
                                              ".*\\*")))
    (seq-filter (lambda (buffer)
                  (string-match inferior-buffer-name-regexp
                                (buffer-name buffer)))
                (buffer-list))))

(defun julia-repl--read-inferior-buffer-name-suffix (executable-key)
  "Completing read of a inferior buffer name suffix for the Julia REPL.

Returns a symbol, from interning the string that is read. The
completions offered are specific to the EXECUTABLE-KEY.

See ‘julia-repl--inferior-buffer-name’."
  (let* ((matching-inferior-buffers
          (julia-repl--matching-inferior-buffers executable-key))
         (suffix-buffer-alist (mapcar
                               (lambda (buffer)
                                 (cons (buffer-local-value
                                        'julia-repl--inferior-buffer-suffix
                                        buffer)
                                       buffer))
                               matching-inferior-buffers))
         (suffix-buffer-alist (cl-stable-sort suffix-buffer-alist
                                              (lambda (x y)
                                                (or (not x)
                                                    (string< (prin1-to-string x)
                                                             (prin1-to-string y))))
                                              :key #'car))
         (suffix (completing-read "julia-repl inferior buffer name suffix: "
                                  suffix-buffer-alist)))
    (message "suffix buffer alist %s" suffix)
    (intern suffix)))

(cl-defun julia-repl--unused-inferior-buffer-name-index (executable-key)
  "First positive integer that is not used as an inferior buffer name suffix.

See ‘julia-repl--inferior-buffer-name’."
  (let ((index 1))
    (while (get-buffer (julia-repl--add-earmuffs
                        (julia-repl--inferior-buffer-name executable-key
                                                          index)))
      (cl-incf index))
    index))

(defun julia-repl-prompt-set-inferior-buffer-name-suffix (arg)
  "Prompt for and set a Julia REPL inferior buffer name for the current buffer.

A prefix argument ARG modifies the behavior:

- \\[negative-argument] selects the next unused number for the suffix (ie a new
buffer),
- an numerical prefix selects that integer for the suffix.

Both of these happen without prompting."
  (interactive "P")
  (let* ((executable-key (julia-repl--get-executable-key))
         (suffix (cond
                  ((null arg)
                   (julia-repl--read-inferior-buffer-name-suffix executable-key))
                  ((eq arg '-)
                   (julia-repl--unused-inferior-buffer-name-index executable-key))
                  ((integerp arg)
                   arg)
                  ((listp arg)
                   (cl-first arg)))))
    (setq julia-repl-inferior-buffer-name-suffix suffix)
    (message "julia-repl-inferior-buffer-name-suffix set to %s" suffix)))

(defun julia-repl-prompt-switches ()
  "Read and set the switches for the inferior process."
  (interactive)
  (let ((switches (read-string "switches for the julia process: " julia-repl-switches)))
    (message "julia-repl-switches set to \"%s\"" switches)
    (setq julia-repl-switches switches)))

(defun julia-repl-prompt-executable-key ()
  "Prompt for the key of the Julia REPL executable.

Valid keys are the first items in ‘julia-repl-executable-records’."
  (intern
   (completing-read "julia-repl executable: "
                    julia-repl-executable-records nil t nil)))

(defun julia-repl-prompt-set-executable-key ()
  "Prompt and save the key of the Julia REPL executable.

Valid keys are the first items in ‘julia-repl-executable-records’."
  (interactive)
  (let ((key (julia-repl-prompt-executable-key)))
    (setq julia-repl-executable-key key)
    (message "julia-repl-executable-key set to %s"
             (propertize (symbol-name key) 'face 'font-lock-constant-face))))

;;
;; high-level functions
;;

(cl-defun julia-repl-inferior-buffer (&key (executable-key (julia-repl--get-executable-key))
                                           (suffix julia-repl-inferior-buffer-name-suffix)
                                           (terminal-backend julia-repl--terminal-backend ))
  "Return the Julia REPL inferior buffer, creating one if it does not exist."
  (let* ((name (julia-repl--inferior-buffer-name executable-key suffix))
         (live-buffer (julia-repl--locate-live-buffer terminal-backend name)))
    (if live-buffer
        live-buffer
      (let ((executable-record (julia-repl--executable-record executable-key))
            (switches julia-repl-switches))
        (julia-repl--complete-executable-record! executable-record)
        (let* ((executable-path (julia-repl--executable-path executable-record))
               (basedir (plist-get (cddr executable-record) :basedir))
               (inferior-buffer (julia-repl--make-buffer terminal-backend name executable-path
                                                         (when switches
                                                           (split-string switches)))))
          (when julia-repl-compilation-mode
            (julia-repl--setup-compilation-mode inferior-buffer basedir))
          (julia-repl--run-hooks inferior-buffer)
          (with-current-buffer inferior-buffer
            (setq-local julia-repl--inferior-buffer-suffix suffix))
          inferior-buffer)))))

;;;###autoload
(defun julia-repl ()
  "Raise the Julia REPL inferior buffer, creating one if it does not exist.

This is the standard entry point for using this package."
  (interactive)
  (let ((script-buffer (current-buffer))
	(inferior-buffer (julia-repl-inferior-buffer)))
    (with-current-buffer inferior-buffer
      (setq julia-repl--script-buffer script-buffer))
    (if julia-repl-pop-to-buffer
	(pop-to-buffer inferior-buffer)
      (switch-to-buffer inferior-buffer))))

(defun julia-repl--switch-back ()
  "Switch to the buffer that was active before last call to `julia-repl'."
  (interactive)
  (when (buffer-live-p julia-repl--script-buffer)
    (if julia-repl-pop-to-buffer 
	(switch-to-buffer-other-window julia-repl--script-buffer)
      (switch-to-buffer julia-repl--script-buffer))))

;;
;; path rewrites
;;

(defun julia-repl--path-rewrite (path rules)
  "Call each rule (function) in ‘rules’ with ‘path’. When the
result is non-nil, return that and terminate, when all rules are
tested return ‘path’ unchanged."
  (let ((result nil))
    (while (and (not result) rules)
      (setf result (funcall (car rules) path)
            rules (cdr rules)))
    (if result
        result
      path)))

(defun julia-repl--cygwin-replace-cygdrive (path)
  "Rewrite ‘/cygdrive/c/something’ to ‘c:/something’."
  (let ((m (s-match-strings-all "^/cygdrive/\\([A-Za-z]\\)\\(/.*\\)$" path)))
    (when m
      (let ((m1 (cl-first m)))
        (s-concat (cl-second m1) ":" (cl-third m1))))))

(defun julia-repl--cygwin-add-drive (path)
  "When the path does not start with a Windows drive letter,
prepend ‘julia-repl-path-cygwin-prefix’."
  (unless (s-matches? "^[A-Za-z]:/" path)
    (s-concat julia-repl-path-cygwin-prefix path)))

(defconst julia-repl-cygwin-path-rewrite-rules
  (list #'julia-repl--cygwin-replace-cygdrive
        #'julia-repl--cygwin-add-drive)
  "Default list of rewrite rules for Cygwin. Use as a starting
  point, you may need to copy and modify this. See
  ‘julia-repl-path-cygwin-prefix’.")

;;
;; sending to the REPL
;;

(defun julia-repl--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Julia REPL term buffer.

The string is trimmed, then a closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to ‘current-prefix-arg’,
  3. otherwise no newline.

Unless NO-BRACKETED-PASTE, bracketed paste control sequences are used."
  (when (eq no-newline 'prefix)
    (setq no-newline current-prefix-arg))
  (let ((inferior-buffer (julia-repl-inferior-buffer)))
    (if julia-repl-pop-to-buffer
	(display-buffer inferior-buffer))
    (julia-repl--send-to-backend julia-repl--terminal-backend
                                 inferior-buffer (s-trim string) (not no-bracketed-paste)
                                 (not no-newline))))

(defun julia-repl--forward-skip-comments ()
  "Move one line forward, then skip any comments when `julia-repl-skip-comments' is set."
  (forward-line)
  (when julia-repl-skip-comments
    (forward-comment (buffer-size))))

(defun julia-repl-send-line ()
  "Send the current line to the Julia REPL term buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `julia-repl-send-region-or-line'."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t) 'prefix t)
  (julia-repl--forward-skip-comments))

(defun julia-repl-send-region-or-line (&optional prefix suffix)
  "Send active region (if any) or current line to the inferior buffer.

Closed with a newline, unless used with a prefix argument.

When PREFIX and SUFFIX are given, they are concatenated before
and after."
  (interactive)
  (cl-flet ((-send-string (string)
                          (julia-repl--send-string
                           (concat prefix string suffix) 'prefix)))
    (if (use-region-p)
        (progn
          (-send-string (buffer-substring-no-properties
                         (region-beginning) (region-end)))
          (deactivate-mark))
      (progn
        (-send-string (thing-at-point 'line t))
        (julia-repl--forward-skip-comments)))))

(defun julia-repl-edit ()
  "Call @edit on the expression.

Selection semantics same as ‘julia-repl-send-region-or-line’."
  (interactive)
  (julia-repl-send-region-or-line "@edit "))

(defun julia-repl-macroexpand ()
  "Expand the expression as a macro.

Selection semantics same as ‘julia-repl-send-region-or-line’."
  (interactive)
  (julia-repl-send-region-or-line "macroexpand(Main, quote " " end)"))

(defun julia-repl-send-buffer (arg)
  "Send the contents of the current buffer to the Julia REPL.

Use ‘include’ by default if the buffer is associated with a file,
and is not modified (ie has been saved) or saved after
prompting.  Otherwise send the contents directly; you can force
this with a prefix argument ARG."
  (interactive "P")
  (let* ((file (and (not arg) buffer-file-name)))
    (when (and file (buffer-modified-p))
      (if (or julia-repl-save-buffer-on-send (y-or-n-p "Buffer modified, save? "))
          (save-buffer)
        (setq file nil)))
    (julia-repl--send-string
     (if file
         (concat "include(\""
                 (julia-repl--path-rewrite file julia-repl-path-rewrite-rules)
                 "\");")
       (buffer-substring-no-properties (point-min) (point-max))))))


(defun julia-repl-includet-buffer ()
  "Attempts to include a buffer via ‘Revise.includet’.

If a buffer does not correspond to a file, the function does nothing, just shows a message.

If a buffer corresponds to a file and is not saved, the function prompts the user to save.  If the user declines to save an unsaved file, the command is still run if the file otherwise exists, in which case ‘Revise’ will load it later when saved."
  (interactive)
  (let* ((file buffer-file-name))
    (if file
        (progn
          (when (buffer-modified-p)
            (if (or julia-repl-save-buffer-on-send (y-or-n-p "Buffer modified, save? "))
                (save-buffer)
              (unless (file-exists-p file)
                (message "need to save the file first"))))
          (julia-repl--send-string (concat "using Revise; Revise.includet(\""
                                           (julia-repl--path-rewrite file julia-repl-path-rewrite-rules)
                                           "\");")))
      (message "buffer does not correspond to a file"))))

(defun julia-repl--symbols-after-dot ()
  "Dot-separated symbols after point (which should be on a dot), as a list."
  (when (eq (char-after) ?.)
    (forward-char)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (cl-destructuring-bind (start1 . end1) bounds
          (let ((symbol1 (buffer-substring-no-properties start1 end1)))
            (goto-char (1+ end1))
            (cons symbol1 (julia-repl--symbols-after-dot))))))))

(defun julia-repl--symbols-before-dot ()
  "Dot-separated symbols before point (which should be on a dot), as a list, in reverse order."
  (when (eq (char-after) ?.)
    (backward-char)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (cl-destructuring-bind (start1 . end1) bounds
          (let ((symbol1 (buffer-substring-no-properties start1 end1)))
            (goto-char (1- start1))
            (cons symbol1 (julia-repl--symbols-before-dot))))))))

(defun julia-repl--symbols-at-point ()
  "Return the a list of symbols at point (eg a variable, function, or module
name), separated by dots, as a list."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (cl-destructuring-bind (start1 . end1) bounds
        (let ((symbol1 (buffer-substring-no-properties start1 end1))
              (symbols-before-dot (save-excursion
                                    (goto-char (1- start1))
                                    (julia-repl--symbols-before-dot)))
              (symbols-after-dot (save-excursion
                                   (goto-char end1)
                                   (julia-repl--symbols-after-dot))))
          (cl-concatenate 'list (nreverse symbols-before-dot) (list symbol1) symbols-after-dot))))))

(defun julia-repl-doc ()
  "Documentation for symbol at point."
  (interactive)
  (julia-repl--send-string (concat "@doc " (s-join "." (julia-repl--symbols-at-point)))))

(defun julia-repl-list-methods ()
  "List methods for symbol at point."
  (interactive)
  (julia-repl--send-string (concat "methods(" (s-join "." (julia-repl--symbols-at-point)) ")")))

(defun julia-repl-cd ()
  "Change directory to the directory of the current buffer (if applicable)."
  (interactive)
  (if-let ((directory (file-name-directory (buffer-file-name))))
      (progn
	(julia-repl--send-string (concat "cd(\""
                                         (julia-repl--path-rewrite directory julia-repl-path-rewrite-rules)
                                         "\")"))
	(with-current-buffer (julia-repl-inferior-buffer) (cd directory)))
    (warn "buffer not associated with a file")))

(cl-defun julia-repl-find-projectfile (&optional (start-filename (buffer-file-name)))
  "Find a project file in the parent directories of the `start-filename'. When no such file is found, return `nil' "
  (when start-filename
    (cl-flet ((find-projectfile (project-filename)
                (locate-dominating-file start-filename project-filename)))
      (or (find-projectfile "Project.toml")
          (find-projectfile "JuliaProject.toml")))))

(defun julia-repl-activate-parent (arg)
  "Look for a project file in the parent directories, if found, activate the project.

When called with a prefix argument, activate the home project."
  (interactive "P")
  (if arg
      (progn
        (message "activating home project")
        (julia-repl--send-string "import Pkg; Pkg.activate()"))
    (if-let ((projectfile (julia-repl-find-projectfile)))
        (progn
          (message "activating %s" projectfile)
          (julia-repl--send-string
           (concat "import Pkg; Pkg.activate(\""
                   (expand-file-name (file-name-directory projectfile)) "\")")))
      (message "could not find project file"))))

(defun julia-repl-set-julia-editor (editor)
  "Set the JULIA_EDITOR environment variable."
  (interactive)
  (julia-repl--send-string (format "ENV[\"JULIA_EDITOR\"] = \"%s\";" editor)))

(defun julia-repl-use-emacsclient ()
  "Use emacsclient as the JULIA_EDITOR."
  (interactive)
  (julia-repl--send-string "ENV[\"JULIA_EDITOR\"] = \"emacsclient\";"))

(defun julia-repl--show (kind mime base64data)
  "Show data sent from Julia via EmacsVterm.jl in some Emacs window.
KIND identifies the type of data being sent, MIME is the mime
type of the data and BASE64DATA contains the actual
Base64-encoded data.

Currently, only showing the documentation is supported, but
later, things like showing inline images or rendered LaTeX might
be added."
  (pcase `(,kind . ,mime)
    ('("documentation" . "text/html")
     (with-current-buffer-window "*julia-doc*" nil nil
       (insert (decode-coding-string (base64-decode-string base64data) 'utf-8))
       (shr-render-region (point-min) (point-max))
       (goto-char (point-min))
       (view-mode-enter)))
    (`("image" . ,_)
     (with-current-buffer-window "*julia-img*" nil nil
       (let ((imgdata (base64-decode-string base64data)))
         ;; TODO: Implement different display modes like julia-snail
         ;; allow images to be erased
         (fundamental-mode)
         (read-only-mode -1)
         (erase-buffer)
         (insert imgdata)
         (image-mode)
         (image-transform-fit-to-window)
         (add-hook 'window-size-change-functions (lambda (_) (image-transform-fit-to-window)) 0 t))))
    (_ (error "Unsupported data kind `%s' or MIME type `%s' (upgrade julia-repl or use older EmacsVterm.jl)"
              kind mime))))

;;;###autoload
(define-minor-mode julia-repl-mode
  "Minor mode for interacting with a Julia REPL running inside a term.

\\{julia-repl-mode-map}"
  :init-value nil
  :lighter ">"
  :keymap
  `((,(kbd "C-c C-a")    . julia-repl-activate-parent)
    (,(kbd "C-c C-b")    . julia-repl-send-buffer)
    (,(kbd "C-c C-c")    . julia-repl-send-region-or-line)
    (,(kbd "C-c C-d")    . julia-repl-doc)
    (,(kbd "C-c C-e")    . julia-repl-edit)
    (,(kbd "C-c C-l")    . julia-repl-list-methods)
    (,(kbd "C-c C-m")    . julia-repl-macroexpand)
    (,(kbd "C-c C-p")    . julia-repl-cd)
    (,(kbd "C-c C-s")    . julia-repl-prompt-set-inferior-buffer-name-suffix)
    (,(kbd "C-c C-t")    . julia-repl-includet-buffer)
    (,(kbd "C-c C-v")    . julia-repl-prompt-set-executable-key)
    (,(kbd "C-c C-z")    . julia-repl)
    (,(kbd "<C-return>") . julia-repl-send-line))
  (when-let ((filename (buffer-file-name)))
    (setq-local default-directory (file-name-directory filename))))

(provide 'julia-repl)
;;; julia-repl.el ends here
