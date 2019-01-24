;;; julia-repl.el --- A minor mode for a Julia REPL -*- lexical-binding:t -*-

;; Copyright (C) 2016  Tamas K. Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: languages
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
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
(require 'subr-x)
(require 'cl-lib)


;; customizations

(defgroup julia-repl nil
  "A minor mode for a Julia REPL"
  :group 'julia)

(defcustom julia-repl-inferior-buffer-name-base "julia"
  "Prefix for the names for inferior REPL buffers.

See ‘julia-repl--inferior-buffer-name’."
  :type 'string
  :group 'julia-repl)

(defcustom julia-repl-hook nil
  "Hook to run after starting a Julia REPL term buffer."
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


;; global variables

(defvar julia-repl--compilation-regexp-alist
  '(;; matches "while loading /tmp/Foo.jl, in expression starting on line 2"
    (julia-load-error . ("while loading \\([^ ><()\t\n,'\";:]+\\), in expression starting on line \\([0-9]+\\)" 1 2))
    ;; matches "around /tmp/Foo.jl:2", also starting with "at" or "Revise"
    (julia-loc . ("\\(around\\|at\\|Revise\\) \\([^ ><()\t\n,'\";:]+\\):\\([0-9]+\\)" 2 3))
    ;; matches "omitting file /tmp/Foo.jl due to parsing error near line 2", from Revise.parse_source!
    (julia-warn-revise . ("omitting file \\([^ ><()\t\n,'\";:]+\\) due to parsing error near line \\([0-9]+\\)" 1 2))
    )
  "Specifications for highlighting error locations.

Uses function ‘compilation-shell-minor-mode’.")

(defvar julia-repl-executable-records
  '((default "julia"))
  "List of Julia executables.

Entries have the form

  (KEY EXECUTABLE-PATH :BASEDIR BASEDIR)

A missing :BASEDIR will be completed automatically when first used.

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


;; REPL buffer creation and setup

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

(defun julia-repl--add-earmuffs (buffer-name)
  "Add earmuffs (*'s) to BUFFER-NAME.

This matches the buffer name created by ‘make-term’."
  (concat "*" buffer-name "*"))

(cl-defun julia-repl--capture-basedir (executable-path)
  "Attempt to obtain the Julia base directory by querying the Julia executable.

When NIL, this was unsuccessful."
  (let* ((prefix "OK") ; prefix is used to verify that there was no error and help with extraction
         (expr (concat "print(\"" prefix
                       "\" * normpath(joinpath(VERSION ≤ v\"0.7-\" ? JULIA_HOME : Sys.BINDIR, "
                       "Base.DATAROOTDIR, \"julia\", \"base\")))"))
         (switches " --history-file=no --startup-file=no -qe ")
         (maybe-basedir (shell-command-to-string
                         (concat executable-path switches (concat "'" expr "'")))))
    (when (string-prefix-p prefix maybe-basedir)
      (substring maybe-basedir (length prefix)))))

(defun julia-repl--complete-executable-record! (executable-record)
  "Complete EXECUTABLE-RECORD if necessary.

Queries and appends missing information if necessary.

Note: when cannot capture the base dir, it is set to NIL to
prevent further attempts."
  (unless (plist-member (cddr executable-record) :basedir)
    (let* ((executable-path (second executable-record))
           (basedir (julia-repl--capture-basedir executable-path)))
      (nconc executable-record `(:basedir ,basedir))
      (unless basedir
        (warn "could not capture basedir for Julia executable %s"
              executable-path)))))

(defun julia-repl--split-switches ()
  "Return a list of switches, to be passed on to ‘make-term’."
  (when julia-repl-switches
    (split-string julia-repl-switches)))

(defun julia-repl--start-inferior (inferior-buffer-name executable-path)
  "Start a Julia REPL inferior process.

Creates INFERIOR-BUFFER-NAME (‘make-term’ surrounds it with *s),
running EXECUTABLE-PATH.

Return the inferior buffer.  No setup is performed."
  (apply #'make-term inferior-buffer-name executable-path nil
         (julia-repl--split-switches)))

(defun julia-repl--setup-captures ()
  "Set up captured keys which are captured from ‘term’.

Note that this affects ‘term’ globally."
  (mapc (lambda (k)
          (define-key term-raw-map k (global-key-binding k)))
        julia-repl-captures))

(defun julia-repl--setup-compilation-mode (inferior-buffer basedir)
  "Setup compilation mode for the the current buffer in INFERIOR-BUFFER.

BASEDIR is used for resolving relative paths."
  (with-current-buffer inferior-buffer
    (setq-local compilation-error-regexp-alist-alist
                julia-repl--compilation-regexp-alist)
    (setq-local compilation-error-regexp-alist
                (mapcar #'car compilation-error-regexp-alist-alist))
    (when basedir
      (setq-local compilation-search-path (list basedir)))
    (compilation-shell-minor-mode 1)))

(defun julia-repl--run-hooks (inferior-buffer)
  "Run the hooks in ‘julia-repl-hook’ in INFERIOR-BUFFER."
  (with-current-buffer inferior-buffer
    (run-hooks 'julia-repl-hook)))

(defun julia-repl--setup-term (inferior-buffer)
  "Set up customizations for term mode in INFERIOR-BUFFER.

Note that not all effects are buffer local."
  (with-current-buffer inferior-buffer
      (term-char-mode)
      (term-set-escape-char ?\C-x)      ; useful for switching windows
      (setq-local term-prompt-regexp "^(julia|shell|help\\?|(\\d+\\|debug ))>")
      (setq-local term-suppress-hard-newline t)  ; reflow text
      (setq-local term-scroll-show-maximum-output t)
      ;; do I need this?
      (setq-local term-scroll-to-bottom-on-output t)
      ))

(defun julia-repl--setup (inferior-buffer basedir)
  "Setup a newly created INFERIOR-BUFFER.

BASEDIR is used for the base directory."
  (when julia-repl-compilation-mode
    (julia-repl--setup-compilation-mode inferior-buffer basedir))
  (julia-repl--setup-captures)
  (julia-repl--setup-term inferior-buffer)
  (julia-repl--run-hooks inferior-buffer))

(defun julia-repl--start-and-setup (executable-key suffix)
  "Start an setup a Julia REPL.

Buffer name and executable determined by EXECUTABLE-KEY and SUFFIX.

Return the buffer.  Buffer is not raised."
  (let ((executable-record (julia-repl--executable-record executable-key))
        (inferior-buffer-name (julia-repl--inferior-buffer-name executable-key suffix)))
    (julia-repl--complete-executable-record! executable-record)
    (let* ((executable-path (second executable-record))
           (basedir (plist-get (cddr executable-record) :basedir))
           (inferior-buffer (julia-repl--start-inferior inferior-buffer-name
                                                        executable-path)))
      (julia-repl--setup inferior-buffer basedir)
      (setf (buffer-local-value 'julia-repl--inferior-buffer-suffix inferior-buffer) suffix)
      inferior-buffer)))

(cl-defun julia-repl--executable-record (executable-key)
  "Return the executable record for EXECUTABLE-KEY.

It is looked up in ‘julia-repl-executable-records’. An error is
raised if not found."
  (let ((executable-record (assq executable-key julia-repl-executable-records)))
    (unless executable-record
      (error "Could not find %s in JULIA-REPL-EXECUTABLE-RECORDS"
             executable-key))
    executable-record))

(defun julia-repl--live-buffer ()
  "Return the inferior buffer if it has a running REPL, otherwise NIL."
  (if-let ((inferior-buffer
            (get-buffer (julia-repl--add-earmuffs
                         (julia-repl--inferior-buffer-name)))))
      (when (term-check-proc inferior-buffer)
        inferior-buffer)))


;; prompting for executable-key and suffix

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
         (suffix-buffer-alist (stable-sort suffix-buffer-alist
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
      (incf index))
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
                   (first arg)))))
    (setq julia-repl-inferior-buffer-name-suffix suffix)
    (message "julia-repl-inferior-buffer-name-suffix set to %s" suffix)))

(defun julia-repl-prompt-switches ()
  "Read and set the switches for the inferior process."
  (interactive)
  (let ((switches (read-string "switches for the julia process: ")))
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


;; high-level functions

(defun julia-repl-inferior-buffer ()
  "Return the Julia REPL inferior buffer, creating one if it does not exist."
  (if-let ((inferior-buffer (julia-repl--live-buffer)))
      inferior-buffer
    (julia-repl--start-and-setup (julia-repl--get-executable-key)
                                 julia-repl-inferior-buffer-name-suffix)))

;;;###autoload
(defun julia-repl ()
  "Raise the Julia REPL inferior buffer, creating one if it does not exist.

This is the standard entry point for using this package."
  (interactive)
  (switch-to-buffer-other-window (julia-repl-inferior-buffer)))


;; sending to the REPL

(defun julia-repl--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Julia REPL term buffer.

A closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to ‘current-prefix-arg’,
  3. otherwise no newline.

Unless NO-BRACKETED-PASTE, bracketed paste control sequences are used."
  (let ((inferior-buffer (julia-repl-inferior-buffer)))
    (display-buffer inferior-buffer)
    (with-current-buffer inferior-buffer
      (unless no-bracketed-paste        ; bracketed paste start
        (term-send-raw-string "\e[200~"))
      (term-send-raw-string (string-trim string))
      (when (eq no-newline 'prefix)
        (setq no-newline current-prefix-arg))
      (unless no-newline
        (term-send-raw-string "\^M"))
      (unless no-bracketed-paste        ; bracketed paste stop
        (term-send-raw-string "\e[201~")))))

(defun julia-repl-send-line ()
  "Send the current line to the Julia REPL term buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `julia-repl-send-region-or-line'."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t) 'prefix t)
  (forward-line))

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
        (forward-line)))))

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
      (if (y-or-n-p "Buffer modified, save? ")
          (save-buffer)
        (setq file nil)))
    (julia-repl--send-string
     (if file
         (concat "include(\"" file "\")")
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun julia-repl-doc ()
  "Documentation for symbol at point."
  (interactive)
  (julia-repl--send-string (concat "@doc " (thing-at-point 'symbol t))))

(defun julia-repl-cd ()
  "Change directory to the directory of the current buffer (if applicable)."
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (julia-repl--send-string (concat "cd(\"" (file-name-directory filename) "\")"))
    (warn "buffer not associated with a file")))

(defun julia-repl-activate-parent (arg)
  "Look for a project file in the parent directories, if found, activate the project.

When called with a prefix argument, activate the home project."
  (interactive "P")
  (if arg
      (progn
        (message "activating home project")
        (julia-repl--send-string "import Pkg; Pkg.activate()"))
    (cl-flet ((find-projectfile (filename)
                                (locate-dominating-file (buffer-file-name) filename)))
      (if-let ((projectfile (or (find-projectfile "Project.toml")
                                (find-projectfile "JuliaProject.toml"))))
          (progn
            (message "activating %s" projectfile)
            (julia-repl--send-string
             (concat "import Pkg; Pkg.activate(\""
                     (expand-file-name (file-name-directory projectfile)) "\")")))
        (message "could not find project file")))))

(defun julia-repl-set-julia-editor (editor)
  "Set the JULIA_EDITOR environment variable."
  (interactive)
  (julia-repl--send-string (format "ENV[\"JULIA_EDITOR\"] = \"%s\";" editor)))

(defun julia-repl-use-emacsclient ()
  "Use emacsclient as the JULIA_EDITOR."
  (interactive)
  (julia-repl--send-string "ENV[\"JULIA_EDITOR\"] = \"emacsclient\";"))

;;;###autoload
(define-minor-mode julia-repl-mode
  "Minor mode for interacting with a Julia REPL running inside a term."
  nil ">"
  `((,(kbd "C-c C-c")    . julia-repl-send-region-or-line)
    (,(kbd "C-c C-b")    . julia-repl-send-buffer)
    (,(kbd "C-c C-z")    . julia-repl)
    (,(kbd "<C-return>") . julia-repl-send-line)
    (,(kbd "C-c C-e")    . julia-repl-edit)
    (,(kbd "C-c C-d")    . julia-repl-doc)
    (,(kbd "C-c C-m")    . julia-repl-macroexpand)
    (,(kbd "C-c C-s")    . julia-repl-prompt-set-inferior-buffer-name-suffix)
    (,(kbd "C-c C-v")    . julia-repl-prompt-set-executable-key)
    (,(kbd "C-c C-p")    . julia-repl-cd)
    (,(kbd "C-c C-a")    . julia-repl-activate-parent))
  (when-let ((filename (buffer-file-name)))
    (setq-local default-directory (file-name-directory filename))))

(provide 'julia-repl)
;;; julia-repl.el ends here
