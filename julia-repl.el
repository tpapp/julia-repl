;;; julia-repl.el --- A minor mode for a Julia REPL -*- lexical-binding:t -*-

;; Copyright (C) 2016  Tamas K. Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: languages
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (s "1.10"))

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
(require 's)
(require 'subr-x)


;; customizations

(defgroup julia-repl nil
  "A minor mode for a Julia REPL"
  :group 'julia)

(defcustom julia-repl-default-buffer-name "*julia*"
  "Default buffer name for the Julia REPL. Should start and end
with *'s, these are appended when necessary."
  :type 'string
  :group 'julia-repl)

(defcustom julia-repl-switches nil
  "Command line switches for the Julia executable."
  :type 'list
  :group 'julia-repl)

(defcustom julia-repl-hook nil
  "Hook to run after starting a Julia REPL term buffer."
  :type 'hook
  :group 'julia-repl)

(defcustom julia-repl-capture-Mx t
  "When non-nil, M-x is passed through.
Note that this affects all buffers using the ANSI-TERM map."
  :type 'boolean
  :group 'julia-repl)

(defcustom julia-repl-compilation-mode t
  "When non-nil, highlight error locations using COMPILATION-SHELL-MINOR-MODE."
  :type 'boolean
  :group 'julia-repl)


;; global variables

(defvar julia-compilation-regexp-alist
  '(;; matches "while loading /tmp/Foo.jl, in expression starting on line 2"
    (julia-load-error . ("while loading \\([^ ><()\t\n,'\";:]+\\), in expression starting on line \\([0-9]+\\)" 1 2))
    ;; matches "at /tmp/Foo.jl:2"
    (julia-runtime-error . ("at \\([^ ><()\t\n,'\";:]+\\):\\([0-9]+\\)" 1 2)))
  "Specifications for highlighting error locations (using compilation-")

(defvar julia-repl-executable-records
  '((default "julia"))
  "List of Julia executables, of the form

  (KEY EXECUTABLE :BASEDIR BASEDIR)

A missing :BASEDIR will be completed automatically when first used.

Should contain the entry with the key DEFAULT")

(defvar-local julia-repl-buffer-name julia-repl-default-buffer-name
  "Name for the Julia REPL buffer associated with a source code buffer.")

(defvar-local julia-repl-executable 'default
  "Key for the executable associated with the buffer. Looked up
in JULIA-REPL-EXECUTABLE-RECORDS.")


;; REPL buffer creation and setup

(defun julia-repl--strip-earmuffs (string)
  "Strip *'s from the beginning and end of a string if present."
  (s-chop-prefix "*" (s-chop-suffix "*" string)))

(defun julia-repl--add-earmuffs (string)
  "Add earmuffs (*'s) to a string."
  (s-concat "*" string "*"))

(defun julia-repl--ensure-earmuffs (string)
  "Ensure that a string starts and ends with earmuffs (*'s)."
  (julia-repl--add-earmuffs (julia-repl--strip-earmuffs string)))

(cl-defun julia-repl--capture-basedir (executable)
  "Obtain the Julia base directory by querying the Julia EXECUTABLE. When NIL,
an error was encountered."
  (let* ((prefix "OK")
         (expr (concat "\"print(\\\"" prefix
                       "\\\" * normpath(joinpath(JULIA_HOME, Base.DATAROOTDIR, "
                       "\\\"julia\\\", \\\"base\\\")))\""))
         (switches " --history-file=no --startup-file=no -qe ")
         (maybe-basedir (shell-command-to-string
                         (concat executable switches expr))))
    (when (string-prefix-p prefix maybe-basedir)
      (substring maybe-basedir (length prefix)))))

(defun julia-repl--executable-record-complete! (executable-record)
  "Completed EXECUTABLE-RECORD by querying and appending missing
information if necessary."
  (unless (plist-member executable-record :basedir)
    (let* ((executable-path (second executable-record))
           (basedir (julia-repl--capture-basedir executable-path)))
      (if basedir
          (nconc executable-record `(:basedir ,basedir))
        (warn "could not capture basedir for Julia executable %s"
              executable-path))))
  executable-record)

(defun julia-repl--start-inferior (buffer-name executable-path)
  "Start a Julia REPL inferior process using EXECUTABLE in a buffer named
BUFER-NAME, return the buffer. No setup is performed."
  (let ((switches julia-repl-switches))
    (when current-prefix-arg
      (setq switches (split-string
                      (read-string "julia switches: " julia-repl-switches))))
    (apply #'make-term (julia-repl--strip-earmuffs buffer-name)
           executable-path nil switches)))

(defun julia-repl--setup (buffer basedir)
  "Setup a newly created REPL buffer. BASEDIR is used for the base directory."
  (with-current-buffer buffer
      (term-char-mode)
      (term-set-escape-char ?\C-x)      ; useful for switching windows
      (when julia-repl-capture-Mx
        (define-key term-raw-map (kbd "M-x") (global-key-binding (kbd "M-x"))))
      (when julia-repl-compilation-mode
        (setq-local compilation-error-regexp-alist-alist
                    julia-compilation-regexp-alist)
        (setq-local compilation-error-regexp-alist
                    (mapcar #'car compilation-error-regexp-alist-alist))
        (when basedir
          (setq-local compilation-search-path (list basedir)))
        (compilation-shell-minor-mode 1))
      (setq-local term-prompt-regexp "^(julia|shell|help\\?|(\\d+\\|debug ))>")
      (setq-local term-suppress-hard-newline t)  ; reflow text
      (make-local-variable 'julia-repl-inferior) ; marks buffers
      (run-hooks 'julia-repl-hook)))

(defun julia-repl--start-and-setup (buffer-name executable-record)
  "Using information from EXECUTABLE-RECORD, start a Julia REPL in a term buffer,
return the buffer. Buffer is not raised."
  (julia-repl--executable-record-complete! executable-record)
  (let* ((executable-path (second executable-record))
         (basedir (plist-get executable-record :basedir))
         (buffer (julia-repl--start-inferior buffer-name executable-path)))
    (julia-repl--setup buffer basedir)
    buffer))

(defun julia-repl--buffer-name ()
  "Return the Julia REPL buffer name for the current buffer."
  (julia-repl--ensure-earmuffs julia-repl-buffer-name))

(defun julia-repl--live-buffer ()
  "If there is a running REPL associated with the current buffer,
return its buffer, otherwise NIL."
  (if-let ((buffer (get-buffer (julia-repl--buffer-name))))
      (when (term-check-proc buffer)
        buffer)))

(defun julia-repl--executable-record ()
  "Return the executable record for the current JULIA-REPL-EXECUTABLE."
  (let ((executable-record (assq julia-repl-executable
                                 julia-repl-executable-records)))
    (unless executable-record
      (error "Could not find %s in JULIA-REPL-EXECUTABLE-RECORDS"
             julia-repl-executable))
    executable-record))

(defun julia-repl--start ()
  "Start a REPL and return the buffer. Uses JULIA-REPL-EXECUTABLE
and JULIA-REPL-BUFFER-NAME."
  (julia-repl--start-and-setup (julia-repl--buffer-name)
                               (julia-repl--executable-record)))

(defun julia-repl-buffer ()
  "Return the Julia REPL term buffer, creating one if it does not exist."
  (if-let ((buffer (julia-repl--live-buffer)))
      buffer
    (julia-repl--start)))

;;;###autoload
(defun julia-repl ()
  "Raise the Julia REPL term buffer, creating one if it does not exist.
This should be the standard entry point."
  (interactive)
  (switch-to-buffer-other-window (julia-repl-buffer)))

(defun julia-repl-prompt-buffer-name ()
  "Prompt for a Julia REPL inferior buffer name."
  (interactive)
  (let ((buffer-name (read-buffer "julia-repl buffer name: "
                                  (julia-repl--buffer-name)
                                  nil
                                  (lambda (arg)
                                    (let ((buffer-name (if (consp arg)
                                                           (car arg)
                                                         arg)))
                                      (local-variable-p 'julia-repl-inferior
                                                        (get-buffer
                                                         buffer-name)))))))
    (setq-local julia-repl-buffer-name
                (julia-repl--ensure-earmuffs buffer-name))))

(defun julia-repl-prompt-executable ()
  "Prompt for the Julia REPL executable. See JULIA-REPL-EXECUTABLE-RECORDS."
  (interactive)
  (setq-local julia-repl-executable
              (intern
               (completing-read "julia-repl executable: "
                                julia-repl-executable-records nil t nil))))


;; sending to the REPL

(defun julia-repl--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Julia REPL term buffer.

A closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to CURRENT-PREFIX-ARG,
  3. otherwise no newline.

Unless NO-BRACKED-PASTE, bracketed paste control sequences are used."
  (let ((buffer (julia-repl-buffer)))
    (display-buffer buffer)
    (with-current-buffer buffer
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
  "Send the current line to the Julia REPL term buffer. Closed
with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste. Unless you want this specifically, you should
probably be using `julia-repl-send-region-or-line'."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t) 'prefix t)
  (forward-line))

(defun julia-repl-send-region-or-line (&optional prefix suffix)
  "Send active region (if any) or current line to the Julia REPL term buffer;
closed with a newline, unless used with a prefix argument.

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
  "Same as SEND-REGION-OR-LINE, but called with the prefix @edit."
  (interactive)
  (julia-repl-send-region-or-line "@edit "))

(defun julia-repl-macroexpand ()
  "Same as SEND-REGION-OR-LINE, but wrapped in a macroexpand."
  (interactive)
  (julia-repl-send-region-or-line "macroexpand(quote " " end)"))

(defun julia-repl-send-buffer (arg)
  "Send the contents of the current buffer to the Julia REPL term
buffer. Use `include' by default if the buffer is associated with
a file, and is not modified (ie has been saved) or saved after
prompting. Otherwise send the contents directly; you can force
this with a prefix argument."
  (interactive "P")
  (let* ((file (and (not arg) buffer-file-name)))
    (when (and file (buffer-modified-p))
      (if (y-or-n-p "Buffer modified, save?")
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


;; keybindings

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
    (,(kbd "C-c C-s")    . julia-repl-prompt-buffer-name)
    (,(kbd "C-c C-v")    . julia-repl-prompt-executable)))

(provide 'julia-repl)
;;; julia-repl.el ends here
