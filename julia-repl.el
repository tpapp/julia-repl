;;; julia-repl.el --- A minor mode for a Julia REPL -*- lexical-binding:t -*-

;; Copyright (C) 2016  Tamas K. Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: languages
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))

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

(defgroup julia-repl nil
  "A minor mode for a Julia REPL"
  :group 'julia)

(defcustom julia-repl-buffer-name "julia"
  "Buffer name for the Julia REPL.  Will be surrounded by *'s."
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

(defvar julia-compilation-regexp-alist
  '(;; matches "while loading /tmp/Foo.jl, in expression starting on line 2"
    (julia-load-error . ("while loading \\([^ ><()\t\n,'\";:]+\\), in expression starting on line \\([0-9]+\\)" 1 2))
    ;; matches "at /tmp/Foo.jl:2"
    (julia-runtime-error . ("at \\([^ ><()\t\n,'\";:]+\\):\\([0-9]+\\)" 1 2)))
  "Specifications for highlighting error locations (using compilation-")

(defvar julia-repl--executable "julia"
  "Path for Julia executable. Do not set this directly, use
JULIA-REPL-SET-EXECUTABLE.")

(cl-defun julia-repl--capture-basedir (&optional (executable julia-repl--executable))
  "Obtain the Julia base directory by querying the Julia  EXECUTABLE. When NIL,
an error was encountered."
  (let* ((prefix "OK")
         (expr (concat "\"print(\\\"" prefix
                       "\\\" * normpath(joinpath(JULIA_HOME, Base.DATAROOTDIR, "
                       "\\\"julia\\\", \\\"base\\\")))\""))
         (switches " --history-file=no --startup-file=no -qe ")
         (maybe-basedir (shell-command-to-string (concat executable switches expr))))
    (when (string-prefix-p prefix maybe-basedir)
      (substring maybe-basedir (length prefix)))))

(defvar julia-repl--basedir (julia-repl--capture-basedir)
  "Cached base directory for Julia executable.")

(defun julia-repl-set-executable (executable &optional basedir)
  "Set the julia executable and optionally the basedir. When the
  latter is not given, it is obtained by running the executable and saved."
  (setq julia-repl--executable executable)
  (setq julia-repl--basedir
        (if basedir
            basedir
          (julia-repl--capture-basedir))))

(defun julia-repl--start-inferior ()
  "Start a Julia REPL inferior process, return the buffer.
No setup is performed.  See JULIA-REPL-BUFFER-NAME,
JULIA-REPL-SET-EXECUTABLE."
  (let ((switches julia-repl-switches))
    (when current-prefix-arg
      (setq switches (split-string
                      (read-string "julia switches: " julia-repl-switches))))
    (apply #'make-term julia-repl-buffer-name julia-repl--executable nil switches)))

(defun julia-repl--start-and-setup ()
  "Start a Julia REPL in a term buffer, return the buffer.
Buffer is not raised."
  (let ((buf (julia-repl--start-inferior)))
    (with-current-buffer buf
      (term-char-mode)
      (term-set-escape-char ?\C-x)      ; useful for switching windows
      (when julia-repl-capture-Mx
        (define-key term-raw-map (kbd "M-x") #'execute-extended-command))
      (when julia-repl-compilation-mode
        (setq-local compilation-error-regexp-alist-alist
                    julia-compilation-regexp-alist)
        (setq-local compilation-error-regexp-alist
                    (mapcar #'car compilation-error-regexp-alist-alist))
        (setq-local compilation-search-path (list julia-repl--basedir))
        (compilation-shell-minor-mode 1))
      (setq-local term-prompt-regexp "^(julia|shell|help\\?|(\\d+\\|debug ))>")
      (run-hooks 'julia-repl-hook))
    buf))

(defun julia-repl-buffer ()
  "Return the Julia REPL term buffer, creating one if it does not exist."
  (if-let (buffer (get-buffer (concat "*" julia-repl-buffer-name "*")))
      (if (term-check-proc buffer)
          buffer
        (julia-repl--start-and-setup))
    (julia-repl--start-and-setup)))

;;;###autoload
(defun julia-repl ()
  "Raise the Julia REPL term buffer, creating one if it does not exist.
This should be the standard entry point."
  (interactive)
  (switch-to-buffer-other-window (julia-repl-buffer)))

(defun julia-repl--send-string (string &optional no-newline)
  "Send STRING to the Julia REPL term buffer. A closing newline
is sent according to NO-NEWLINE:
  1. NIL sends the newline,
  2. 'PREFIX sends it according to CURRENT-PREFIX-ARG,
  3. otherwise no newline."
  (let ((buffer (julia-repl-buffer)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (term-send-raw-string (string-trim string))
      (when (eq no-newline 'prefix)
        (setq no-newline current-prefix-arg))
      (unless no-newline
        (term-send-raw-string "\^M")))))

(defun julia-repl-send-line ()
  "Send the current line to the Julia REPL term buffer. Closed
with a newline, unless used with a prefix argument."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t) 'prefix)
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
  (let ((use-file (and (not arg) buffer-file-truename)))
    (when (and use-file (buffer-modified-p))
      (if (y-or-n-p "Buffer modified, save?")
          (save-buffer)
        (setq use-file nil)))
    (if use-file
        (julia-repl--send-string
         (concat "include(\"" buffer-file-truename "\")"))
      (julia-repl--send-string
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun julia-repl-doc ()
  "Documentation for symbol at point."
  (interactive)
  (julia-repl--send-string (concat "@doc " (thing-at-point 'symbol t))))

(defun julia-repl-workspace ()
  "Call workspace()."
  (interactive)
  (julia-repl--send-string "workspace()"))

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
    (,(kbd "C-c C-w")    . julia-repl-workspace)
    (,(kbd "C-c C-m")    . julia-repl-macroexpand)))

(provide 'julia-repl)
;;; julia-repl.el ends here
