;;; -*- lexical-binding: t; -*-
;;; julia-repl.el --- A minor mode for a Julia REPL based on term

;; Copyright (C) 2016  Tamas K. Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Keywords: languages

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-julia-repl-mode")
;; (require 'julia-repl-mode)

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
;; Run a julia REPL inside a terminal in Emacs. In contrast to ESS, use
;; the Julia REPL facilities for interactive features, such readline,
;; help, debugging.

;; Package-Requires: ((names "Lookup.Latest.Version") (emacs "25"))

;;; Code:

(defcustom julia-repl-buffer-name "julia"
  "Buffer name for the Julia REPL. Will be surrounded by *'s"
  :type 'string)

(defcustom julia-repl-executable "julia"
  "Path for Julia executable."
  :type 'string)

(defcustom  julia-repl-hook nil
  "Hook to run after starting a Julia REPL term buffer."
  :type 'hook)

(defun julia-repl--setup-keys ()
  "Set up keys for the REPL."
  ;; FIXME global redefinition bad style, check if multi-term is more flexible
  (cl-flet ((defraw (key string)
              (define-key term-raw-map key
                (lambda ()
                  (interactive)
                  (term-send-raw-string string))))
            (defcmd (key command)
              (define-key term-raw-map key command)))
    ;; remap for readline (works better)
    (defraw [up] "\e[A")
    (defraw [down] "\e[B")
    (defraw [right] "\e[C")
    (defraw [left] "\e[D")
    ;; passed through
    (defcmd (kbd "M-x") execute-extended-command)))

(defun julia-repl--start ()
  "Start a Julia REPL in a term buffer, return the buffer. Buffer
is not raised."
  (let ((buf (make-term julia-repl-buffer-name julia-repl-executable)))
    (with-current-buffer buf
      (term-char-mode)
      (term-set-escape-char ?\C-x)      ; useful for switching windows
      (setq-local term-prompt-regexp "^(julia|shell|help\\?|(\\d+\\|debug ))>")
      (run-hooks 'julia-repl-hook)
      (julia-repl--setup-keys))
    buf))

(defun julia-repl-buffer (&optional switch)
  "Return the Julia REPL term buffer, creating one if it does not exist."
  (aif (get-buffer (concat "*" julia-repl-buffer-name "*"))
      (if (term-check-proc it)
          it
        (julia-repl--start))
    (julia-repl--start)))

(defun julia-repl-raise ()
  "Raise the Julia REPL term buffer, creating one if it does not exist."
  (interactive)
  (switch-to-buffer-other-window (julia-repl-buffer)))

(defun julia-repl--send-string (string)
  "Send STRING to the Julia REPL term buffer."
  (let ((buffer (julia-repl-buffer)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (term-send-raw-string (s-trim string))
      (term-send-raw-string "\^M"))))

(defun julia-repl-send-line ()
  "Send the current line to the Julia REPL term buffer."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t))
  (forward-line))

(defun julia-repl-send-region-or-line (&optional prefix)
  "When there is an active region, send that to the Julia REPL
term buffer, otherwise the current line.

When PREFIX is given, it is prepended."
  (interactive)
  (if (use-region-p)
      (progn
        (julia-repl--send-string
         (concat prefix
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))))
        (deactivate-mark))
    (progn
      (julia-repl--send-string (concat prefix (thing-at-point 'line t)))
      (forward-line))))

(defun julia-repl-edit-region-or-line ()
  "Same as SEND-REGION-OR-LINE, but called with the prefix Base.@edit."
  (interactive)
  (julia-repl-send-region-or-line "Base.@edit "))

(defun julia-repl-send-buffer ()
  "Send the contents of the current buffer to the Julia REPL term
buffer."
  (interactive)
  (julia-repl--send-string
   (buffer-substring-no-properties (point-min) (point-max))))

(defun julia-repl-doc ()
  "Documentation for symbol at point."
  (interactive)
  (julia-repl--send-string (concat "Base.@doc " (thing-at-point 'symbol t))))

;;;###autoload
(define-minor-mode julia-repl-mode
  "Minor mode for interacting with a Julia REPL running inside a term."
  nil ">"
  `((,(kbd "C-c C-c")    . julia-repl-send-region-or-line)
    (,(kbd "C-c C-b")    . julia-repl-send-buffer)
    (,(kbd "C-c C-z")    . julia-repl-raise)
    (,(kbd "<C-return>") . julia-repl-send-line)
    (,(kbd "C-c C-e")    . julia-repl-edit-region-or-line)
    (,(kbd "C-c C-d")    . julia-repl-doc)
    ))

(provide 'julia-repl)
;;; julia-repl.el ends here
