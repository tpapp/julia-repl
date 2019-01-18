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

(defgroup julia-repl-term nil
  "A Julia REPL."
  :group 'julia-repl)

(defcustom julia-repl-term-hook nil
  "Hook to run after starting a Julia REPL term buffer."
  :type 'hook
  :group 'julia-repl-term)

(defcustom julia-repl-term-captures (list (kbd "M-x"))
  "List of key sequences that are passed through (the global binding is used).

Note that this affects all buffers using the ‘ansi-term’ map."
  :type '(repeat key-sequence)
  :group 'julia-repl-term)


;; sending to the REPL

(defun julia-repl-term--send-string (string &optional no-newline no-bracketed-paste)
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

(defun julia-repl-term-send-line ()
  "Send the current line to the Julia REPL term buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `julia-repl-send-region-or-line'."
  (interactive)
  (julia-repl--send-string (thing-at-point 'line t) 'prefix t)
  (forward-line))


(defun julia-repl-term--start-inferior (inferior-buffer-name executable-path)
  "Start a Julia REPL inferior process.

Creates INFERIOR-BUFFER-NAME (‘make-term’ surrounds it with *s),
running EXECUTABLE-PATH.

Return the inferior buffer.  No setup is performed."
  (message "name %s path %s" inferior-buffer-name executable-path)
  (apply #'make-term inferior-buffer-name executable-path nil
         (julia-repl--split-switches)))

(defun julia-repl-term--setup-captures ()
  "Set up captured keys which are captured from ‘term’.

Note that this affects ‘term’ globally."
  (mapc (lambda (k)
          (define-key term-raw-map k (global-key-binding k)))
        julia-repl-term-captures))

(defun julia-repl-term--setup-term (inferior-buffer)
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

(provide 'julia-repl-term)
