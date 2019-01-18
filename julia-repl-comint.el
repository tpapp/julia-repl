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
;; Run a julia REPL inside a comint buffer in Emacs.

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'cl-lib)

(defgroup julia-repl-comint nil
  "A Julia REPL through comint."
  :group 'julia-repl)

(defcustom julia-repl-comint-hook nil
  "Hook to run after starting a Julia REPL comint buffer."
  :type 'hook
  :group 'julia-repl-comint)

(defcustom julia-repl-comint-use-prompt-faces nil
  "When non-nil, color the julia prompts according to their respective faces."
  :group 'julia-repl-comint)

(defgroup julia-repl-prompt-faces nil
  "Faces used for Julia prompts in julia-repl."
  :group 'julia-repl-comint
  :group 'faces)

(defface julia-repl-julia-prompt-face
  '((t (:foreground "green3":weight bold)))
  "Face for the julia> prompt in the REPL buffer."
  :group 'julia-repl-prompt-faces)

(defface julia-repl-help-prompt-face
  '((t (:foreground "gold" :weight bold)))
  "Face for the help?> prompt in the REPL buffer."
  :group 'julia-repl-prompt-faces)

(defface julia-repl-pkg-prompt-face
  '((t (:foreground "deepskyblue1" :weight bold)))
  "Face for the (env)pkg> prompt in the REPL buffer."
  :group 'julia-repl-prompt-faces)

(defface julia-repl-shell-prompt-face
  '((t (:foreground "firebrick" :weight bold)))
  "Face for the shell> prompt in the REPL buffer."
  :group 'julia-repl-prompt-faces)



;; sending to the REPL

(defun julia-repl-comint--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Julia REPL comint buffer.

A closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to ‘current-prefix-arg’,
  3. otherwise no newline.

NO-BRACKETED-PASTE is ignored for sending to a comint process."
  (let ((inferior-buffer (julia-repl-inferior-buffer)))
    (display-buffer inferior-buffer)
    (when (eq no-newline 'prefix)
        (setq no-newline current-prefix-arg))
    (with-current-buffer inferior-buffer
      (comint-send-string inferior-buffer
			  (concat string (unless no-newline "\n"))))))

(defun julia-repl-comint--result-as-string (command)
  (save-window-excursion
    (let ((inf-buf (julia-repl-inferior-buffer))
	  (buf (get-buffer-create "*julia-result*"))
	  (cmd (concat command))
	  result)
      (with-current-buffer buf (erase-buffer))
      (with-current-buffer inf-buf
	(comint-redirect-send-command-to-process cmd buf inf-buf nil)
	(while (not comint-redirect-completed)
	  (sleep-for 0 1)))
      (with-current-buffer buf
	(replace-regexp-in-string "\\(^\"\\|\"$\\)" "" (string-trim (buffer-string)))))))

(defun julia-repl-comint-send-line ()
  "Send the current line to the Julia REPL comint buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `julia-repl-send-region-or-line'."
  (interactive)
  (julia-repl-comint--send-string (thing-at-point 'line t) 'prefix t)
  (forward-line))

(defun julia-repl--comint-input-sender (proc string)
  (save-current-buffer
    (let* ((help-?-regexp "^ *\\(?:\\(?1: *?\\? *\\)\\(?2:.+\\)\\)")
	   (help-?-match (string-match help-?-regexp string))
	   (pkg-bracket-regexp "^ *\\(?:\\(?1: *?\\] *\\)\\(?2:.+\\)\\)")
	   (pkg-bracket-match (string-match pkg-bracket-regexp string))
	   (shell-sc-regexp "^ *\\(?:\\(?1: *?; *\\)\\(?2:.+\\)\\)")
	   (shell-sc-match (string-match shell-sc-regexp string)))
      (cond (help-?-match
	     (comint-simple-send proc (format "@doc %s" (match-string 2 string))))
	    (pkg-bracket-match
	     (comint-simple-send proc (format "using Pkg; pkg\"%s\"" (match-string 2 string))))
	    (shell-sc-match
	     (comint-simple-send proc (format "run(`%s`)" (match-string 2 string))))
	    (t
	     (comint-simple-send proc string))))))

(defun julia-repl-comint--start-inferior (inferior-buffer-name executable-path)
  (let ((buf (get-buffer-create (julia-repl--add-earmuffs inferior-buffer-name))))
    (with-current-buffer buf
      (apply #'make-comint-in-buffer inferior-buffer-name nil
	     (executable-find executable-path) nil (julia-repl--split-switches))
      (julia-repl-inferior-julia-mode)
      (setq comint-input-sender 'julia-repl--comint-input-sender)
      buf)))

(defun julia-repl-comint--setup-buffer ()
  (run-hooks 'julia-repl-comint-hook))


(defvar julia-repl-julia-prompt-regexp "^julia> "
  "Regexp for matching the julia prompt.")

(defun julia-repl-comint-r-square-bracket ()
  "Switch to the Pkg3 REPL interface at the Julia prompt or insert ']'."
  (interactive)
  (insert "]")
  (when (looking-back (concat julia-repl-julia-prompt-regexp "\\]"))
    (let ((prompt (julia-repl-comint--result-as-string "using Pkg; Pkg.REPLMode.promptf()"))
	  (bol (save-excursion (search-backward-regexp "^") (point)))
	  (pt (point))) 
      (when julia-repl-comint-use-prompt-faces
       (put-text-property 0 (1- (length prompt)) 'face 'julia-repl-pkg-prompt-face prompt))
      (put-text-property bol pt 'display prompt))))

(defun julia-repl-comint-question-mark ()
  "Switch to the help interface at the Julia prompt or insert '?'."
  (interactive)
  (insert "?")
  (when (looking-back (concat julia-repl-julia-prompt-regexp "\\?"))
    (let ((prompt "help?> ")
	  (bol (save-excursion (search-backward-regexp "^") (point))))
      (when julia-repl-comint-use-prompt-faces
       (put-text-property 0 (1- (length prompt)) 'face 'julia-repl-help-prompt-face prompt))
      (put-text-property bol (point) 'display prompt))))

(defun julia-repl-comint-semicolon ()
  "Switch to the shell interface at the Julia prompt or insert ';'."
  (interactive)
  (when (looking-back (concat julia-repl-julia-prompt-regexp "\\]") nil)
    (julia-repl-comint-backspace))
  (insert ";")
  (when (looking-back (concat julia-repl-julia-prompt-regexp ";"))
    (let ((prompt "shell> ")
	  (bol (save-excursion (search-backward-regexp "^") (point))))
      (when julia-repl-comint-use-prompt-faces
       (put-text-property 0 (1- (length prompt)) 'face 'julia-repl-shell-prompt-face prompt))
      (put-text-property bol (point) 'display prompt))))

(defun julia-repl-comint-backspace ()
  (interactive)
  (let ((bol (save-excursion (search-backward-regexp "^") (point))))
    (cond
     ((looking-back julia-repl-julia-prompt-regexp)
      nil)
     ((looking-back (concat julia-repl-julia-prompt-regexp "."))
      (put-text-property bol (point) 'display nil)
      (delete-backward-char 1))
     (t
      (delete-backward-char 1)))))


(defun julia-repl-comint-maybe-send-input ()
  "Send the input to the julia process (or add a newline and indent if
it's an incomplete julia expression)."
  (interactive)
  (when completion-in-region-mode
    (completion-in-region-mode -1))
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "No Julia process is running!")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send
				   (if comint-use-prompt-regexp
				       (end-of-line)
				     (goto-char (field-end))))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
	     (jl-call (format "Meta.parse(\"\"\"%s \"\"\")" input))
	     (result (julia-repl-comint--result-as-string jl-call))
	     (is-incomplete-expression (or (string-match-p "Expr(:incomplete" result)
					 (string-match-p "Expr(:continue" result))))
	(if (or (= (length input) 0) (not is-incomplete-expression)
		(string-match-p "^[\];\?]" input))
	    (comint-send-input)
	  (insert "\n       "))))))

(defvar julia-repl-inferior-julia-mode-map
  (let ((keymap (copy-keymap comint-mode-map)))
    (define-key keymap (kbd "]")   #'julia-repl-comint-r-square-bracket)
    (define-key keymap (kbd "?")   #'julia-repl-comint-question-mark)
    (define-key keymap (kbd ";")   #'julia-repl-comint-semicolon)
    (define-key keymap (kbd "DEL") #'julia-repl-comint-backspace)
    (define-key keymap (kbd "RET") #'julia-repl-comint-maybe-send-input)
    keymap)
  "Key bindings for the Julia REPL running in the comint buffer.")


(define-derived-mode julia-repl-inferior-julia-mode comint-mode "julia-repl"
  "Major mode for interacting with the Julia REPL.

\\{julia-repl-inferior-julia-mode-map}"
  :group 'julia-repl-comint

  (when julia-repl-comint-use-prompt-faces
   (set-face-attribute 'comint-highlight-prompt nil :inherit 'julia-repl-julia-prompt-face))
  (font-lock-add-keywords nil julia-font-lock-keywords)

  (setq comint-prompt-regexp julia-repl-julia-prompt-regexp
	comint-prompt-read-only nil
	comint-use-prompt-regexp nil))

(provide 'julia-repl-comint)
