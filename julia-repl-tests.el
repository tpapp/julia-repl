;;; julia-repl-tests.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tamas Papp
;; Author: Tamas Papp <tkpapp@gmail.com>
;; Package-Requires: ((emacs "25.1")(s "1.12"))

(require 'cl-lib)
(require 'julia-repl)
(require 'ert)

(ert-deftest julia-repl-cygwin-rewrite-test ()
  (should (equal (julia-repl--path-rewrite "/home/PK/thread_buffers.jl"
                                           julia-repl-cygwin-path-rewrite-rules)
                 "c:/cygwin64/home/PK/thread_buffers.jl"))
  (should (equal (julia-repl--path-rewrite "/cygdrive/c/Users/PK/another.jl"
                                           julia-repl-cygwin-path-rewrite-rules)
                 "c:/Users/PK/another.jl")))

(cl-defmacro julia-repl--buffer (contents position &body body)
  "Make a temporary buffer with ‘contents’ and point at ‘position’, then run ‘body’."
  `(with-temp-buffer
     (julia-repl-mode)
     (insert ,contents)
     (goto-char ,position)
     ,@body))

(defun julia-repl--symbol-extraction (contents position)
  "Extract symbols in reverse order from a temporary buffer with
‘contents’ and point at ‘position’."
  (julia-repl--buffer contents position (julia-repl--symbols-at-point)))

(ert-deftest julia-repl-symbol-extraction-test ()
  (let ((symbols '("Foo" "bar" "baz")))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz" 13) symbols))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz " 12) symbols))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz " 14) nil))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz " 6) symbols))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz.( " 12) symbols))
    (should (equal (julia-repl--symbol-extraction "Foo.bar.baz.( " 6) symbols))))

(ert-deftest julia-repl-location-rx ()
  (let ((str "@ Foo ~/code/Foo/src/Foo.jl:100"))
    (should (string-match julia-repl--CR-at str))
    (should (equal (match-string 1 str) "Foo"))
    (should (equal (match-string 2 str) "~/code/Foo/src/Foo.jl"))
    (should (equal (match-string 3 str) "100"))))

(ert-deftest julia-repl-error-locations ()
  ;; module name, absolute path
  (should
   (equal
    (cdr (s-match julia-repl--CR-at "   @ Main.MyModule /tmp/tmp.jl:3"))
    '("Main.MyModule" "/tmp/tmp.jl" "3")))
  ;; tilde in path
  (should
   (equal
    (cdr (s-match julia-repl--CR-at "   @ Foo ~/tmp.jl:99"))
    '("Foo" "/tmp/tmp.jl" "99")))
  ;; underscore
  (should
   (equal
    (cdr (s-match julia-repl--CR-at "   @ Main.test_loops ~/tmp.jl:7"))
    '("Main.test_loops" "~/tmp.jl" "7")))
  ;; no module
  (should
   (equal
    (cdr (s-match julia-repl--CR-at "   @ ~/tmp.jl:7"))
    '(nil "~/tmp.jl" "7"))))
