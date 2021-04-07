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
    (string-match julia-repl--rx-at str)
    (equal (match-string 1 str) "Foo")
    (equal (match-string 2 str) "~/code/Foo/src/Foo.jl")
    (equal (match-string 3 str) "100")))
