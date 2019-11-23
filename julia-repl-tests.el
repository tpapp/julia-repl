;;; julia-repl-tests.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tamas Papp
;; Author: Tamas Papp <tkpapp@gmail.com>

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
