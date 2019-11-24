;;; install dependencies in a CI environment

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install-file "julia-repl.el")
