# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.5.2

### Added

- higlight "in expression starting at" locations

# 1.5.1

!!! NOTE
    The changelog was not updated between 1.3.0 and 1.5.1, sorry.

- protect against missing basedir

# 1.3.0

- allow vterm

- add a function and keybinding (`C-c C-l`) to list methods.

- make `C-c C-d` work with symbols that have a module path

# 1.2.0

- <kbd>C-c C-t</kbd> will include buffer with `Revise.includet` (@wraith1995)

- setting `julia-repl-save-buffer-on-send` to `t` will save always save buffers without asking before `include` and `includet` (@antoine-levitt)

- explain how to get more colors with [eterm-256color](https://github.com/dieggsy/eterm-256color) in the readme (@antoine-levitt)

- various minor fixes

# 1.1.0

- Add `julia-repl-set-julia-editor` and `julia-repl-use-emacsclient` to set the editor (@dellison)

- Fix bug in regex (@dellison)

- make `julia-repl-cd` change the directory associated with the Julia REPL inferior buffer (@dellison)

- fix implicit dependencies (thanks @pand5461 for reporting and checking)

# 1.0.2 and before

Sorry, no changelog available.
