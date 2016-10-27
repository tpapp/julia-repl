# julia-repl: run an inferior Julia REPL in Emacs

This is a minor mode for interacting with a Julia REPL running inside Emacs. The `julia` process is started in an ANSI terminal (`term`), which allows text formatting and colors, and interaction with the help system and the debugger.

It is recommended that you use this minor mode with [julia-mode](https://github.com/JuliaEditorSupport/julia-emacs).

## Loading

Place this in your Emacs initialization files (eg `.emacs`):
```lisp
(add-to-list 'load-path path-to-julia-repl)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
```

## Usage

The default keys are as follows.

key | action
----|-------
`C-c C-c` | send region (when applicable) or line to REPL
`C-c C-b` | send whole buffer to REPL
`C-c C-z` | raise the REPL or create a new one
`C-RET` | send line to REPL
`C-c C-e` | invoke `@edit` on region (when applicable) or line
`C-c C-d` | invoke `@doc` on symbol

Also, note some keybindings for `term`:
1. `C-x C-j` switches to *line mode*, where you can kill/yank, move around the buffer, use standard Emacs keybindings,
2. `C-c C-j` switches back to *char mode*,
3. for scrolling, use `S-<prior>` and `S-<next>`.
See the help of `term` for more.

## Limitations

- There should be a way to send function definitions without selecting them (ie select around point).
- Current implementation redefines `term-mode-map` globally to make arrow keys work.

## Comparison to ESS

A well-known alternative is [ESS](https://ess.r-project.org/), which also supports Julia. `julia-repl` was written because I could not use [Gallium](https://github.com/Keno/Gallium.jl) from ESS, which is based on `comint`, and thus does not allow a fully functioning terminal. Also, relying on the interactive features of the Julia REPL implies that I would not need to change this library to incorporate extensions and changes.
