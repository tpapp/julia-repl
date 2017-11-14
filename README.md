# julia-repl: run an inferior Julia REPL in Emacs

This is a minor mode for interacting with a Julia REPL running inside Emacs. The `julia` process is started in an ANSI terminal (`term`), which allows text formatting and colors, and interaction with the help system and the debugger.

It is recommended that you use this minor mode with [julia-mode](https://github.com/JuliaEditorSupport/julia-emacs).

<img src="./screenshot.png" width="70%" alt="screenshot">

## Installation and loading

**Please make sure you have at least Emacs 25**. The `term` code changed a bit since Emacs 24, and the package does not support it. For example, Ubuntu users can get the latest Emacs snapshot [here](https://launchpad.net/~ubuntu-elisp/+archive/ubuntu/ppa).

Place this in your **Emacs initialization files** (eg `.emacs`):
```emacs-lisp
(add-to-list 'load-path path-to-julia-repl)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
```
If you want to use a Julia executable other than `julia` in your path, set
```emacs-lisp
(julia-repl-set-executable "/path/to/julia")
```

## Usage

`M-x julia-repl`, or `C-c C-z` from a buffer in which the `julia-repl` minor mode is active starts a new inferior Julia process. The keys below can be used to interact with this process.

| key           | action                                             |
|---------------|----------------------------------------------------|
| `C-c C-c`     | send region (when applicable) or line to REPL      |
| `C-c C-b`     | send whole buffer to REPL (using include)          |
| `C-u C-c C-b` | send whole buffer to REPL (directly)               |
| `C-c C-z`     | raise the REPL or create a new one                 |
| `C-RET`       | send line to REPL (without bracketed paste)        |
| `C-c C-e`     | invoke `@edit` on region (when applicable) or line |
| `C-c C-d`     | invoke `@doc` on symbol                            |
| `C-c C-w`     | `workspace()`                                      |
| `C-c C-m`     | expand macro                                       |

All actions that send something to the REPL terminate with a **newline**, triggering evaluation. If you want to avoid sending a newline (eg maybe because you want to edit an expression), use prefix arguments (`C--` or `C-u`, currently both have the same effect). This of course does not apply to `C-c C-b`.

All commands send code using [bracketed paste](https://cirw.in/blog/bracketed-paste). When Julia is waiting for input, control characters like `^[[200~` may show up in your buffer, this is innocuous. If you input takes a long time to evaluate, you can step through it line-by-line with `C-RET`.

Also, note some keybindings for `term`:

1. `C-x C-j` switches to *line mode*, where you can kill/yank, move around the buffer, use standard Emacs keybindings,
2. `C-c C-k` switches back to *char mode*,
3. for scrolling, use `S-<prior>` and `S-<next>`.

See the help of `term` for more.

## Limitations

- See the [issues](https://github.com/tpapp/julia-repl/issues).

## Comparison to ESS

A well-known alternative is [ESS](https://ess.r-project.org/), which also supports Julia. `julia-repl` was written because I could not use [Gallium](https://github.com/Keno/Gallium.jl) from ESS, which is based on `comint`, and thus does not allow a fully functioning terminal. Also, relying on the interactive features of the Julia REPL implies that I would not need to change this library to incorporate extensions and changes.
