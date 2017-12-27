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
If you want to use a Julia executable other than `julia` in your path, see [below](#julia-executables).

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
| `C-c C-m`     | expand macro                                       |

All actions that send something to the REPL terminate with a **newline**, triggering evaluation. If you want to avoid sending a newline (eg maybe because you want to edit an expression), use prefix arguments (`C--` or `C-u`, currently both have the same effect). This of course does not apply to `C-c C-b`.

All commands send code using [bracketed paste](https://cirw.in/blog/bracketed-paste). When Julia is waiting for input, control characters like `^[[200~` may show up in your buffer, this is innocuous. If you input takes a long time to evaluate, you can step through it line-by-line with `C-RET`.

When called with a prefix (eg `C-u` or `C--`), `C-c C-z` (`julia-repl`) will prompt for the executable key and always create a new REPL. This is useful for temporary REPL's, eg trying out something without affecting an existing process. Users can also bind `julia-repl-prompt-new` for this functionality.

## Buffer-local inferior REPL and Julia executable

The minor mode allows the user to select a particular Julia executable and optionally a different inferior buffer for each source code buffer. This allows running two versions (eg stable and master) of Julia simultaneously, and/or running multiple inferior REPLs of the same Julia version.

### Julia executables

Set `julia-repl-executable-records` to a list of keys and executables. For example,
```elisp
(setq julia-repl-executable-records
      '((default "julia")
        (master (expanduser "~/src/julia-git/julia"))))
```
provides two executables. Make sure that there is one entry with the key of the global value of `julia-repl-executable-key`, which defaults to `'default`.

Use `C-c C-s` to select one of these (`julia-repl-prompt-executable`). You can also set the value of `julia-repl-executable-key` directly to a key in the `julia-repl-executable-records`, eg using [file variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html), but make sure you select a correct value.

The name of the inferior buffer will reflect your choice: the default is `*julia*` (indicator omitted), while the `master` executable would map to `*julia-master*`, and so on.

### Executable suffix

You can also set a *suffix* for the inferior buffer, if you want multiple ones in parallel. This can be a number, which will show up as `<number>`, or a symbol, which appears as `-symbol`.

It is recommended that you use `C-c C-s` (`julia-repl-prompt-inferior-buffer-name-suffix`), which prompts for a string by default. Prefix arguments modify it like this:

- numerical prefixes select that integer: eg `C-3 C-c C-s` set the suffix to `3`.

- the negative prefix picks the next unused integer: eg `C- C-c C-s` sets the suffix to `4` if `1`, `2`, `3` are in use.

### Switches

Switches to the `julia` process can be provided in the variable `julia-repl-switches`, which is also buffer-local. Eg

```elisp
(setq-default julia-repl-switches "-p 4")
```

### File local variables

If you are using the same settings for a specific file, consider using [file variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html). For example, if you use `add-file-local-variable` to create a block at the end of the Julia source file similar to

```julia
# Local Variables:
# julia-repl-executable-key: master
# julia-repl-inferior-buffer-name-suffix: tests
# julia-repl-switches: "-p 4"
# End:
```

then the next time you open a REPL, it will have the name `*julia-master-tests*`, and 4 worker processes.

## Interacting with `term`

Note some keybindings for `term`:

1. `C-x C-j` switches to *line mode*, where you can kill/yank, move around the buffer, use standard Emacs keybindings,
2. `C-c C-k` switches back to *char mode*,
3. for scrolling, use `S-<prior>` and `S-<next>`.

See the help of `term` for more.

## Limitations

See the [issues](https://github.com/tpapp/julia-repl/issues).

## Comparison to ESS

A well-known alternative is [ESS](https://ess.r-project.org/), which also supports Julia. `julia-repl` was written because I could not use [Gallium](https://github.com/Keno/Gallium.jl) from ESS, which is based on `comint`, and thus does not allow a fully functioning terminal. Also, relying on the interactive features of the Julia REPL implies that I would not need to change this library to incorporate extensions and changes.
