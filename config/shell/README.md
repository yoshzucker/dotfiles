# config/shell — Shell Environment

Modular shell configuration for macOS (Ghostty) and Windows (MSYS2/Mintty).
All `env/` files are bash/zsh-safe and non-interactive-safe.

## Layout

```
config/shell/
├── env/           # Early env — sourced explicitly from ~/.zshenv
│   ├── common.sh  # TZ, ~/.local/bin + ~/.venv/bin in PATH
│   ├── macos.sh   # Homebrew init, BREW_PREFIX, gnubin PATH  (Darwin-only)
│   └── msys.sh    # pp/wp path converters, open()            (MSYS2-only)
└── zsh/           # Interactive-only — sourced explicitly from ~/.zshrc
    ├── colors.sh  # THEME_NAME/VARIANT, truecolor, OSC palette, THEME_MONO*
    ├── zsh.sh     # history, setopt, compinit, prompt, vcs_info, keybinds
    ├── aliases.sh # eza/ls, emacs client, git, tool aliases
    └── fzf.sh     # fzf keybindings, completions, FZF_DEFAULT_OPTS
```

## Loading Order & Semantics

Both layers use explicit sourcing (not glob) so load order is visible and
adding a file requires a deliberate edit.

**`~/.zshenv`** (every zsh invocation, non-interactive safe):
```sh
source ~/.config/shell/env/common.sh
source ~/.config/shell/env/macos.sh   # self-guards: no-op outside Darwin
source ~/.config/shell/env/msys.sh    # self-guards: no-op outside MSYS2
```

**`~/.zshrc`** (interactive only):
```sh
source ~/.config/shell/zsh/colors.sh   # must be first — sets THEME_MONO*
source ~/.config/shell/zsh/zsh.sh      # needs THEME_MONO* for prompt
source ~/.config/shell/zsh/aliases.sh
source ~/.config/shell/zsh/fzf.sh      # needs THEME_MONO* for colors
```

`colors.sh` must precede `zsh.sh` and `fzf.sh` because they reference
`THEME_MONO*` environment variables at source time.

## Platform Guards

Each module self-guards at the top rather than relying on a shared mechanism:

| Module | Guard |
|--------|-------|
| `env/macos.sh` | `[ "$(/usr/bin/uname -s)" = "Darwin" ] \|\| return 0` |
| `env/msys.sh`  | `[ -n "${MSYSTEM:-}" ] \|\| return 0` |
| `zsh/*.sh`     | `[ -n "$ZSH_VERSION" ] \|\| return 0` |
| `zsh/colors.sh`| additionally `[ "$THEME_NAME" = "gensho" ] \|\| return 0` |

## Adding a New env Module

1. Create `env/<name>.sh`.
2. Add a platform guard if needed.
3. Export uppercase vars (`BREW_PREFIX`, …).
4. Register it in `~/.zshenv` with an explicit `source` line.
5. Test: `exec zsh -l` (interactive) and `zsh -c 'echo $VAR'` (non-interactive).

## Adding a New zsh Module

1. Create `zsh/<name>.sh` with `[ -n "$ZSH_VERSION" ] || return 0` at the top.
2. If it consumes `THEME_MONO*`, it must be sourced after `colors.sh` in `.zshrc`.
3. Register it in `~/.zshrc` with an explicit `source` line.

## Brew Integration

See [../../pkg/brew/Brewfile](../../pkg/brew/Brewfile) — declarative single source of truth.
Run `brew bundle --file pkg/brew/Brewfile`.
