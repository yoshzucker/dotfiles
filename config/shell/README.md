# config/shell — UL Modular Shell Environment

This directory contains the **modular, minimal, high-maintainability** shell configuration.
Targets: macOS (Ghostty) and Windows (MSYS2/Mintty). All `env/` files are safe for bash and zsh.

## Layout

```
config/shell/
├── loader.sh    # one-time sourcing guard (__load_guard) shared by all modules
├── env/         # Early env — sourced explicitly from ~/.zshenv (non-interactive safe)
│   ├── common.sh  # THEME_NAME, THEME_VARIANT, TZ, ~/.local/bin + ~/.venv/bin in PATH
│   ├── macos.sh   # Homebrew init, BREW_PREFIX, gnubin PATH  (Darwin-only)
│   └── msys.sh    # pp/wp path converters, open()            (MSYS2-only)
└── zsh/         # Interactive-only — sourced from ~/.zshrc
    ├── 10-ui-terminal-colors.sh
    ├── 15-ui-dircolors.sh
    ├── 20-ui-truecolor.sh
    ├── 30-zsh-ux.sh   # history, prompt, vcs_info, compinit, keys
    ├── 40-tool-alias.sh
    └── 45-tool-fzf.sh
```

## Loading Order & Semantics

`~/.zshenv` sources env modules **explicitly** (not via glob) so the order is visible and adding a new file requires a deliberate edit:

```sh
source ~/.config/shell/env/common.sh
source ~/.config/shell/env/macos.sh   # self-guards: no-op outside Darwin
source ~/.config/shell/env/msys.sh    # self-guards: no-op outside MSYS2
```

`~/.zshrc` sources `shell/zsh/*.sh` (lexical 10-45, interactive only).

## Guard (shared via loader.sh)

Every module begins with the same four-line block:

```sh
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c 'a-zA-Z0-9' '_')"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && . "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0
```

`__load_guard` sets a flag variable (`__LOADED_<name>__`) so sourcing the same file twice is a no-op. It handles both zsh (`${(P)var}`) and bash (`${!var}`) indirect expansion.

## Adding a New env Module

1. Create `env/<name>.sh` (no numeric prefix needed).
2. Paste the four-line guard block at the top.
3. Add a platform guard if needed (`[ "$(uname -s)" = "Darwin" ] || return 0`).
4. Export uppercase vars for downstream (`THEME_*`, `BREW_PREFIX`, …).
5. Register it in `~/.zshenv` with an explicit `source` line.
6. Test: `exec zsh -l` (interactive) and `zsh -c 'echo $VAR'` (non-interactive).

## Brew Integration

See [../../pkg/brew/Brewfile](../../pkg/brew/Brewfile) — declarative single source of truth.
Run `brew bundle --file pkg/brew/Brewfile`.
