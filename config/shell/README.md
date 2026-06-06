# config/shell — UL Modular Shell Environment

This directory contains the **modular, minimal, high-maintainability** shell configuration after the UL refactor (2026).

## Layout

```
config/shell/
├── loader.sh          # Option B: single source of truth for one-time sourcing guard
├── env/               # Early modules — sourced from ~/.zshenv (all zsh, even non-interactive)
│   ├── 00-core-env.sh # THEME_NAME/THEME_VARIANT exports (macOS + MSYS2)
│   ├── 10-brew-env.sh # brew shellenv + central BREW_PREFIX export (macOS)
│   ├── 15-path.sh     # ~/.local/bin, venv, coreutils gnubin
│   ├── 20-timezone.sh # TZ=Asia/Tokyo
│   └── 30-msys.sh     # pp/wp path converters, open() (MSYS2 only)
└── zsh/               # Interactive-only — sourced from ~/.zshrc
    ├── 10-ui-terminal-colors.sh
    ├── 15-ui-dircolors.sh
    ├── 20-ui-truecolor.sh
    ├── 30-zsh-ux.sh   # history, prompt, vcs_info, compinit, keys
    ├── 40-tool-alias.sh
    └── 45-tool-fzf.sh
```

## Loading Order & Semantics

- `~/.zshenv` → `shell/env/*.sh` (lexical 00-30)
- `~/.zprofile` → (thin, login only)
- `~/.zshrc`  → `shell/zsh/*.sh` (lexical 10-45)

This follows standard zsh startup layers and eliminates previous duplication.

## Guard (Option B — no copy-paste)

Every module begins with:

```sh
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0
```

See [loader.sh](loader.sh) for the implementation. Each module captures its own filename-derived name and passes it explicitly to `__load_guard`.

## Adding a New Module

1. Choose `env/` (early, cross-shell) or `zsh/` (interactive only).
2. Pick next available number in the sequence (e.g. 25-foo.sh).
3. Copy the header + guard block from an existing file.
4. Write the logic. Export uppercase vars for downstream (THEME_*, BREW_PREFIX).
5. Document `deps:` and `exports:` in the header comment.
6. Test with `exec zsh -l` and non-interactive `zsh -c 'echo $VAR'`.

## Brew Integration

See [../../pkg/brew/Brewfile](../../pkg/brew/Brewfile) — declarative single source of truth (moved out of config/ tree).
Run `./bootstrap update` or `brew bundle --file pkg/brew/Brewfile`.

## Relation to Future Chezmoi

The small, single-purpose files + declarative Brewfile make migration to chezmoi (next step) straightforward (each .sh can become a template, Brewfile can be managed via `brew` package in chezmoi).

## Legacy

Old `config/lib/pXX_*/zXX_*.sh` were moved here with cleaned names during the refactor.
`config/lib/` directory was removed.
