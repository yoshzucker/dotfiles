# dotfiles

Modular, maintainable personal configuration files.

## Quick Start

**Unix-like (macOS / Linux / WSL)**

```sh
# Fresh install or re-deploy after git pull
curl -fsSL https://raw.githubusercontent.com/yoshzucker/dotfiles/main/bootstrap | bash

# Or after cloning locally
cd dotfiles
./bootstrap
```

**Windows (PowerShell 5.0+)**

```powershell
# Fresh or after pull (clone or download bootstrap.ps1 + repo)
.\bootstrap.ps1
```

The default (no arguments) performs a full bootstrap: ensures package manager + packages from declarative manifests + creates/refreshes managed symlinks.

See `./bootstrap --help` or `.\bootstrap.ps1 -h` for all options.

## Recommended Commands

| Command                  | Description                                      | When to use                          |
|--------------------------|--------------------------------------------------|--------------------------------------|
| `./bootstrap` (or `bootstrap`) | Full bootstrap / re-deploy (packages + links)   | First time, after clone, or to ensure everything |
| `./bootstrap update`     | Update packages from manifests + refresh links + clean broken symlinks | After `git pull` to get latest package versions |
| `./bootstrap link`       | Refresh symlinks only (idempotent)               | Quick fix for links after manual changes |
| `./bootstrap unlink`     | Remove *only* symlinks created by this repo      | Before uninstall or major cleanup (real files untouched) |
| `./bootstrap doctor [--fix]` | Scan (and optionally delete) broken symlinks under $HOME, ~/.config, ~/.local, ~/.emacs.d | Diagnose or clean dangling links |
| `./bootstrap list independent` | List brew packages with no dependents         | Maintenance / cleanup                |
| `./bootstrap list dependent`   | Show dependents for each brew package         | Maintenance                          |

All operations are **non-interactive** and **safe to re-run** any number of times.

## Fresh Environment via curl (or equivalent)

- Unix: the one-liner above works from a minimal system (requires curl, git will be pulled in via packages on Linux).
- Windows: download the repo (or just bootstrap.ps1 + pkg/ and the directories you want), run `.\bootstrap.ps1`. On first Scoop install it will guide you to reopen the terminal and re-run.

After bootstrap completes, restart your shell (`exec $SHELL -l` on Unix, or new PowerShell on Windows) to pick up PATH and env changes.

## After Pulling Changes

```sh
git pull
./bootstrap update     # Unix
.\bootstrap.ps1 update # Windows
```

This updates packages (brew/Scoop/apt) to latest per manifests + refreshes all symlinks + removes any newly-broken ones.

## How Symlinks Work (Safety First)

- `home/` → symlinked directly under `$HOME`
- `config/` → symlinked under `~/.config` (XDG)
- `local/bin/` → individual scripts symlinked into real directory `~/.local/bin` (the directory itself is never a symlink)
- `emacs.d/` → symlinked under `~/.emacs.d`

**Conflict handling (XDG strict):**
When a real file (not a symlink) exists at a target path, it is moved to:
`${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles/backups/<timestamp>/<relative-path>`
Only real conflicting files are backed up. Existing symlinks (even if not ours) are replaced if they collide with our targets. No other data is ever touched.

**unlink / doctor safety:**
- `unlink` and pre-link cleanup only remove symlinks whose *target* (via realpath / resolved path) lies inside this repository directory.
- Real files, directories, and symlinks pointing elsewhere are left completely alone.
- `doctor --fix` removes broken (dangling) symlinks under the usual locations; this is intentionally broader for cleanup convenience.
- Recursive link scans skip generated/dependency directories (`myenv`, virtualenv names, `node_modules`, build/cache dirs) and never descend into symlink target trees.

This design guarantees that re-running after a `git pull` (or on a fresh machine) always converges to the exact same desired state without destroying user data.

## Package Management (pkg/)

All package lists live under `pkg/` (intentionally **outside** `config/`, so they are never symlinked into `~/.config` or `~/.local`).

- `pkg/brew/Brewfile` — Homebrew (macOS + Linux) — `brew bundle`
- `pkg/apt/packages.txt` — Base APT packages (Debian/Ubuntu/WSL)
- `pkg/pacman/{packages.txt,msys2-packages.txt}` — Arch / MSYS2
- `pkg/scoop/scoopfile.json` — Scoop (Windows) — `scoop import`

**Why this layout?**
- Single source of truth per platform.
- `config/` stays pure user configuration (XDG-clean).
- Easy to dump/regenerate: `brew bundle dump --force --file pkg/brew/Brewfile`, `scoop export | Out-File ...`

Run `./bootstrap update` (or the explicit package commands) to apply.

## Platform Notes

- **macOS**: Homebrew + emacs-plus (provides Emacs.app). Post-install step places Emacs.app in /Applications.
- **Linux (Debian/Ubuntu/WSL)**: APT base packages first, then Homebrew on top. `language-pack-ja` etc. for Japanese support.
- **Windows**: Scoop (extras bucket included). Symlink support requires **Developer Mode** enabled (Settings → Update & Security → For developers) or running PowerShell as Administrator. First run after Scoop install usually requires terminal restart.
- **Emacs**: The `emacs.d/` tree in the repo is linked under `~/.emacs.d` by bootstrap.

## Requirements

- Unix: bash, curl, git (git and curl are typically pulled in early on minimal systems via the bootstrap process on Linux).
- Windows: PowerShell 5.0+, internet access. Developer Mode recommended for symlinks.
- All: 64-bit system, reasonable free disk space.

## Regenerating Manifests

- Brew: `brew bundle dump --force --file pkg/brew/Brewfile`
- Scoop: `scoop export | Out-File pkg/scoop/scoopfile.json -Encoding utf8`
- APT / pacman: edit the .txt files manually (keep one package per line + comments).

## License

MIT License.

## Philosophy

Minimal, modular, long-term maintainable. XDG where it makes sense. Declarative packages. Idempotent everything. No surprises on re-run or fresh machines.
