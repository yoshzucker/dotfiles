# pkg/ — Package manifests and installation lists

This directory holds **declarative** lists of packages and tools.  
Everything here lives **outside** `config/` so it is never symlinked into `~/.config`.

## Contents

- `brew/Brewfile` — Homebrew (macOS + Linux) — used by `brew bundle`
- `apt/packages.txt` — APT packages for Debian/Ubuntu
- `pacman/packages.txt` + `pacman/msys2-packages.txt` — pacman (Arch, MSYS2)
- `scoop/scoopfile.json` — Scoop (Windows) — generated via `scoop export`

## Usage from the bootstrap script

Run `./bootstrap` (or `./bootstrap update`) or the platform-native commands directly:
- `brew bundle --file pkg/brew/Brewfile`
- `scoop import pkg/scoop/scoopfile.json` (Windows)
- `sudo apt install -y $(cat pkg/apt/packages.txt | grep -v '^#' | grep -v '^$')` etc.

## Regenerating

- Brew: `brew bundle dump --force --file pkg/brew/Brewfile`
- Scoop (on Windows): `scoop export | Out-File pkg/scoop/scoopfile.json -Encoding utf8`

All files should contain only package names (plus comments). No sensitive data.
