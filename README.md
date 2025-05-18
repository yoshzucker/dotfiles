# dotfiles

Personal configuration files for a consistent development environment.


---

## Entry Points

### `setup` (Unix-like systems)

```sh
./setup install      # Install essential packages
./setup configure    # Configure environment (link dotfiles, etc.)
./setup update       # Update installed packages
./setup list         # List managed components
./setup link         # Create symlinks from dotfiles to appropriate locations
```

This script detects the platform and installs or configures tools accordingly.

---

### `setup.ps1` (Windows)

PowerShell script for setting up the environment on Windows. It performs:

- Dotfiles linking (`emacs.d` → `.emacs.d`, etc.)
- Scoop installation
- Core apps installation via Scoop
- Configuration of tools like `wsltty`

To run:

```powershell
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
./setup.ps1
```

---

### `teardown` (Unix-like systems)

```sh
./teardown unlink    # Remove symlinks created by ./setup link
```

Cleans up symlinks under `$HOME`, `.config`, `.local`, and `.emacs.d`.

---

## Manual Setup

Some settings cannot be fully automated.

### iTerm2 (macOS)

- Preferences → Profiles → Keys  
- Add keybindings from `etc/iterm2/non-ansi-keys.itermkeymap`

### Karabiner-Elements (macOS)

- Complex Modifications → Add Rule → Enable the installed rules

### org-protocol (Windows)

- Use `org-protocol-native.reg` (for native Emacs)  
  or `org-protocol-wsl2.reg` (for WSL2 Emacs)
- Double-click to register
- Add bookmarklets from:
  - `org-protocol-bookmarklet.txt`
  - `org-roam-protocol-bookmarklet.txt`

---

## Notes (Windows)

To manually create a symbolic link (PowerShell):

```sh
mklink path_to path_from
```

For directories, use /D:

```
mklink /D path_to path_from
```

---

## License

MIT License.  
`terminfo/24bit.src` is derived from Stack Overflow (CC BY-SA 4.0).
