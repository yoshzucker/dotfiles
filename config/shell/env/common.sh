# --- common.sh ---------------------------------------------------------------
# Shared env: timezone and user-local PATH entries.
# Applies to zsh on macOS and to Git Bash on Windows.
# Exports: TZ
# Modifies: PATH

export TZ='Asia/Tokyo'

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
# Last, behind the system's own: what an installer drops in here never
# stands in for a command of the same name.
[ -d "$HOME/.grok/bin"  ] && PATH="$PATH:$HOME/.grok/bin"
export PATH

# Tool config locations (XDG-aware). Each tool reads these at startup.
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/config"

# --- end of common.sh --------------------------------------------------------
