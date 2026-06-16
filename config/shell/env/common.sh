# --- common.sh ---------------------------------------------------------------
# Shared env: timezone and user-local PATH entries.
# Applies on macOS and MSYS2. Safe for bash and zsh.
# Exports: TZ
# Modifies: PATH

export TZ='Asia/Tokyo'

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.venv/bin"  ] && PATH="$HOME/.venv/bin:$PATH"
[ -d "$HOME/.grok/bin"  ] && PATH="$HOME/.grok/bin:$PATH"
export PATH

# --- end of common.sh --------------------------------------------------------
