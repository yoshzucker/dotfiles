# --- common.sh ---------------------------------------------------------------
# Shared env: timezone and user-local PATH entries.
# Applies on macOS and MSYS2. Safe for bash and zsh.
# Exports: TZ
# Modifies: PATH

export TZ='Asia/Tokyo'

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.venv/bin"  ] && PATH="$HOME/.venv/bin:$PATH"
[ -d "$HOME/.grok/bin"  ] && PATH="$HOME/.grok/bin:$PATH"
# Where `cargo install' puts what it builds.  Scoop's rustup keeps CARGO_HOME
# under its persist directory and puts that on the Windows PATH itself, so on
# MSYS2 this directory does not exist and the line does nothing.
[ -d "$HOME/.cargo/bin" ] && PATH="$HOME/.cargo/bin:$PATH"
export PATH

# Tool config locations (XDG-aware). Each tool reads these at startup.
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/config"
export EZA_CONFIG_DIR="$HOME/.config/eza"
export BAT_CONFIG_PATH="$HOME/.config/bat/config"

# --- end of common.sh --------------------------------------------------------
