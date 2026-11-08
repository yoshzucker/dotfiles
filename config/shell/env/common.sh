# --- common.sh ---------------------------------------------------------------
# Shared env: timezone, color depth and user-local PATH entries.
# Applies to zsh on macOS and to Git Bash on Windows.
# Exports: TZ, COLORTERM, RIPGREP_CONFIG_PATH
# Modifies: PATH

export TZ='Asia/Tokyo'

# The terminals here -- Ghostty and mintty -- draw 24-bit color.  Their
# colors come from theme files of their own (config/ghostty/themes,
# config/mintty/themes), written by `gen-theme-palette gensho', and the zsh
# prompt names the terminal's slots; no shell sets a color.
export COLORTERM=truecolor

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
export PATH

# Tool config locations (XDG-aware). Each tool reads these at startup.
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/config"

# --- end of common.sh --------------------------------------------------------
