# --- z12_ui_truecolor.sh -------------------------------------------------
# Enable truecolor support and set up terminfo.

# Guard: source only, skip if already loaded
(return 0 2>/dev/null) || { echo "This script must be sourced."; exit 1; }

f="${BASH_SOURCE[0]:-${(%):-%N}}"
b="${f##*/}"
v="__LOADED_${b%.*}__"

if [ -n "${ZSH_VERSION:-}" ]; then
  eval "test -n \"\${$v:-}\""
  if [ $? -eq 0 ]; then
    return 0
  fi
else
  if [ -n "${!v:-}" ]; then
    return 0
  fi
fi

eval "$v=1"

# Set COLORTERM
export COLORTERM=truecolor

# Check if terminfo for xterm-24bits exists
if ! infocmp xterm-24bits >/dev/null 2>&1; then
  tic -x -o "$HOME/.terminfo" "$HOME/dotfiles/config/terminfo/24bit.src" || true
fi

# Set TERM for Emacs to use 24-bit color mode
# Emacs uses this to render precise colors in terminal UIs
alias emacs='env TERM=xterm-24bits emacs'

# --- end of z12_ui_truecolor.sh ------------------------------------------
