# --- 20-ui-truecolor.sh --------------------------------------------------
# Force COLORTERM=truecolor, ensure 24-bit terminfo, set emacs TERM alias.
# References config/terminfo/24bit.src (source of truth).

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

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
