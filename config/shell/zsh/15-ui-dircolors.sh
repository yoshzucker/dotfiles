# --- 15-ui-dircolors.sh --------------------------------------------------
# Load dircolors(1) for the current THEME_NAME/THEME_VARIANT.
# Uses files from ~/.config/dircolors/.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

# Required (from 00-core-env + theme):
#   - THEME_NAME, THEME_VARIANT, TERM_TYPE

# Validate required environment variables
[ -z "$THEME_NAME" ] && return 0
[ -z "$THEME_VARIANT" ] && return 0
[ -z "$TERM_TYPE" ] && return 0

# Determine appropriate dircolors file based on theme
dcs_path=""
case "${THEME_NAME}_${THEME_VARIANT}" in
  nord_*)          dcs_path="$HOME/.config/dircolors/ansi16.nord.dark"       ;;
  solarized_light) dcs_path="$HOME/.config/dircolors/ansi16.solarized.light" ;;
  solarized_dark)  dcs_path="$HOME/.config/dircolors/ansi16.solarized.dark"  ;;
esac

# Apply dircolors if a valid dcs_path was determined
if [ -n "$dcs_path" ]; then
  eval "$(dircolors "$dcs_path")"
fi

# --- end of z11_ui_dircolors.sh ------------------------------------------
