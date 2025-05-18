# --- z11_ui_dircolors.sh -------------------------------------------------
# Set terminal ANSI colors and dircolors based on theme variables.
# This script is POSIX-compliant and can be sourced from zsh or bash.

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

# Required environment variables:
#   - theme_name:     'nord' or 'solarized'
#   - theme_variant:  'light' or 'dark'
#   - term_type:      'ansi' or 'iterm'

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
