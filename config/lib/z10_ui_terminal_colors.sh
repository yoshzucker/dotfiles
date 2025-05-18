# --- z10_ui_terminal_colors.sh -------------------------------------------
# Set terminal ANSI colors based on theme_name and term_type.
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

# Required env variables:
#   - theme_name: 'nord' or 'solarized'
#   - theme_variant: 'light' or 'dark'
#   - term_type: 'ansi' or 'iterm'

# Exit if required variables are missing
[ -z "$THEME_NAME" ] && return
[ -z "$THEME_VARIANT" ] && return
[ -z "$TERM_TYPE" ] && return

set_color() {
  local term_type="$1"
  local color_name="$2"
  local color_value="$3"

  local esc_prefix='\x1b]'
  local esc_suffix='\x07'

  local color_sequence
  case "$term_type" in
    iterm)
      color_sequence="1337;SetColors=${color_name}="
      ;;
    ansi)
      local osc_colorparet=4
      typeset -A osc_map=(
        fg 10
        bg 11
        curbg 12
      )
      typeset -A ansi_map=(
        black 0 red 1 green 2 yellow 3 blue 4
        magenta 5 cyan 6 white 7
        br_black 8 br_red 9 br_green 10 br_yellow 11
        br_blue 12 br_magenta 13 br_cyan 14 br_white 15
      )

      local osc="${osc_map[$color_name]:-$osc_colorparet}"
      local index="${ansi_map[$color_name]:-}"
      color_sequence="${osc};${index:+${index};}#"
      ;;
    *)
      echo "❗ Unknown terminal type: $term_type" >&2
      return 1
      ;;
  esac

  printf "%b" "${esc_prefix}${color_sequence}${color_value}${esc_suffix}"

  # export for tmux/emacs/others
  local export_name="THEME_${(U)color_name}"
  export "$export_name"="#$color_value"
}

# Apply color schemes
case "${theme_name}_${theme_variant}" in
  nord_*)
    set_color "$TERM_TYPE" "black"      "3b4252" # nord1
    set_color "$TERM_TYPE" "red"        "bf616a" # nord11
    set_color "$TERM_TYPE" "green"      "a3be8c" # nord14
    set_color "$TERM_TYPE" "yellow"     "ebcb8b" # nord13
    set_color "$TERM_TYPE" "blue"       "81a1c1" # nord9
    set_color "$TERM_TYPE" "magenta"    "b48ead" # nord15
    set_color "$TERM_TYPE" "cyan"       "88c0d0" # nord8
    set_color "$TERM_TYPE" "white"      "e5e9f0" # nord5
    set_color "$TERM_TYPE" "br_black"   "4c566a" # nord3
    set_color "$TERM_TYPE" "br_red"     "bf616a" # nord11
    set_color "$TERM_TYPE" "br_green"   "a3be8c" # nord14
    set_color "$TERM_TYPE" "br_yellow"  "ebcb8b" # nord13
    set_color "$TERM_TYPE" "br_blue"    "81a1c1" # nord9
    set_color "$TERM_TYPE" "br_magenta" "b48ead" # nord15
    set_color "$TERM_TYPE" "br_cyan"    "8fbcbb" # nord7
    set_color "$TERM_TYPE" "br_white"   "eceff4" # nord6
    set_color "$TERM_TYPE" "fg"         "d8dee9" # nord4
    set_color "$TERM_TYPE" "bg"         "2e3440" # nord0
    set_color "$TERM_TYPE" "curbg"      "81a1c1" # nord9
    ;;
  solarized_light)
    set_color "$TERM_TYPE" "black"      "073642" #base02
    set_color "$TERM_TYPE" "red"        "dc322f" #red
    set_color "$TERM_TYPE" "green"      "859900" #green
    set_color "$TERM_TYPE" "yellow"     "b58900" #yellow
    set_color "$TERM_TYPE" "blue"       "268bd2" #blue
    set_color "$TERM_TYPE" "magenta"    "d33682" #magenta
    set_color "$TERM_TYPE" "cyan"       "2aa198" #cyan
    set_color "$TERM_TYPE" "white"      "eee8d5" #base2
    set_color "$TERM_TYPE" "br_black"   "002b36" #base03
    set_color "$TERM_TYPE" "br_red"     "cb4b16" #orange
    set_color "$TERM_TYPE" "br_green"   "586e75" #base01
    set_color "$TERM_TYPE" "br_yellow"  "657b83" #base00
    set_color "$TERM_TYPE" "br_blue"    "839496" #base0
    set_color "$TERM_TYPE" "br_magenta" "6c71c4" #violet
    set_color "$TERM_TYPE" "br_cyan"    "93a1a1" #base1
    set_color "$TERM_TYPE" "br_white"   "fdf6e3" #base3
    set_color "$TERM_TYPE" "fg"         "586e75"
    set_color "$TERM_TYPE" "bg"         "fdf6e3"
    set_color "$TERM_TYPE" "curbg"      "268bd2"
    ;;
  solarized_dark)
    set_color "$TERM_TYPE" "black"      "073642"
    set_color "$TERM_TYPE" "red"        "dc322f"
    set_color "$TERM_TYPE" "green"      "859900"
    set_color "$TERM_TYPE" "yellow"     "b58900"
    set_color "$TERM_TYPE" "blue"       "268bd2"
    set_color "$TERM_TYPE" "magenta"    "d33682"
    set_color "$TERM_TYPE" "cyan"       "2aa198"
    set_color "$TERM_TYPE" "white"      "eee8d5"
    set_color "$TERM_TYPE" "br_black"   "002b36"
    set_color "$TERM_TYPE" "br_red"     "cb4b16"
    set_color "$TERM_TYPE" "br_green"   "586e75"
    set_color "$TERM_TYPE" "br_yellow"  "657b83"
    set_color "$TERM_TYPE" "br_blue"    "839496"
    set_color "$TERM_TYPE" "br_magenta" "6c71c4"
    set_color "$TERM_TYPE" "br_cyan"    "93a1a1"
    set_color "$TERM_TYPE" "br_white"   "fdf6e3"
    set_color "$TERM_TYPE" "fg"         "eee8d5"
    set_color "$TERM_TYPE" "bg"         "002b36"
    set_color "$TERM_TYPE" "curbg"      "268bd2"
    ;;
  *)
    return
    ;;
esac

# Semantic theme color exports for UI use
case "${theme_name}_${theme_variant}" in
  nord_*)
    export THEME_FG_DIM="$THEME_BR_BLACK"
    export THEME_BG_HIGHLIGHT="$THEME_BLACK"
    export THEME_PRIMARY="$THEME_BLUE"
    export THEME_SECONDARY="$THEME_MAGENTA"
    export THEME_ACCENT="$THEME_BR_YELLOW"
    ;;
  solarized_light)
    export THEME_FG_DIM="$THEME_BR_CYAN"
    export THEME_BG_HIGHLIGHT="$THEME_WHITE"
    export THEME_PRIMARY="$THEME_BLUE"
    export THEME_SECONDARY="$THEME_MAGENTA"
    export THEME_ACCENT="$THEME_YELLOW"
    ;;
  solarized_dark)
    export THEME_FG_DIM="$THEME_BR_YELLOW"
    export THEME_BG_HIGHLIGHT="$THEME_BR_GREEN"
    export THEME_PRIMARY="$THEME_BLUE"
    export THEME_SECONDARY="$THEME_MAGENTA"
    export THEME_ACCENT="$THEME_YELLOW"
    ;;
esac

# --- end of z10_ui_terminal_colors.sh ------------------------------------
