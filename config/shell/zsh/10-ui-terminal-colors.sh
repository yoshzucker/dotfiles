# --- 10-ui-terminal-colors.sh --------------------------------------------
# Apply terminal theme colors via standard ANSI OSC sequences (4/10/11/12).
# Exports THEME_* semantic colors used by prompt, fzf, tmux.
# zsh-specific (uses ${(U)} and set_color).

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

# Required env variables (from 00-core-env):
#   - THEME_NAME, THEME_VARIANT

# Exit if required variables are missing
[ -z "$THEME_NAME" ] && return
[ -z "$THEME_VARIANT" ] && return

set_color() {
  local color_name="$1"
  local color_value="$2"

  local esc_prefix='\x1b]'
  local esc_suffix='\x07'

  typeset -A osc_map=(fg 10 bg 11 curbg 12)
  typeset -A ansi_map=(
    black 0 red 1 green 2 yellow 3 blue 4
    magenta 5 cyan 6 white 7
    br_black 8 br_red 9 br_green 10 br_yellow 11
    br_blue 12 br_magenta 13 br_cyan 14 br_white 15
  )

  local osc="${osc_map[$color_name]:-4}"
  local index="${ansi_map[$color_name]:-}"
  local color_sequence="${osc};${index:+${index};}#"

  printf "%b" "${esc_prefix}${color_sequence}${color_value}${esc_suffix}"

  local export_name="THEME_${(U)color_name}"
  export "$export_name"="#$color_value"
}

# Apply color schemes
case "${theme_name}_${theme_variant}" in
  gensho_dark)
    set_color "black"      "333435"
    set_color "red"        "d4647f"
    set_color "green"      "59965e"
    set_color "yellow"     "a2835a"
    set_color "blue"       "638cb4"
    set_color "magenta"    "cb63ae"
    set_color "cyan"       "5f9196"
    set_color "white"      "6a6d6d"
    set_color "br_black"   "404242"
    set_color "br_red"     "bb785a"
    set_color "br_green"   "5c5e5f"
    set_color "br_yellow"  "797c7d"
    set_color "br_blue"    "4e5050"
    set_color "br_magenta" "9a79c9"
    set_color "br_cyan"    "262828"
    set_color "br_white"   "888c8c"
    set_color "fg"         "888c8c"
    set_color "bg"         "262828"
    set_color "curbg"      "888c8c"
    ;;
  nord_*)
    set_color "black"      "3b4252" # nord1
    set_color "red"        "bf616a" # nord11
    set_color "green"      "a3be8c" # nord14
    set_color "yellow"     "ebcb8b" # nord13
    set_color "blue"       "81a1c1" # nord9
    set_color "magenta"    "b48ead" # nord15
    set_color "cyan"       "88c0d0" # nord8
    set_color "white"      "e5e9f0" # nord5
    set_color "br_black"   "4c566a" # nord3
    set_color "br_red"     "bf616a" # nord11
    set_color "br_green"   "a3be8c" # nord14
    set_color "br_yellow"  "ebcb8b" # nord13
    set_color "br_blue"    "81a1c1" # nord9
    set_color "br_magenta" "b48ead" # nord15
    set_color "br_cyan"    "8fbcbb" # nord7
    set_color "br_white"   "eceff4" # nord6
    set_color "fg"         "d8dee9" # nord4
    set_color "bg"         "2e3440" # nord0
    set_color "curbg"      "81a1c1" # nord9
    ;;
  solarized_light)
    set_color "black"      "073642" #base02
    set_color "red"        "dc322f" #red
    set_color "green"      "859900" #green
    set_color "yellow"     "b58900" #yellow
    set_color "blue"       "268bd2" #blue
    set_color "magenta"    "d33682" #magenta
    set_color "cyan"       "2aa198" #cyan
    set_color "white"      "eee8d5" #base2
    set_color "br_black"   "002b36" #base03
    set_color "br_red"     "cb4b16" #orange
    set_color "br_green"   "586e75" #base01
    set_color "br_yellow"  "657b83" #base00
    set_color "br_blue"    "839496" #base0
    set_color "br_magenta" "6c71c4" #violet
    set_color "br_cyan"    "93a1a1" #base1
    set_color "br_white"   "fdf6e3" #base3
    set_color "fg"         "586e75"
    set_color "bg"         "fdf6e3"
    set_color "curbg"      "268bd2"
    ;;
  solarized_dark)
    set_color "black"      "073642"
    set_color "red"        "dc322f"
    set_color "green"      "859900"
    set_color "yellow"     "b58900"
    set_color "blue"       "268bd2"
    set_color "magenta"    "d33682"
    set_color "cyan"       "2aa198"
    set_color "white"      "eee8d5"
    set_color "br_black"   "002b36"
    set_color "br_red"     "cb4b16"
    set_color "br_green"   "586e75"
    set_color "br_yellow"  "657b83"
    set_color "br_blue"    "839496"
    set_color "br_magenta" "6c71c4"
    set_color "br_cyan"    "93a1a1"
    set_color "br_white"   "fdf6e3"
    set_color "fg"         "eee8d5"
    set_color "bg"         "002b36"
    set_color "curbg"      "268bd2"
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
