# --- colors.sh -----------------------------------------------------------
# Theme identity, truecolor setup, terminal palette (OSC 4/10/11/12), THEME_MONO*.
# zsh-specific.
#
# ANSI 16-color slot strategy (Solarized convention; matches
# gensho--export-name-map in ~/Developer/gensho-theme/gensho-theme.el):
#
#   0  black        mono1
#   1-6 hues        red/green/yellow/blue/magenta/cyan
#   7  white        mono5
#   8  brightblack  mono0 = bg   (intentionally near-invisible for dim text)
#   9  brightred    orange
#   10 brightgreen  mono2
#   11 brightyellow mono3
#   12 brightblue   mono4
#   13 brightmagenta purple
#   14 brightcyan   mono6        (visible light grey — NEVER bg)
#   15 brightwhite  mono7
#
# Single source of truth: gensho-theme.el (`gensho-export-palette 'hex-list)`).
# The hex values below are harvested via `emacs --batch` and pasted in.
# Re-harvest after changing gensho-{wet,dry}-hsl.

[ -n "$ZSH_VERSION" ] || return 0

# Theme identity — kept here because only the interactive terminal layer consumes it.
export THEME_NAME="gensho"
export THEME_VARIANT="${THEME_VARIANT:-dark}"      # dark | light

# Truecolor — terminal-wide, not theme-specific.
export COLORTERM=truecolor
# Skip the costly `infocmp` fork (~0.25s/shell on Windows) by checking the
# compiled entry directly. ncurses stores it under a hashed (78/) or letter
# (x/) subdir depending on platform; the (N) nullglob qualifier matches either
# without a subprocess. Array assignment (not [[ ]]) so filename generation runs.
_ti24=($HOME/.terminfo/*/xterm-24bits(N))
(( $#_ti24 )) || tic -x -o "$HOME/.terminfo" "$HOME/dotfiles/config/terminfo/24bit.src" 2>/dev/null
unset _ti24
# xterm-24bits is just xterm-256color + Tc; only needed where Tc is missing
# (e.g. mintty default xterm-256color terminfo on Windows). Skip on terminals
# whose own terminfo already advertises Tc (Ghostty, *-direct, tmux-256color),
# otherwise we'd force emacs onto a less accurate entry on macOS.
case "$TERM" in
  xterm-ghostty|*-direct|tmux-256color|tmux-direct) ;;
  *) alias emacs='env TERM=xterm-24bits emacs' ;;
esac

[ "$THEME_NAME" = "gensho" ] || return 0

# Always export THEME_<NAME>; only emit OSC outside tmux. Inside tmux the OSC
# is swallowed before reaching the outer terminal (DCS passthrough would be
# needed), so it'd be wasted bytes per shell start while the Ghostty palette
# stays correctly set from the very first non-tmux zsh of the session.
set_color() {
  local color_name="$1"
  local color_value="$2"

  typeset -A osc_map=(fg 10 bg 11 curbg 12)
  typeset -A ansi_map=(
    black 0 red 1 green 2 yellow 3 blue 4
    magenta 5 cyan 6 white 7
    br_black 8 br_red 9 br_green 10 br_yellow 11
    br_blue 12 br_magenta 13 br_cyan 14 br_white 15
  )

  if [[ -z $TMUX ]]; then
    local esc_prefix='\x1b]'
    local esc_suffix='\x07'
    local osc="${osc_map[$color_name]:-4}"
    local index="${ansi_map[$color_name]:-}"
    local color_sequence="${osc};${index:+${index};}#"
    printf "%b" "${esc_prefix}${color_sequence}${color_value}${esc_suffix}"
  fi

  local export_name="THEME_${(U)color_name}"
  export "$export_name"="#$color_value"
}

# ----- palette tables (harvested via gensho-export-palette 'hex-list) -----
# Order: slots 0..15 (canonical ANSI). Update both arrays if HSL changes.
typeset -ga GENSHO_DARK_HEX=(
  333435  # 0  black        mono1
  d4647f  # 1  red
  59965e  # 2  green
  a2835a  # 3  yellow
  638cb4  # 4  blue
  cb63ae  # 5  magenta
  5f9196  # 6  cyan
  6a6d6d  # 7  white        mono5
  262828  # 8  brightblack  mono0 (bg)
  bb785a  # 9  brightred    orange
  404242  # 10 brightgreen  mono2
  4e5050  # 11 brightyellow mono3
  5c5e5f  # 12 brightblue   mono4
  9a79c9  # 13 brightmagenta purple
  797c7d  # 14 brightcyan   mono6
  888c8c  # 15 brightwhite  mono7
)
typeset -ga GENSHO_LIGHT_HEX=(
  494b4b  # 0  black        mono1
  d4647f  # 1  red
  59965e  # 2  green
  a2835a  # 3  yellow
  638cb4  # 4  blue
  cb63ae  # 5  magenta
  5f9196  # 6  cyan
  838687  # 7  white        mono5
  3c3d3e  # 8  brightblack  mono0 (bg)
  bb785a  # 9  brightred    orange
  57595a  # 10 brightgreen  mono2
  656868  # 11 brightyellow mono3
  747777  # 12 brightblue   mono4
  9a79c9  # 13 brightmagenta purple
  929697  # 14 brightcyan   mono6
  a2a6a7  # 15 brightwhite  mono7
)

# Slot 0..15 → set_color name (used to walk the active palette).
typeset -ga _GENSHO_SLOT_NAMES=(
  black red green yellow blue magenta cyan white
  br_black br_red br_green br_yellow br_blue br_magenta br_cyan br_white
)

_gensho_emit_palette() {
  local -a hex
  if [[ "$THEME_VARIANT" == "light" ]]; then
    hex=("${GENSHO_LIGHT_HEX[@]}")
  else
    hex=("${GENSHO_DARK_HEX[@]}")
  fi
  local i
  for (( i = 1; i <= 16; i++ )); do
    set_color "${_GENSHO_SLOT_NAMES[i]}" "${hex[i]}"
  done
  # OSC 10/11/12 — bg uses slot 8 hex (= mono0), fg uses slot 15 hex (= mono7),
  # cursor bg uses slot 14 hex (= mono6).
  set_color "fg"    "${hex[16]}"
  set_color "bg"    "${hex[9]}"
  set_color "curbg" "${hex[15]}"
}

_gensho_emit_palette

# THEME_MONO* — portable mono ramp for fzf, tmux, and zsh prompt.
# Consumers read THEME_MONO* without knowing the underlying ANSI slot.
# After Solarized re-shuffle:
#   mono0 = slot 8  (brightblack)  = bg
#   mono1 = slot 0  (black)
#   mono2 = slot 10 (brightgreen)
#   mono3 = slot 11 (brightyellow)
#   mono4 = slot 12 (brightblue)
#   mono5 = slot 7  (white)
#   mono6 = slot 14 (brightcyan)
#   mono7 = slot 15 (brightwhite)
export THEME_MONO0="$THEME_BR_BLACK"
export THEME_MONO1="$THEME_BLACK"
export THEME_MONO2="$THEME_BR_GREEN"
export THEME_MONO3="$THEME_BR_YELLOW"
export THEME_MONO4="$THEME_BR_BLUE"
export THEME_MONO5="$THEME_WHITE"
export THEME_MONO6="$THEME_BR_CYAN"
export THEME_MONO7="$THEME_BR_WHITE"

# delta picks its light/dark profile via DELTA_FEATURES; the git config
# defines [delta "gensho-dark"] and [delta "gensho-light"].
export DELTA_FEATURES="gensho-${THEME_VARIANT}"

# --- end of colors.sh ----------------------------------------------------
