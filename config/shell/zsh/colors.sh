# --- colors.sh -----------------------------------------------------------
# Theme identity, truecolor setup, terminal palette (OSC 4/10/11/12), THEME_MONO*.
# zsh-specific.

[ -n "$ZSH_VERSION" ] || return 0

# Theme identity — kept here because only the interactive terminal layer consumes it
export THEME_NAME="gensho"
export THEME_VARIANT="dark"

# Truecolor — terminal-wide, not theme-specific
export COLORTERM=truecolor
if ! infocmp xterm-24bits >/dev/null 2>&1; then
  tic -x -o "$HOME/.terminfo" "$HOME/dotfiles/config/terminfo/24bit.src" || true
fi
alias emacs='env TERM=xterm-24bits emacs'

[ "$THEME_NAME" = "gensho" ] || return 0

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

# gensho wet/dark palette — ordered per gensho-theme.el ordered-keys.
# Non-standard ANSI slot assignments (designed for TUI semantic coherence):
#   slot 11 (br_yellow)  → purple (keyword/control-flow)
#   slot 13 (br_magenta) → mono6  (cursor/prompt gray)
#   slot 14 (br_cyan)    → mono0  (background, fills the slot)
set_color "black"      "333435"  # mono1  — subtle bg layer / selection highlight
set_color "red"        "d4647f"  # red    — error / critical
set_color "green"      "59965e"  # green  — string / success
set_color "yellow"     "a2835a"  # yellow — warning
set_color "blue"       "638cb4"  # blue   — constant / link
set_color "magenta"    "cb63ae"  # magenta — function name
set_color "cyan"       "5f9196"  # cyan   — type / structure
set_color "white"      "6a6d6d"  # mono5  — secondary text / comment
set_color "br_black"   "404242"  # mono2  — chrome bg
set_color "br_red"     "bb785a"  # orange — interactive highlight
set_color "br_green"   "5c5e5f"  # mono4  — faint text / shadow
set_color "br_yellow"  "9a79c9"  # purple — keyword / control-flow (slot 11)
set_color "br_blue"    "4e5050"  # mono3  — active chrome
set_color "br_magenta" "797c7d"  # mono6  — cursor / prompt gray (slot 13)
set_color "br_cyan"    "262828"  # mono0  — background (slot 14)
set_color "br_white"   "888c8c"  # mono7  — foreground
set_color "fg"         "888c8c"  # mono7  — default foreground
set_color "bg"         "262828"  # mono0  — default background
set_color "curbg"      "797c7d"  # mono6  — cursor color

# THEME_MONO* — portable mono ramp for fzf, tmux, and zsh prompt.
# Each theme maps its neutral gray steps to these names; consumers use THEME_MONO*
# without knowing the underlying ANSI slot or theme name.
export THEME_MONO0="$THEME_BR_CYAN"     # #262828 — background
export THEME_MONO1="$THEME_BLACK"       # #333435 — subtle selection / highlight bg
export THEME_MONO2="$THEME_BR_BLACK"    # #404242 — chrome bg (rest)
export THEME_MONO3="$THEME_BR_BLUE"     # #4e5050 — chrome bg (active)
export THEME_MONO4="$THEME_BR_GREEN"    # #5c5e5f — faint text / shadow
export THEME_MONO5="$THEME_WHITE"       # #6a6d6d — secondary text / comment
export THEME_MONO6="$THEME_BR_MAGENTA"  # #797c7d — prominent secondary (cursor, prompt)
export THEME_MONO7="$THEME_BR_WHITE"    # #888c8c — primary foreground

# --- end of colors.sh ----------------------------------------------------
