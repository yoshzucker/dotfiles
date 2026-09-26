# --- colors.sh -----------------------------------------------------------
# Theme identity, truecolor setup, terminal palette (OSC 4/10/11/12), THEME_MONO*.
# zsh-specific.
#
# Two audiences, so two tables (both in the generated palette-<theme>.sh):
#
#   the terminal itself  speaks ANSI and nothing else, so it is painted from
#                        _THEME_ANSI_* -- the sixteen slots in canonical order
#                        -- plus OSC 10/11/12 for foreground, background and
#                        cursor.
#   tmux, fzf, the       style themselves from the theme's ramp, so they read
#   zsh prompt           THEME_MONO0..7 / THEME_DIM0 / the hues, exported here
#                        from _THEME_SEM_*.
#
# The second set is named after the ramp, not after whichever ANSI slot a level
# happens to occupy: a level with no slot at all (the background, the dim
# levels) is still just a key, and moving the slot assignment changes only what
# gen-theme-palette writes.
#
# Regenerate the data with `gen-theme-palette gensho` after changing the
# theme's palette or its ANSI slot assignment.

[ -n "$ZSH_VERSION" ] || return 0

# Theme identity — kept here because only the interactive terminal layer consumes it.
export THEME_NAME="gensho"
export THEME_VARIANT="${THEME_VARIANT:-light}"      # dark | light

# Truecolor — terminal-wide, not theme-specific.
export COLORTERM=truecolor
# Ensure xterm-24bits terminfo entry exists (mintty's default xterm-256color
# lacks the Tc capability).
#
# Both layouts are tested because ncurses picks one per platform: a letter
# directory (x/) where filenames are cheap, a hashed one (78/, the hex of
# `x') on macOS.  Testing only the first meant the guard never matched here
# and `tic' ran on every interactive shell -- silently, since the errors
# went to /dev/null along with the successes.  A process is a fifth of a
# second on the Windows machine, paid before every prompt.
if [ ! -f "$HOME/.terminfo/x/xterm-24bits" ] && \
   [ ! -f "$HOME/.terminfo/78/xterm-24bits" ]; then
  tic -x -o "$HOME/.terminfo" "$HOME/dotfiles/config/terminfo/24bit.src"
fi
# xterm-24bits is just xterm-256color + Tc; only needed where Tc is missing
# (e.g. mintty default xterm-256color terminfo on Windows). Skip on terminals
# whose own terminfo already advertises Tc (Ghostty, *-direct, tmux-256color),
# otherwise we'd force emacs onto a less accurate entry on macOS.
case "$TERM" in
  xterm-ghostty|*-direct|tmux-256color|tmux-direct) ;;
  *) alias emacs='env TERM=xterm-24bits emacs' ;;
esac

[ "$THEME_NAME" = "gensho" ] || return 0

source "${${(%):-%x}:A:h}/palette-${THEME_NAME}.sh"

# The sixteen ANSI slots in canonical order -- a property of the terminal, not
# of the theme, so it stays here rather than in the generated table.
typeset -ga _THEME_ANSI_NAMES=(
  black red green yellow blue magenta cyan white
  br_black br_red br_green br_yellow br_blue br_magenta br_cyan br_white
)

# Paint one color. Numbered slots go through OSC 4, the three singular ones
# through OSC 10/11/12. Inside tmux the sequence is swallowed before it reaches
# the outer terminal (DCS passthrough would be needed), so it would be wasted
# bytes per shell start, while the Ghostty palette stays correctly set from the
# very first non-tmux zsh of the session.
_theme_osc() {
  local name="$1" value="$2"
  [[ -n $TMUX ]] && return 0

  typeset -A osc_map=(fg 10 bg 11 curbg 12)
  typeset -A ansi_map=(
    black 0 red 1 green 2 yellow 3 blue 4
    magenta 5 cyan 6 white 7
    br_black 8 br_red 9 br_green 10 br_yellow 11
    br_blue 12 br_magenta 13 br_cyan 14 br_white 15
  )

  local index="${ansi_map[$name]:-}"
  local osc="${osc_map[$name]:-}"
  [[ -n $index || -n $osc ]] || return 0
  printf "%b" "\x1b]${osc:-4};${index:+${index};}#${value}\x07"
}

_theme_apply_palette() {
  local -a ansi
  local -A sem
  if [[ "$THEME_VARIANT" == "light" ]]; then
    ansi=("${_THEME_ANSI_LIGHT[@]}")
    sem=("${(@kv)_THEME_SEM_LIGHT}")
  else
    ansi=("${_THEME_ANSI_DARK[@]}")
    sem=("${(@kv)_THEME_SEM_DARK}")
  fi

  local i
  for (( i = 1; i <= 16; i++ )); do
    _theme_osc "${_THEME_ANSI_NAMES[i]}" "${ansi[i]}"
  done

  # mono1 is the surface and mono7 the text, which is what the terminal's own
  # background and foreground mean; the cursor takes mono6.
  _theme_osc bg    "${sem[mono1]}"
  _theme_osc fg    "${sem[mono7]}"
  _theme_osc curbg "${sem[mono6]}"

  # dim1 is exported by nobody, so it is not exported here either; add it to
  # the loop if a consumer ever wants a second dim level.
  local key
  for key in mono0 mono1 mono2 mono3 mono4 mono5 mono6 mono7 dim0 \
             red orange yellow green cyan blue purple magenta; do
    export "THEME_${(U)key}"="#${sem[$key]}"
  done
}

_theme_apply_palette

# delta picks its light/dark profile via DELTA_FEATURES; the git config
# defines [delta "gensho-dark"] and [delta "gensho-light"].
export DELTA_FEATURES="gensho-${THEME_VARIANT}"

# --- end of colors.sh ----------------------------------------------------
