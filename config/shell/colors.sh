# --- colors.sh -----------------------------------------------------------
# What a terminal program needs to know to draw in color: that the terminal
# takes 24-bit color, and a terminfo entry that says so where the terminal's
# own does not.  Read by zsh and by bash alike.
#
# The colors themselves are not set by any shell.  Ghostty and mintty read
# theme files of their own (config/ghostty/themes, config/mintty/themes), the
# zsh prompt names the terminal's slots, and tmux reads a theme file of its
# own (config/tmux), all written by `gen-theme-palette gensho'.

# Truecolor — terminal-wide, not theme-specific.
export COLORTERM=truecolor

# Ensure xterm-24bits terminfo entry exists (mintty's default xterm-256color
# lacks the Tc capability).
#
# Both layouts are tested because ncurses picks one per platform: a letter
# directory (x/) where filenames are cheap, a hashed one (78/, the hex of
# `x') on macOS.  Testing only the first meant the guard never matched here
# and `tic' ran on every interactive shell -- silently, since the errors
# went to /dev/null along with the successes.  Starting a process is a
# fifth of a second on the Windows machine, paid before every prompt.
# `command -v' is a builtin, so asking whether tic is there starts nothing.
if [ ! -f "$HOME/.terminfo/x/xterm-24bits" ] && \
   [ ! -f "$HOME/.terminfo/78/xterm-24bits" ] && command -v tic >/dev/null; then
  tic -x -o "$HOME/.terminfo" "$HOME/dotfiles/config/terminfo/24bit.src"
fi
# xterm-24bits is just xterm-256color + Tc; only needed where Tc is missing
# (e.g. mintty default xterm-256color terminfo on Windows). Skip on terminals
# whose own terminfo already advertises Tc (Ghostty, *-direct, tmux-256color),
# otherwise we'd force emacs onto a less accurate entry on macOS.
case "$TERM" in
  xterm-ghostty|*-direct|tmux-256color|tmux-direct) ;;
  *) alias emacs='TERM=xterm-24bits emacs' ;;
esac

# --- end of colors.sh ----------------------------------------------------
