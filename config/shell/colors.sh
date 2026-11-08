# --- colors.sh -----------------------------------------------------------
# What a terminal program needs to know to draw in color: that the terminal
# takes 24-bit color.  Read by zsh and by bash alike.
#
# The colors themselves are not set by any shell.  Ghostty and mintty read
# theme files of their own (config/ghostty/themes, config/mintty/themes),
# written by `gen-theme-palette gensho', and the zsh prompt names the
# terminal's slots.

# Truecolor — terminal-wide, not theme-specific.
export COLORTERM=truecolor

# --- end of colors.sh ----------------------------------------------------
