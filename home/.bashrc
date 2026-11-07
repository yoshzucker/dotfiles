# --- ~/.bashrc (UL dotfiles) -----------------------------------------------
# bash, which is Git Bash on Windows: the shell there, and the one Emacs
# opens in a terminal (my-app-terminal.el).  zsh is the shell on the Mac.
#
# Git for Windows' /etc/profile has already put its own directories and the
# Windows PATH -- Scoop's shims among them -- on PATH, and set LANG from the
# Windows locale.  What is left is the part of the zsh setup that is not
# about zsh.

[ -f ~/.config/shell/env/common.sh ] && . ~/.config/shell/env/common.sh

# Whatever this machine needs and no other does; see ~/.zshenv.
[ -f ~/.config/shell/env/local.sh  ] && . ~/.config/shell/env/local.sh

case $- in
  *i*) ;;
  *) return ;;
esac

# A command typed with a leading space is not kept, as in zsh.
HISTCONTROL=ignoreboth

[ -f ~/.config/shell/colors.sh  ] && . ~/.config/shell/colors.sh
[ -f ~/.config/shell/aliases.sh ] && . ~/.config/shell/aliases.sh

# --- end ~/.bashrc ---------------------------------------------------------
