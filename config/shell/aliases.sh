# --- aliases.sh ----------------------------------------------------------
# Interactive aliases and small helper functions, for zsh on the Mac and
# Git Bash on Windows alike: nothing here is particular to either shell.
# Each block self-guards on tool presence so the file is portable across
# machines.  What only zsh can do lives in zsh/zsh.sh.

# ----- open -----
# macOS has `open'; Git Bash has `start', which hands a file or URL to
# whatever Windows opens it with.
case "$OSTYPE" in
  msys*|cygwin*) open() { start "$@"; } ;;
esac

# ----- emacs / git / misc -----
alias killemacs="emacsclient -e '(client-save-kill-emacs)'"
alias gitroot='cd "$(git rev-parse --show-toplevel)"'

# --- end of aliases.sh ---------------------------------------------------
