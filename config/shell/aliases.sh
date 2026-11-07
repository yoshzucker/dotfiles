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

# ----- cheatsheet (`?` / `cheat`) -----
# Shows the in-repo dotfiles cheatsheet (always-available textual reference).
cheat() {
  local f="$HOME/.config/cheat/dotfiles.md"
  [ -f "$f" ] || { echo "no cheatsheet at $f" >&2; return 1; }
  cat "$f"
}
alias '?'='cheat'

# ----- emacs / git / misc -----
alias emacsc='emacsclient -t -a '''
alias killemacs="emacsclient -e '(client-save-kill-emacs)'"
alias gitroot='cd "$(git rev-parse --show-toplevel)"'

alias R='R --no-save'

# --- end of aliases.sh ---------------------------------------------------
