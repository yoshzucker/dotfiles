# --- aliases.sh ----------------------------------------------------------
# Interactive aliases and small helper functions (eza/ls, emacsc, gitroot, ...).

[ -n "$ZSH_VERSION" ] || return 0

if command -v eza >/dev/null 2>&1; then
  alias ls='eza --group-directories-first'
  alias la='eza -a --group-directories-first'
  alias ll='eza -l --group-directories-first'
  alias lla='eza -la --group-directories-first'
  alias lt='eza --tree'
else
  alias ls='ls --color=auto'
  alias la='ls -aFC'
  alias ll='ls -l'
fi
ff() { find . -name "*$1*" -print; }

alias emacsc='emacsclient -t -a '''
alias killemacs="emacsclient -e '(client-save-kill-emacs)'"
alias gitroot='cd $(git rev-parse --show-toplevel)'

alias R='R --no-save'
alias rgp="rg --pre-glob '*.{pdf,xl[tas][bxm],xl[wsrta],do[ct],do[ct][xm],p[po]t[xm],p[op]t,html,htm,xhtm,xhtml,epub,chm,od[stp]}' --pre rgpipe"

# --- end of aliases.sh ---------------------------------------------------
