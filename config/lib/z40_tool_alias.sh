# --- z40_tool_alias.sh ---------------------------------------------------
# Shell aliases (for interactive shell)

[ -n "$ZSH_VERSION" ] || return 0

alias ls='ls --color=auto'
alias la='ls -aFC'
alias ll='ls -l'
ff() { find . -name "*$1*" -print; }

alias emacsc='emacsclient -t -a '''
alias killemacs="emacsclient -e '(client-save-kill-emacs)'"
alias gitroot='cd $(git rev-parse --show-toplevel)'

alias R='R --no-save'
alias rgp="rg --pre-glob '*.{pdf,xl[tas][bxm],xl[wsrta],do[ct],do[ct][xm],p[po]t[xm],p[op]t,html,htm,xhtm,xhtml,epub,chm,od[stp]}' --pre rgpipe"

# --- end of z40_tool_alias.sh --------------------------------------------
