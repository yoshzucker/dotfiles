# --- 40-tool-alias.sh ----------------------------------------------------
# Interactive aliases and small helper functions (ls, emacsc, gitroot, rgp, ...).
# zsh-only.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

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
