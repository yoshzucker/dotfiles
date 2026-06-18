# --- aliases.sh ----------------------------------------------------------
# Interactive aliases and small helper functions. Each block self-guards on
# tool presence so the file is portable across machines.

[ -n "$ZSH_VERSION" ] || return 0

# ----- eza (ls family) -----
# Icons + git status + relative time + age gradient for ll/lla.
# `lt` is a shallow 2-level tree that respects .gitignore.
if command -v eza >/dev/null 2>&1; then
  __EZA='eza --icons=auto --git --group-directories-first --time-style=relative'
  alias  ls="$__EZA"
  alias  la="$__EZA -a"
  alias  ll="$__EZA -l --color-scale=age --color-scale-mode=gradient"
  alias lla="$__EZA -la --color-scale=age --color-scale-mode=gradient"
  alias  lt="$__EZA --tree --level=2 --git-ignore"
  unset __EZA
else
  alias ls='ls --color=auto'
  alias la='ls -aFC'
  alias ll='ls -l'
fi
ff() { find . -name "*$1*" -print; }

# ----- viewers / monitors -----
if command -v bat >/dev/null 2>&1; then
  alias cat='bat --paging=never'
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi
command -v btm     >/dev/null 2>&1 && { alias htop='btm'; alias top='btm'; }
command -v dust    >/dev/null 2>&1 && alias du='dust'
command -v duf     >/dev/null 2>&1 && alias df='duf'
command -v procs   >/dev/null 2>&1 && alias ps='procs'
command -v lazygit >/dev/null 2>&1 && alias lg='lazygit'

# ----- yazi (TUI file manager) with cd follow -----
# `y` opens yazi; on exit, the shell cd's to yazi's final directory.
if command -v yazi >/dev/null 2>&1; then
  y() {
    local tmp cwd
    tmp="$(mktemp -t yazi-cwd.XXXXXX)" || return 1
    yazi "$@" --cwd-file="$tmp"
    cwd="$(command cat -- "$tmp" 2>/dev/null)"
    [ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
    rm -f -- "$tmp"
  }
fi

# ----- cheatsheet fallback (`?` / `cheat`) -----
# Shows the in-repo dotfiles cheatsheet. navi (Ctrl-G) is the richer UI;
# this is the always-available textual reference.
cheat() {
  local f="$HOME/.config/cheat/dotfiles.md"
  [ -f "$f" ] || { echo "no cheatsheet at $f" >&2; return 1; }
  if command -v bat >/dev/null 2>&1; then
    bat --style=plain --language=markdown "$f"
  else
    command cat "$f"
  fi
}
alias '?'='cheat'

# ----- emacs / git / misc -----
alias emacsc='emacsclient -t -a '''
alias killemacs="emacsclient -e '(client-save-kill-emacs)'"
alias gitroot='cd $(git rev-parse --show-toplevel)'

alias R='R --no-save'
alias rgp="rg --pre-glob '*.{pdf,xl[tas][bxm],xl[wsrta],do[ct],do[ct][xm],p[po]t[xm],p[op]t,html,htm,xhtm,xhtml,epub,chm,od[stp]}' --pre rgpipe"


# ----- shell-reset: flush derived state, reload fresh -----
# Clears the tool-init caches (~/.cache/zsh/init_*.zsh) and the tmux server,
# then re-execs a fresh login shell (which re-warms the tmux daemon). Use
# after a theme/PATH change or a tool upgrade if anything looks stale.
# Everything it removes is derived data and is regenerated -- nothing is lost.
shell-reset() {
  rm -f "${XDG_CACHE_HOME:-$HOME/.cache}"/zsh/init_*.zsh
  command -v tmux >/dev/null 2>&1 && tmux kill-server 2>/dev/null
  exec zsh -l
}

# --- end of aliases.sh ---------------------------------------------------
