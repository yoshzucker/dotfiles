# --- aliases.sh ----------------------------------------------------------
# Interactive aliases and small helper functions. Each block self-guards on
# tool presence so the file is portable across machines.

[ -n "$ZSH_VERSION" ] || return 0

# ----- shell-reset: flush derived state, reload fresh -----
# Removes the completion dump and the stamp that dates its last full audit
# (zsh.sh), then re-execs a fresh login shell, which rebuilds both.  Use after
# installing a tool whose completion should not wait for tomorrow's audit,
# or a theme/PATH change if anything looks stale.  Everything it removes is
# derived data -- nothing is lost.
shell-reset() {
  rm -f ~/.zcompdump ~/.zcompdump-stamp
  exec zsh -l
}

# ----- open (cross-platform file/URL opener) -----
# macOS has /usr/bin/open natively; provide a shim on MSYS2.
if [[ $OSTYPE == msys* || $OSTYPE == cygwin* ]]; then
  open() { start "$@" }
  compdef _files start   # start has no zsh completion spec; add file completion
fi
compdef _files open

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

# ----- viewers / monitors -----
if command -v bat >/dev/null 2>&1; then
  alias cat='bat --paging=never'
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

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

# ----- cheatsheet (`?` / `cheat`) -----
# Shows the in-repo dotfiles cheatsheet (always-available textual reference).
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

# ----- marp (slide live preview server + export) -----
# Both helpers register the whole themes dir (--theme-set, $HOME expands
# portably) and pick a theme by its `/* @theme NAME */` name, so they stay
# theme-agnostic: drop a new CSS into ~/.config/marp/themes and it's selectable.
# Theme *colors* live only in those CSS files; the default theme *names* are the
# only knob here, overridable per-invocation via env vars (zsh analog of the
# Emacs defcustoms).
#
# `marps [dir]`         live-reload server (default: cwd). Dark, editor-matched
#                       gensho-light by default; MARP_PREVIEW_THEME overrides.
# `marpx file [fmt] [theme]`  one-shot export. fmt=pdf|pptx|html (default pdf),
#                       theme defaults to MARP_EXPORT_THEME (white `custom`).
if command -v marp >/dev/null 2>&1; then
  marps() {
    marp --config-file "$HOME/.config/marp/marp.config.yml" \
         --theme-set "$HOME/.config/marp/themes" \
         --theme "${MARP_PREVIEW_THEME:-gensho-light}" \
         --server "${1:-.}"
  }
  marpx() {
    local file="$1" fmt="${2:-pdf}" theme="${3:-${MARP_EXPORT_THEME:-custom}}"
    if [[ -z "$file" ]]; then
      print -u2 "usage: marpx <file> [pdf|pptx|html] [theme]"
      return 2
    fi
    marp --config-file "$HOME/.config/marp/marp.config.yml" \
         --theme-set "$HOME/.config/marp/themes" \
         --theme "$theme" \
         "--$fmt" -- "$file"
  }
fi

# --- end of aliases.sh ---------------------------------------------------
