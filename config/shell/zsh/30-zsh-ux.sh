# --- 30-zsh-ux.sh --------------------------------------------------------
# Zsh interactive UX: history, setopt, compinit, prompt (with vcs_info),
# basic keybinds. Requires THEME_* from UI modules.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

[ -n "$ZSH_VERSION" ] || return 0

# ----- history -----
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_no_store
setopt hist_reduce_blanks

# ----- setopt: user interaction behavior -----
setopt correct               # auto correct command typos
setopt nobeep                # disable beep on error
setopt auto_pushd            # cd dir1 -> dir2 -> pushd stack
setopt auto_cd               # typing a dir name = cd
setopt interactive_comments  # allow # comments in interactive shell

# ----- compinit -----
autoload -Uz compinit
compinit -C -d ~/.zcompdump-$HOST # rebuild -> compinit -u -d ~/.zcompdump-$HOST

# ----- completion -----
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ----- prompt -----
PROMPT="%{%F{${THEME_SECONDARY}}%}%n@%m %# %{%f%}"

RPROMPT_BASE="%{%F{${THEME_SECONDARY}}%}%(4~|.../%2~|%~)%{%f%}"
RPROMPT="$RPROMPT_BASE"

# ----- git vcs -----
autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats '%b'

precmd() {
  vcs_info
  if [[ -n "$vcs_info_msg_0_" ]]; then
    RPROMPT="%{%F{${THEME_PRIMARY}}%}(${vcs_info_msg_0_})%{%f%} $RPROMPT_BASE"
  else
    RPROMPT="$RPROMPT_BASE"
  fi
}

# ----- key -----
bindkey -e
bindkey '^h' backward-word
bindkey '^l' forward-word
bindkey '^f' backward-char
bindkey '^b' forward-char

# --- end of z30_zsh_ux.sh ------------------------------------------------
