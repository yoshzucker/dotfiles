# --- zsh.sh ---------------------------------------------------------------
# Zsh interactive UX: history, setopt, compinit, prompt (with vcs_info),
# basic keybinds. Requires THEME_MONO* from colors.sh.

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
# MONO6: prominent secondary (cursor/prompt/identifier level per gensho mono ramp)
# MONO5: secondary text (less prominent; path in rprompt stays quieter than prompt)
PROMPT="%{%F{${THEME_MONO6}}%}%n@%m %# %{%f%}"

RPROMPT_BASE="%{%F{${THEME_MONO5}}%}%(4~|.../%2~|%~)%{%f%}"
RPROMPT="$RPROMPT_BASE"

# ----- git vcs -----
autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats '%b'

precmd() {
  vcs_info
  if [[ -n "$vcs_info_msg_0_" ]]; then
    RPROMPT="%{%F{${THEME_MONO6}}%}(${vcs_info_msg_0_})%{%f%} $RPROMPT_BASE"
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

# --- end of zsh.sh -------------------------------------------------------
