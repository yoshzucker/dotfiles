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
[ -d ~/.grok/completions/zsh ] && fpath=(~/.grok/completions/zsh $fpath)
autoload -Uz compinit
compinit -C -d ~/.zcompdump-$HOST # rebuild -> compinit -u -d ~/.zcompdump-$HOST

# ----- completion -----
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ----- prompt -----
# Pure-style two-line prompt. Color roles follow gensho-theme mono ramp:
#   MONO7 = primary content (path)        — like default text
#   MONO6 = prominent secondary (❯)       — same step as minibuffer-prompt
#   MONO5 = secondary low-weight (git, host) — same step as comments
# user@host is shown only on SSH / root, freeing the input line in local shells.
setopt prompt_subst

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats $' %b'
zstyle ':vcs_info:git:*' actionformats $' %b|%a'

_prompt_context() {
  if [[ -n "$SSH_CONNECTION" || -n "$SSH_TTY" ]]; then
    print -n "%F{${THEME_MONO5}}%n@%m%f"
  elif (( EUID == 0 )); then
    print -n "%F{${THEME_MONO5}}%n%f"
  fi
}

precmd() { vcs_info }

PROMPT=$'\n%F{'"${THEME_MONO7}"$'}%(4~|.../%2~|%~)%f'\
$' %F{'"${THEME_MONO5}"$'}${vcs_info_msg_0_}%f\n'\
$'%(?.%F{'"${THEME_MONO6}"$'}.%F{red})❯%f '

RPROMPT='$(_prompt_context)'

# ----- key -----
bindkey -e
bindkey '^h' backward-word
bindkey '^l' forward-word
bindkey '^f' backward-char
bindkey '^b' forward-char

# --- end of zsh.sh -------------------------------------------------------
