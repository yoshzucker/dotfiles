# --- zsh.sh ---------------------------------------------------------------
# Interactive zsh. Requires THEME_* from colors.sh.

[ -n "$ZSH_VERSION" ] || return 0

# ----- Shell Options -----
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_all_dups hist_no_store hist_reduce_blanks
setopt nobeep auto_pushd auto_cd interactive_comments prompt_subst
# `correct` intentionally omitted: SPROMPT collides with the two-line PROMPT
# below and the correction prompt ends up invisible.

# ----- Completion System -----
[ -d ~/.grok/completions/zsh ] && fpath=(~/.grok/completions/zsh $fpath)
zmodload zsh/complist
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=2  # 1 match → insert; 2+ → menu
autoload -Uz compinit && compinit -d ~/.zcompdump

# ma= must include fg explicitly: zsh applies it standalone, not layered over di=.
() {
  local dihex="${THEME_CYAN#\#}" bhex="${THEME_MONO1#\#}"
  local dr=$((16#${dihex[1,2]})) dg=$((16#${dihex[3,4]})) db=$((16#${dihex[5,6]}))
  local br=$((16#${bhex[1,2]})) bg=$((16#${bhex[3,4]})) bb=$((16#${bhex[5,6]}))
  zstyle ':completion:*' list-colors \
    "di=38;2;${dr};${dg};${db}" \
    "ma=38;2;${dr};${dg};${db};48;2;${br};${bg};${bb}"
}

# ----- Prompt & VCS Info -----
autoload -Uz vcs_info add-zsh-hook
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats ' %b'
zstyle ':vcs_info:git:*' actionformats ' %b|%a'
add-zsh-hook precmd vcs_info

# Two-line PROMPT: top=path + vcs_info, bottom=❯ colored by $?.
PROMPT=$'\n%F{'"${THEME_MONO7}"$'}%(4~|.../%2~|%~)%f'\
$' %F{'"${THEME_MONO5}"$'}${vcs_info_msg_0_}%f\n'\
$'%(?.%F{'"${THEME_MONO6}"$'}.%F{red})❯%f '

# RPROMPT: SSH shows user@host; root shows user; otherwise empty.
# All zsh prompt escapes -- no subshell fork.
if [[ -n $SSH_CONNECTION || -n $SSH_TTY ]]; then
  RPROMPT="%F{${THEME_MONO5}}%n@%m%f"
else
  RPROMPT="%(!.%F{${THEME_MONO5}}%n%f.)"
fi

# ----- Plugins -----
ZPLUGDIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=${THEME_MONO4},italic"
[[ -r $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
  source $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh

# ----- Keybindings -----
bindkey -e

# Terminal-specific Tab (Ctrl+I) aliases.
bindkey -s $'\e[105;5u' '\t'  # Ghostty: CSI u (codepoint i=105)
bindkey -s $'\e[9;5u'   '\t'  # kitty fallback
bindkey -s $'\e[1;5n'   '\t'  # mintty: XTermModifyOtherKeys=1

bindkey '^J' down-line-or-select

# menuselect keymap (active during completion menu): vim-style navigation.
bindkey -M menuselect '^J' down-history
bindkey -M menuselect '^K' up-history
# ^F accepts the current candidate (mirrors autosuggest-accept outside menuselect).
bindkey -M menuselect '^F' accept-line

# ----- zoxide -----
command -v zoxide >/dev/null 2>&1 && eval "$(zoxide init zsh)"

# --- end of zsh.sh -------------------------------------------------------
