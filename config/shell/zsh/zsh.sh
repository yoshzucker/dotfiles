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

# What the Mac's /etc/zshrc provides, which ~/.zshenv skips: combining
# characters drawn as one cell (it runs `locale' to decide; every terminal
# here is UTF-8), and /usr/bin/log not shadowed by zsh's own `log'.
setopt combining_chars
disable log

# ----- Completion System -----
[ -d ~/.grok/completions/zsh ] && fpath=(~/.grok/completions/zsh $fpath)
zmodload zsh/complist
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=2  # 1 match → insert; 2+ → menu
# compinit's audit stats every file in every fpath directory, and the
# endpoint scanner on the Windows machine inspects them one at a time: half
# a second, before every prompt.  The audit earns its keep -- it is what
# notices a completion installed since yesterday -- but not once per shell.
# So it runs once a day and the rest of the day starts from the dump.
#
# The stamp is separate from the dump because compinit rewrites the dump
# only when something changed, which would leave a quiet day looking like a
# day the audit never ran.
#
# The `for' is how the age is read without starting anything: `[[ ]]' does
# not expand patterns, so the glob qualifier has to land somewhere that
# does.  It matches at most one file, or none when the stamp is old or
# absent.
autoload -Uz compinit
() {
  local fresh=0
  for _ in ~/.zcompdump-stamp(N.mh-24); do fresh=1; done
  if (( fresh )); then
    compinit -C -d ~/.zcompdump
  else
    compinit -d ~/.zcompdump
    print -n > ~/.zcompdump-stamp
  fi
}

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
autoload -Uz add-zsh-hook

# What `vcs_info' gave, without the processes it spent on giving it.  It runs
# git several times per prompt, and a git process costs a third of a second
# on the Windows machine -- paid again after every command, in every
# repository.  None of it is needed: the branch is the one line in
# .git/HEAD, and an interrupted operation is a file or directory beside it,
# so this reads them with zsh builtins and starts nothing at all.
#
# `%' is doubled because PROMPT runs under `prompt_subst' and a branch may
# legally contain one.
git_prompt_info() {
  git_prompt_msg=''
  local dir=$PWD gitdir head action
  while [[ -n $dir ]]; do
    if [[ -d $dir/.git ]]; then
      gitdir=$dir/.git
      break
    elif [[ -f $dir/.git ]]; then      # linked worktree or submodule
      read -r gitdir < $dir/.git
      gitdir=${gitdir#gitdir: }
      [[ $gitdir == /* ]] || gitdir=$dir/$gitdir
      break
    fi
    dir=${dir%/*}
  done
  [[ -n $gitdir && -r $gitdir/HEAD ]] || return
  read -r head < $gitdir/HEAD
  if [[ $head == ref:* ]]; then
    head=${head#ref: refs/heads/}
  else
    head=${head[1,7]}                  # detached: the short hash
  fi
  if   [[ -d $gitdir/rebase-merge || -d $gitdir/rebase-apply ]]; then action=rebase
  elif [[ -f $gitdir/MERGE_HEAD ]];        then action=merge
  elif [[ -f $gitdir/CHERRY_PICK_HEAD ]];  then action=cherry-pick
  elif [[ -f $gitdir/REVERT_HEAD ]];       then action=revert
  elif [[ -f $gitdir/BISECT_LOG ]];        then action=bisect
  fi
  git_prompt_msg=" ${head//\%/%%}${action:+|$action}"
}
add-zsh-hook precmd git_prompt_info

# Two-line PROMPT: top=path + vcs_info, bottom=❯ colored by $?.
PROMPT=$'\n%F{'"${THEME_MONO7}"$'}%(4~|.../%2~|%~)%f'\
$' %F{'"${THEME_MONO5}"$'}${git_prompt_msg}%f\n'\
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

# The plugin wraps every ZLE widget so that each one refreshes the
# suggestion, and by default it wraps them all again before every prompt --
# six hundred widgets, 5 ms on the Mac, after every command.  Once is
# enough: the wrapping still happens before the first prompt, after every
# startup file has defined its widgets, and only a widget defined later --
# by hand, at the prompt -- would go unwrapped.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1

[[ -r $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
  source $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh

# The plugin turns async on by itself for zsh 5.0.8 and newer, and async
# means a fork on every keystroke.  Windows has no fork: MSYS2 emulates it by
# copying the address space, and the endpoint scanner inspects each new
# process, which together put about half a second between a key and its
# character.  The Mac forks cheaply, and still more slowly than the search
# it hands off: 1000 history entries take 0.1 ms in-process, the fork 0.4.
#
# `unset', not `=0': the plugin tests whether the variable exists.
unset ZSH_AUTOSUGGEST_USE_ASYNC

# ----- Keybindings -----
bindkey -e

# Terminal-specific Tab (Ctrl+I) aliases.
bindkey -s $'\e[105;5u' '\t'  # Ghostty: CSI u (codepoint i=105)
bindkey -s $'\e[9;5u'   '\t'  # kitty fallback
bindkey -s $'\e[1;5n'   '\t'  # mintty: XTermModifyOtherKeys=1

bindkey '^J' down-line-or-select

# Delete, Home and End, as the terminals here send them: xterm's form from
# Ghostty and mintty, the VT220 form from tmux.  The Mac's /etc/zshrc bound
# these from terminfo, which names the application-mode sequences that are
# only sent after smkx -- so only tmux's ever matched -- and MSYS2 has no
# such file at all.
bindkey '^[[3~' delete-char
bindkey '^[[H'  beginning-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[F'  end-of-line
bindkey '^[[4~' end-of-line

# menuselect keymap (active during completion menu): vim-style navigation.
bindkey -M menuselect '^J' down-history
bindkey -M menuselect '^K' up-history
# ^F accepts the current candidate (mirrors autosuggest-accept outside menuselect).
bindkey -M menuselect '^F' accept-line

# ----- Directory jumping -----
# No zoxide.  Its hook starts a process on every cd -- a tenth of a second or
# more under MSYS2 -- and the Windows hook zoxide 0.10.0 generates recorded
# nothing at all.  `cd -<Tab>' (auto_pushd) and fzf's Alt-C remain.

# --- end of zsh.sh -------------------------------------------------------
