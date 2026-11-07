# --- zsh.sh ---------------------------------------------------------------
# Interactive zsh.

[ -n "$ZSH_VERSION" ] || return 0

# ----- Shell Options -----
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_all_dups hist_no_store hist_reduce_blanks
# A command typed with a leading space is not kept: the way to run one that
# carries something private on its command line.
setopt hist_ignore_space
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
autoload -Uz compinit
compinit -d ~/.zcompdump

# Colors by the terminal's slots, so the theme the terminal reads decides
# them.  ma= must include fg explicitly: zsh applies it standalone, not
# layered over di=.  Its background is the terminal's own (49).
zstyle ':completion:*' list-colors "di=36" "ma=36;49"

compdef _files open

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
PROMPT=$'\n%F{15}%(4~|.../%2~|%~)%f'\
$' %F{12}${git_prompt_msg}%f\n'\
$'%(?.%F{7}.%F{red})❯%f '

# RPROMPT: SSH shows user@host; root shows user; otherwise empty.
# All zsh prompt escapes -- no subshell fork.
if [[ -n $SSH_CONNECTION || -n $SSH_TTY ]]; then
  RPROMPT="%F{12}%n@%m%f"
else
  RPROMPT="%(!.%F{12}%n%f.)"
fi

# ----- Keybindings -----
bindkey -e

# Terminal-specific Tab (Ctrl+I) aliases.
bindkey -s $'\e[105;5u' '\t'  # Ghostty: CSI u (codepoint i=105)
bindkey -s $'\e[9;5u'   '\t'  # kitty fallback
bindkey -s $'\e[1;5n'   '\t'  # mintty: XTermModifyOtherKeys=1

# Delete, Home and End, as the terminals here send them: xterm's form from
# Ghostty and mintty, the VT220 form from tmux.  The Mac's /etc/zshrc bound
# these from terminfo, which names the application-mode sequences that are
# only sent after smkx -- so only tmux's ever matched.
bindkey '^[[3~' delete-char
bindkey '^[[H'  beginning-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[F'  end-of-line
bindkey '^[[4~' end-of-line

# menuselect keymap (active during completion menu): vim-style navigation.
bindkey -M menuselect '^J' down-history
bindkey -M menuselect '^K' up-history
# ^F accepts the current candidate.
bindkey -M menuselect '^F' accept-line

# ----- Directory jumping -----
# No zoxide.  Its hook starts a process on every cd -- a tenth of a second or
# more under MSYS2 -- and the Windows hook zoxide 0.10.0 generates recorded
# nothing at all.  `cd -<Tab>' (auto_pushd) remains.

# --- end of zsh.sh -------------------------------------------------------
