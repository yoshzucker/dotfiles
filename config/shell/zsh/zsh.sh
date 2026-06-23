# --- zsh.sh ---------------------------------------------------------------
# Zsh interactive shell. Requires THEME_* from colors.sh.

[ -n "$ZSH_VERSION" ] || return 0

# ----- Shell Options -----
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt hist_ignore_dups hist_ignore_all_dups hist_no_store hist_reduce_blanks
setopt correct nobeep auto_pushd auto_cd interactive_comments

# ----- Completion System -----
[ -d ~/.grok/completions/zsh ] && fpath=(~/.grok/completions/zsh $fpath)

# zsh-autocomplete must be sourced before compinit.
() {
  local zplugdir="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"
  [[ -r $zplugdir/zsh-autocomplete/zsh-autocomplete.plugin.zsh ]] || return 0
  zstyle ':autocomplete:*' fzf-completion no
  source $zplugdir/zsh-autocomplete/zsh-autocomplete.plugin.zsh

  # Patch: sysopen <(...) can fail (SIGWINCH, fd pressure) returning /dev/fd/-1.
  # Without a guard, zle -Fw is called with an empty fd, causing spurious errors.
  # Applies to both :wait (timer fd) and :start (completion fd) — same pattern.
  if (( ${+functions[.autocomplete:async:wait]} )); then
    .autocomplete:async:wait() {
      typeset -g _autocomplete_async_fd=
      sysopen -r -o cloexec -u _autocomplete_async_fd <(
        local -F seconds=
        builtin zstyle -s :autocomplete: delay seconds ||
            builtin zstyle -s :autocomplete: min-delay seconds ||
            (( seconds = 0.05 ))
        (( seconds = max( 0, seconds - _autocomplete__overhead ) ))
        local -i timeout=$(( 100 * seconds ))
        zselect -t $timeout
        print
      ) 2>/dev/null
      [[ -n $_autocomplete_async_fd ]] || return 0
      builtin zle -Fw "$_autocomplete_async_fd" .autocomplete:async:wait:fd-widget
      return 0
    }
  fi

  if (( ${+functions[.autocomplete:async:start]} )); then
    .autocomplete:async:start() {
      typeset -g _autocomplete_async_fd=
      sysopen -r -o cloexec -u _autocomplete_async_fd <(
        local +h PS4=$_autocomplete__ps4
        .autocomplete:async:start:inner 2>>| $_autocomplete__log
      ) 2>/dev/null
      [[ -n $_autocomplete_async_fd ]] || return 0
      builtin zle -Fw "$_autocomplete_async_fd" .autocomplete:async:complete:fd-widget
      command true
    }
  fi

  # Bug in zsh-autocomplete: wait:fd-widget and complete:fd-widget close the fd only
  # when [[ -t $fd ]] (i.e. the fd is a tty). sysopen <(...) produces a pipe, never
  # a tty, so the close never executes. Every keypress leaks 1-2 fds; once the process
  # fd table exhausts (~256 on macOS), pipe() fails, <(...) subshells get invalid fds,
  # and print raises EBADF → fd-watcher goes unregistered → autocomplete stops working.
  # Wrap both widgets to unconditionally close the pipe fd after the original runs.
  if (( ${+functions[.autocomplete:async:wait:fd-widget]} )); then
    functions[.autocomplete:async:wait:fd-widget:orig]=$functions[.autocomplete:async:wait:fd-widget]
    .autocomplete:async:wait:fd-widget() {
      local -i _fd=$1
      .autocomplete:async:wait:fd-widget:orig "$@"
      (( _fd >= 10 )) && exec {_fd}<&- 2>/dev/null
    }
  fi

  if (( ${+functions[.autocomplete:async:complete:fd-widget]} )); then
    functions[.autocomplete:async:complete:fd-widget:orig]=$functions[.autocomplete:async:complete:fd-widget]
    .autocomplete:async:complete:fd-widget() {
      local -i _fd=$1
      .autocomplete:async:complete:fd-widget:orig "$@"
      (( _fd >= 10 )) && exec {_fd}<&- 2>/dev/null
    }
  fi

  # Bug in zsh-autocomplete: async:clear checks -v _autocomplete__async_fd (two
  # underscores — never set anywhere in the plugin) and -t (false for pipe fds).
  # Both guards always fail, so the fallback fd cleanup on line-finish is never run.
  # Rewrite to close the fd unconditionally when the variable is set and valid.
  if (( ${+functions[.autocomplete:async:clear]} )); then
    .autocomplete:async:clear() {
      [[ -v _autocomplete_async_fd && $_autocomplete_async_fd -ge 10 ]] &&
        exec {_autocomplete_async_fd}<&- 2>/dev/null
      unset _autocomplete_async_fd curcontext _autocomplete__isearch
      .autocomplete:async:reset-context
      builtin zle -Rc
      return 0
    }
  fi
}

# Compile completion functions into one digest to avoid per-file Defender scans
# on Windows (30-50 files × ~9ms → 0.30s). Use `builtin zcompile` — a stray
# `autoload zcompile` shadows the builtin and fails with "file not found".
() {
  emulate -L zsh
  local zwc="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completions.zwc"
  if [[ ! -e $zwc || $commands[zsh] -nt $zwc ]]; then
    local files=( ${^fpath}/_*(N.) )
    (( $#files )) && { mkdir -p "${zwc:h}"; builtin zcompile -U "$zwc" $files }
  fi
  [[ -e $zwc ]] && fpath=( "$zwc" $fpath )
}

zmodload zsh/complist
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=2  # 1 match → insert; 2+ → menu

autoload -Uz compinit
compinit -C -d ~/.zcompdump-$HOST  # force-rebuild: compinit -u

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
setopt prompt_subst
typeset -g vcs_info_msg_0_=""

PROMPT=$'\n%F{'"${THEME_MONO7}"$'}%(4~|.../%2~|%~)%f'\
$' %F{'"${THEME_MONO5}"$'}${vcs_info_msg_0_}%f\n'\
$'%(?.%F{'"${THEME_MONO6}"$'}.%F{red})❯%f '

# RPROMPT is fixed for the shell lifetime (SSH/root context never changes).
# Per-prompt $(...) would fork a subshell (~42ms on Windows).
if [[ -n $SSH_CONNECTION || -n $SSH_TTY ]]; then
  RPROMPT="%F{${THEME_MONO5}}%n@%m%f"
elif (( EUID == 0 )); then
  RPROMPT="%F{${THEME_MONO5}}%n%f"
else
  RPROMPT=""
fi

# Fork-free git branch/action: reads .git/ directly, no subprocess.
autoload -Uz add-zsh-hook
_git_prompt_info() {
  emulate -L zsh
  local dir=$PWD gitdir head ref line action
  while [[ -n $dir ]]; do
    if [[ -d $dir/.git ]]; then
      gitdir=$dir/.git; break
    elif [[ -f $dir/.git ]]; then
      line=$(<$dir/.git); gitdir=${line#gitdir: }
      [[ $gitdir != /* && $gitdir != [A-Za-z]:/* ]] && gitdir=$dir/$gitdir
      break
    fi
    [[ $dir == / || $dir == [A-Za-z]:/ ]] && break
    dir=${dir:h}
  done
  if [[ -z $gitdir || ! -d $gitdir || ! -f $gitdir/HEAD ]]; then
    vcs_info_msg_0_=""; return
  fi
  head=$(<$gitdir/HEAD)
  if [[ $head == ref:* ]]; then
    ref=${head#ref: refs/heads/}
  else
    ref="@${head[1,8]}"
  fi
  if [[ -d $gitdir/rebase-merge || -d $gitdir/rebase-apply ]]; then action="rebase"
  elif [[ -f $gitdir/MERGE_HEAD ]]; then action="merge"
  elif [[ -f $gitdir/CHERRY_PICK_HEAD ]]; then action="cherry-pick"
  elif [[ -f $gitdir/BISECT_LOG ]]; then action="bisect"
  fi
  [[ -n $action ]] && ref="$ref|$action"
  vcs_info_msg_0_=" $ref"
}
add-zsh-hook precmd _git_prompt_info

# ----- Plugins -----
ZPLUGDIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"

ZSH_AUTOSUGGEST_MANUAL_REBIND=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=${THEME_MONO4},italic"
[[ -r $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
  source $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh
# MSYS2/Cygwin: async uses process substitution (fork ~30-70ms) per keystroke.
# Plugin auto-enables async for zsh >= 5.0.8; unset explicitly after source.
[[ $OSTYPE == msys* || $OSTYPE == cygwin* ]] && unset ZSH_AUTOSUGGEST_USE_ASYNC

# ----- Keybindings -----
bindkey -e

# Terminal-specific Tab (Ctrl+I) aliases.
bindkey -s $'\e[105;5u' '\t'  # Ghostty: CSI u (codepoint i=105)
bindkey -s $'\e[9;5u'   '\t'  # kitty fallback
bindkey -s $'\e[1;5n'   '\t'  # mintty: XTermModifyOtherKeys=1

# 
bindkey '^J' down-line-or-select

# menuselect keymap (active during completion menu): vim-style navigation.
bindkey -M menuselect '^J' down-history
bindkey -M menuselect '^K' up-history
# ^F accepts the current candidate (mirrors autosuggest-accept outside menuselect).
bindkey -M menuselect '^F' accept-line

# ----- zoxide -----
# Caches init scripts keyed by binary name; regenerates when the binary changes.
# Force refresh: rm ~/.cache/zsh/init_*.zsh
_cached_init() {
  local name="$1" bin_path; shift
  bin_path=$(command -v "$1" 2>/dev/null) || return 0
  local cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/init_${name}.zsh"
  if [[ ! -f "$cache" || "$bin_path" -nt "$cache" ]]; then
    mkdir -p "${cache:h}"
    "$@" > "$cache"
  fi
  source "$cache"
}

_cached_init zoxide zoxide init zsh
# zoxide's per-cd hook forks zoxide.exe (~70ms on Windows); patch it async.
if (( ${+functions[__zoxide_hook]} )); then
  functions[__zoxide_hook_sync]=$functions[__zoxide_hook]
  __zoxide_hook() { __zoxide_hook_sync &! }
fi
unfunction _cached_init

# --- end of zsh.sh -------------------------------------------------------
