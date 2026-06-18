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

typeset -g vcs_info_msg_0_=""

PROMPT=$'\n%F{'"${THEME_MONO7}"$'}%(4~|.../%2~|%~)%f'\
$' %F{'"${THEME_MONO5}"$'}${vcs_info_msg_0_}%f\n'\
$'%(?.%F{'"${THEME_MONO6}"$'}.%F{red})❯%f '

# Context (user@host on SSH, user on root) is fixed for the life of the shell,
# so compute RPROMPT ONCE here rather than via $(...) on every prompt -- a
# per-prompt command substitution forks a subshell (~42ms on Windows/Defender).
# %n/%m/%F are prompt escapes, re-rendered cheaply each draw, so the indicator
# still shows on every prompt line (point-of-action safety + scrollback record).
if [[ -n $SSH_CONNECTION || -n $SSH_TTY ]]; then
  RPROMPT="%F{${THEME_MONO5}}%n@%m%f"
elif (( EUID == 0 )); then
  RPROMPT="%F{${THEME_MONO5}}%n%f"
else
  RPROMPT=""
fi

# ----- key -----
bindkey -e
bindkey '^H' backward-word
bindkey '^L' forward-word
bindkey '^F' backward-char
bindkey '^B' forward-char

# ----- plugins (direct source, no manager) -----
# We source the plugin repos directly instead of using a manager. sheldon spawns
# a native sheldon.exe on every shell start (a fork we work hard to avoid on
# Windows) and, on this corporate box, fails to lock its config dir entirely
# (os error 5) so it loaded nothing. These three are plain `source`-able repos;
# `bootstrap` clones them into $ZPLUGDIR.
#
# Load order is significant: fzf-tab needs compinit (already run above), and
# fast-syntax-highlighting must be last so it sees every widget the others bind.
#
# These knobs must be set BEFORE the plugins that read them are sourced.
# Suggestions run synchronously: the async path uses zsh/zpty, which forks a
# worker per fetch (~expensive on Windows); the history strategy is in-process.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1   # skip per-precmd widget rebind overhead
ZSH_HIGHLIGHT_MAXLENGTH=512       # skip syntax highlighting beyond 512 chars

ZPLUGDIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"
[[ -r $ZPLUGDIR/fzf-tab/fzf-tab.plugin.zsh ]] && source $ZPLUGDIR/fzf-tab/fzf-tab.plugin.zsh
[[ -r $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && source $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh
[[ -r $ZPLUGDIR/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ]] && source $ZPLUGDIR/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

# ----- git status (fork-free) -----
# vcs_info forks git several times per prompt and probes every VCS backend
# (cvs/bzr/fossil/...), costing ~0.5s per prompt on Windows where each fork
# spawns a native git.exe. We show only branch + in-progress action, both
# readable straight from .git/ with zero subprocess -> ~1ms.
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

# ----- completion menu -----
# Apply fzf-tab's styles only when fzf-tab actually loaded (it defines
# enable-fzf-tab). Otherwise fall back to zsh's native menu-select so TAB still
# completes — without this guard, `menu no` would disable the menu with nothing
# to take over (e.g. on machines where the plugins aren't cloned).
if (( ${+functions[enable-fzf-tab]} )); then
  # Native menu off so fzf-tab can take over.
  # No bat preview (expensive process spawn); eza listing for cd only.
  zstyle ':completion:*' menu no
  zstyle ':fzf-tab:complete:cd:*' fzf-preview \
    'eza -1 --color=always --icons=auto $realpath 2>/dev/null || ls -1 $realpath'
  zstyle ':fzf-tab:*' fzf-flags --height=60% --layout=reverse --border=rounded
  zstyle ':fzf-tab:*' switch-group ',' '.'
else
  zstyle ':completion:*' menu select
fi

# ----- external integrations (startup cache) -----
# Cache each tool init output; regenerated when binary mtime changes.
# Force refresh: rm ~/.cache/zsh/init_*.zsh
_cached_init() {
  local name="$1"; shift
  local bin_path
  bin_path=$(command -v "$1" 2>/dev/null) || return 0
  local cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/init_${name}.zsh"
  if [[ ! -f "$cache" || "$bin_path" -nt "$cache" ]]; then
    mkdir -p "${cache:h}"
    "$@" > "$cache" 2>/dev/null
  fi
  source "$cache"
}

_cached_init zoxide  zoxide init zsh
# Make zoxide's per-cd `zoxide add` non-blocking: it forks a native zoxide.exe
# (~70ms on Windows) on every directory change. Background it so the prompt
# returns immediately; the frecency DB update finishes async. (The fork to
# spawn the background job is the cygwin floor, removable only via a Defender
# exclusion.)
if (( ${+functions[__zoxide_hook]} )); then
  functions[__zoxide_hook_sync]=$functions[__zoxide_hook]
  __zoxide_hook() { __zoxide_hook_sync &! }
fi
unfunction _cached_init

# --- end of zsh.sh -------------------------------------------------------
