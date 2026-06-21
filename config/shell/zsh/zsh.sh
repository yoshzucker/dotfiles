# --- zsh.sh ---------------------------------------------------------------
# Zsh interactive shell: history, completion, prompt (fork-free git branch),
# keybindings, plugins. Requires THEME_MONO* from colors.sh.

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

# Pre-compile all completion functions into one digest, prepended to fpath.
# On Defender-scanned Windows each autoloaded function is a separately-scanned
# file read (~9ms), so the first TAB pays 0.5-2s loading the 30-50 functions a
# completion needs. One digest = one read: measured 777 functions 7.11s -> 0.30s.
# Rebuilt only when missing or after a zsh upgrade (new function files); the
# ~1.8s build is a rare one-off. NOTE: call the `zcompile` builtin with FILE
# PATHS (not names) -- a stray `autoload zcompile` would shadow the builtin and
# fail with "function definition file not found".
() {
  emulate -L zsh
  local zwc="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completions.zwc"
  if [[ ! -e $zwc || $commands[zsh] -nt $zwc ]]; then
    local files=( ${^fpath}/_*(N.) )
    if (( $#files )); then
      mkdir -p "${zwc:h}"
      builtin zcompile -U "$zwc" $files 2>/dev/null
    fi
  fi
  [[ -e $zwc ]] && fpath=( "$zwc" $fpath )
}

autoload -Uz compinit
compinit -C -d ~/.zcompdump-$HOST # rebuild -> compinit -u -d ~/.zcompdump-$HOST

# ----- completion -----
# Case-insensitive, plus substring/partial matching so a fragment narrows the
# candidates without fuzzy (e.g. `ki`->kill-server, `srv`->kill-server). This
# gives fzf-like "type to narrow" in-process — no external picker spawn.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*'

# Completion display threshold.
# > 0: suppress "do you wish to see all N?" and auto-invoke fzf-tab instead.
# = 0: disabled; all completions go directly to menuselect.
LISTMAX=20

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
bindkey '^J' down-line-or-history
bindkey '^K' up-line-or-history
bindkey '^L' forward-word

# menuselect keymap (active while the completion menu is open).
# Ctrl+HJKL mirrors arrow-key navigation vim-style.
zmodload zsh/complist
bindkey -M menuselect '^H'         backward-char
# bindkey -M menuselect '^I'         menu-complete  # TAB (raw \t)
# bindkey -M menuselect $'\e[105;5u' menu-complete  # Ghostty/Kitty kbd: Ctrl+I → CSI u
# bindkey -M menuselect $'\e[9;5u'   menu-complete  # kitty-style fallback
bindkey -M menuselect '^J'         down-line-or-history
bindkey -M menuselect '^K'         up-line-or-history
bindkey -M menuselect '^L'         forward-char

# Completion widget: counts matches via compstate, suppresses list+insert when
# matches exceed LISTMAX so the "do you wish to see all N?" prompt never fires.
# Result is stored in _smart_tab_nmatches for _tab_enter_menu to read.
typeset -gi _smart_tab_nmatches=0
_smart_tab_fn() {
  _main_complete
  typeset -g _smart_tab_nmatches=$compstate[nmatches]
  if (( LISTMAX > 0 && _smart_tab_nmatches > LISTMAX )); then
    compstate[list]=
    compstate[insert]=
  fi
}
zle -C _smart_tab_expand complete-word _smart_tab_fn

# TAB: insert longest common prefix on first press; if nothing more can be
# inserted (buffer unchanged), immediately enter menuselect on the same press.
# When candidates exceed LISTMAX, auto-invoke fzf-tab with a loading message.
_tab_enter_menu() {
  POSTDISPLAY=''  # clear autosuggestion ghost before menu becomes interactive
  local buf=$BUFFER cur=$CURSOR
  _smart_tab_nmatches=0
  zle _smart_tab_expand
  if (( LISTMAX > 0 && ${+widgets[fzf-tab-complete]} && _smart_tab_nmatches > LISTMAX )); then
    zle -R "fzf-tab: loading ${_smart_tab_nmatches} candidates..."
    zle fzf-tab-complete
    return
  fi
  [[ $BUFFER == $buf && $CURSOR == $cur ]] && zle _smart_tab_expand
}
zle -N _tab_enter_menu
# Ctrl+I (Tab + Ctrl):
#   tmux inside / legacy term : raw 0x09 → '^I'
#   Ghostty outside tmux      : Kitty kbd → \e[105;5u (codepoint 'i'=105)
#   kitty-style "associate Tab": \e[9;5u (kept as a no-cost fallback)
bindkey '^I'         _tab_enter_menu
bindkey $'\e[105;5u' _tab_enter_menu
bindkey $'\e[9;5u'   _tab_enter_menu
# Ctrl+. (Period + Ctrl) → fzf-tab-complete
#   mintty outside tmux  : XTermModifyOtherKeys=1 → \e[1;5n
#   tmux extended-keys on: CSI u → \e[46;5u (codepoint '.'=46)
bindkey $'\e[1;5n'  fzf-tab-complete
bindkey $'\e[46;5u' fzf-tab-complete

# ----- plugins (direct source, no manager) -----
# Load order: zsh-autosuggestions → fast-syntax-highlighting (last).
# fzf-tab is loaded earlier in fzf.sh (see there for rationale).
# Knobs set before source so plugins read them on load.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1   # skip per-precmd widget rebind overhead
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=${THEME_MONO4},italic"  # mono4 = faint/shadow; italic adds shape contrast vs input
ZSH_HIGHLIGHT_MAXLENGTH=512       # skip syntax highlighting beyond 512 chars
# FSH free-theme default comment=fg=black (mono1, +6L above bg) is nearly
# invisible; override before source so free_theme.zsh's := does not overwrite.
typeset -gA FAST_HIGHLIGHT_STYLES
FAST_HIGHLIGHT_STYLES[freecomment]="fg=${THEME_MONO5},italic"

ZPLUGDIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"
[[ -r $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && source $ZPLUGDIR/zsh-autosuggestions/zsh-autosuggestions.zsh
# Append custom clear widgets AFTER source so plugin's default list (which
# includes accept-line, history-search-*, etc.) is initialized first.
# Pre-source += sets ${+VAR}=1, causing config.zsh's guard to skip defaults.
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(
  _tab_enter_menu   # TAB path: clears before menuselect becomes interactive
  fzf-tab-complete  # Ctrl+. path: clears before fzf opens
)
# Disable async on MSYS2/Cygwin: async uses process substitution (fork) per
# keystroke; fork is ~30-70ms on Windows. Sync mode reads $history in-process.
# The plugin auto-enables async via `typeset -g ZSH_AUTOSUGGEST_USE_ASYNC=`
# for zsh >= 5.0.8, so we must explicitly unset after source to disable it.
[[ $OSTYPE == msys* || $OSTYPE == cygwin* ]] && unset ZSH_AUTOSUGGEST_USE_ASYNC
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
# menu select=2: single match → complete directly; 2+ matches → menu select.
# No `interactive` flag: hjkl / Ctrl+HJKL act as navigation, not filter chars.
zstyle ':completion:*' menu select=2

# list-colors: file-type colors + selected-item highlight.
# di= follows dired-directory → font-lock-type-face → cyan (#5f9196).
# ma= must include the fg explicitly: zsh applies ma= standalone for selected
# items and does NOT layer the type color on top. Without an explicit fg,
# selected items fall back to terminal default instead of their type color.
# Using the same cyan fg as di= makes selection appear as bg-only change.
() {
  local dihex="${THEME_CYAN#\#}" bhex="${THEME_MONO1#\#}"
  local dr=$((16#${dihex[1,2]})) dg=$((16#${dihex[3,4]})) db=$((16#${dihex[5,6]}))
  local br=$((16#${bhex[1,2]})) bg=$((16#${bhex[3,4]})) bb=$((16#${bhex[5,6]}))
  zstyle ':completion:*' list-colors \
    "di=38;2;${dr};${dg};${db}" \
    "ma=38;2;${dr};${dg};${db};48;2;${br};${bg};${bb}"
}

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
