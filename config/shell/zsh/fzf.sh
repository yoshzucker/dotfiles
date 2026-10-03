# --- fzf.sh --------------------------------------------------------------
# fzf keybindings (Ctrl-T, Alt-C, Ctrl-R), `**' completion, default
# opts/command (fd/rg), theme colors.

[ -n "$ZSH_VERSION" ] || return 0

if command -v fzf >/dev/null 2>&1; then
  # The three widgets are written here rather than taken from fzf's
  # key-bindings.zsh.  Before fzf appears, that file's widgets spend a
  # subshell on each of their option and command helpers, `cat' on an
  # options file this setup does not use, and for Ctrl-R a printf and a
  # perl to drop duplicates hist_ignore_all_dups already keeps out -- eight
  # to ten processes, a second or two on Windows.  These start fzf and
  # nothing else (and fd, through fzf, for Ctrl-T and Alt-C).
  #
  # Options are layered as fzf's own widgets layer them: FZF_DEFAULT_OPTS,
  # then the widget's *_OPTS, then what the widget itself needs.  stdin is
  # the terminal for Ctrl-T and Alt-C, which is what makes fzf run
  # FZF_DEFAULT_COMMAND, or its own walker when that is empty.

  # Ctrl-T: paste the chosen paths, quoted, after the cursor.
  fzf-file-widget() {
    local out
    out=$(FZF_DEFAULT_COMMAND=${FZF_CTRL_T_COMMAND-} \
          FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS-} ${FZF_CTRL_T_OPTS-}" \
          command fzf --scheme=path --walker=file,dir,follow,hidden -m < /dev/tty)
    [[ -n $out ]] && LBUFFER+="${(j: :)${(@q)${(f)out}}} "
    zle reset-prompt
  }

  # Alt-C: cd to the chosen directory, as a command of its own so the
  # prompt redraws for the new directory and the line typed so far returns.
  fzf-cd-widget() {
    local dir
    dir=$(FZF_DEFAULT_COMMAND=${FZF_ALT_C_COMMAND-} \
          FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS-} ${FZF_ALT_C_OPTS-}" \
          command fzf --scheme=path --walker=dir,follow,hidden +m < /dev/tty)
    if [[ -z $dir ]]; then
      zle redisplay
      return 0
    fi
    zle push-line
    BUFFER="builtin cd -- ${(q)dir:a}"
    zle accept-line
    local ret=$?
    zle reset-prompt
    return $ret
  }

  # Ctrl-R: replace the line with a command from the history.  The history
  # comes in from a here-string, NUL-separated so multi-line commands stay
  # whole; ${history} lists it newest first.  Ctrl-R again toggles the
  # ranking off, to plain recency.  The line typed so far is the query,
  # passed inside FZF_DEFAULT_OPTS rather than as an argument: MSYS2
  # rewrites an argument like --query=/usr/bin into a Windows path on its
  # way to a native fzf.exe.
  fzf-history-widget() {
    local selected
    selected=$(FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS-} ${FZF_CTRL_R_OPTS-} --query=${(qqq)LBUFFER}" \
               command fzf --scheme=history --read0 +m \
                 --bind=ctrl-r:toggle-sort \
                 <<<${(pj:\0:)history})
    if [[ -n $selected ]]; then
      BUFFER=$selected
      CURSOR=$#BUFFER
    fi
    zle reset-prompt
  }

  zle -N fzf-file-widget
  zle -N fzf-cd-widget
  zle -N fzf-history-widget
  bindkey -M emacs '^T'  fzf-file-widget
  bindkey -M emacs '\ec' fzf-cd-widget
  bindkey -M emacs '^R'  fzf-history-widget

  # `**<Tab>' completion.  Homebrew keeps the repository's shell/ directory;
  # MSYS2's package installs the same file under share/fzf.  Read from disk
  # rather than through `fzf --zsh', which would start fzf in every shell.
  # Ordinary Tab passes through it without starting anything.
  for __d in ${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/opt/fzf/shell} \
             ${MINGW_PREFIX:+$MINGW_PREFIX/share/fzf}; do
    [[ -r $__d/completion.zsh ]] || continue
    source $__d/completion.zsh
    break
  done
  unset __d

  # Single-line FZF_DEFAULT_OPTS: third-party callers shell-parse this variable
  # and choke on embedded newlines or parenthesized actions
  # (e.g. execute-silent(...)). Persistent layout + safe binds only here;
  # per-context complexity (execute-silent for copy) lives in *_OPTS below.
  export FZF_DEFAULT_OPTS="--smart-case --height=60% --layout=reverse --border=rounded --info=inline-right --scrollbar=│ --marker=▎ --pointer=▌ --bind=ctrl-/:toggle-preview --bind=alt-a:select-all --bind=alt-d:deselect-all --color=bg+:${THEME_MONO1} --color=fg+:${THEME_MONO7} --color=hl:${THEME_MONO6},hl+:${THEME_MONO7} --color=info:${THEME_MONO5},prompt:${THEME_MONO6} --color=pointer:${THEME_MONO6} --color=marker:${THEME_MONO6},spinner:${THEME_MONO5} --color=header:${THEME_MONO5} --color=border:${THEME_MONO3}"
fi

if command -v fd >/dev/null 2>&1; then
  # fd already reads ~/.config/fd/ignore; only --hidden/--follow tweaks here.
  export FZF_DEFAULT_COMMAND='fd --hidden --follow --type f'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND='fd --hidden --follow --type d'
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g !.git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

# Platform clipboard for ctrl-y copy binding.
# execute-silent(...) with spaces inside must be double-quoted in the opts string:
# Windows CommandLineToArgvW splits on unquoted spaces, breaking the action.
if [[ $OSTYPE == darwin* ]]; then
  _fzf_copy='pbcopy'
elif command -v win32yank >/dev/null 2>&1; then
  _fzf_copy='win32yank -i'
else
  _fzf_copy='clip'
fi

# Ctrl-T: file picker with bat preview + ctrl-y to copy path.
export FZF_CTRL_T_OPTS=$'--preview "bat --color=always --style=numbers --line-range=:300 {} 2>/dev/null || eza -1 --color=always --icons=auto {} 2>/dev/null"\n--preview-window=right,60%,border-left\n'"--bind=\"ctrl-y:execute-silent(printf %s {} | ${_fzf_copy})+abort\""

# Alt-c: directory jump with eza tree preview.
export FZF_ALT_C_OPTS=$'--preview "eza --tree --level=2 --color=always --icons=auto {} 2>/dev/null"\n--preview-window=right,50%,border-left\n'"--bind=\"ctrl-y:execute-silent(printf %s {} | ${_fzf_copy})+abort\""

unset _fzf_copy

# --- end of fzf.sh -------------------------------------------------------
