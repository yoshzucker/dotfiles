# --- fzf.sh --------------------------------------------------------------
# fzf keybindings, completions, default opts/command (fd/rg), theme colors.
# Sources the zsh integration from wherever the package manager put it.

[ -n "$ZSH_VERSION" ] || return 0

if command -v fzf >/dev/null 2>&1; then
  # Homebrew keeps the repository's shell/ directory; MSYS2's package
  # installs the same files under share/fzf.  Read from disk rather than
  # through `fzf --zsh', which would start fzf in every shell.
  for __d in ${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/opt/fzf/shell} \
             ${MINGW_PREFIX:+$MINGW_PREFIX/share/fzf}; do
    [[ -r $__d/key-bindings.zsh ]] || continue
    source $__d/key-bindings.zsh
    source $__d/completion.zsh
    break
  done
  unset __d

  # Single-line FZF_DEFAULT_OPTS: third-party callers shell-parse this variable
  # and choke on embedded newlines or parenthesized actions
  # (e.g. execute-silent(...)). Persistent layout + safe binds only here;
  # per-context complexity (execute-silent for copy) lives in *_OPTS below.
  # Ctrl-R is fzf-history-widget (bound here by fzf's key-bindings.zsh).
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
