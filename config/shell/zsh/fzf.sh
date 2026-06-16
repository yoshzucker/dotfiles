# --- fzf.sh --------------------------------------------------------------
# fzf keybindings, completions, default opts/command (fd/rg), theme colors.
# Searches multiple possible fzf install locations; prefers BREW_PREFIX.

[ -n "$ZSH_VERSION" ] || return 0

typeset -g __FZF_SOURCED=0
__fzf_try_source() {
  local d
  for d in "$1/share/fzf" "$1/shell"; do
    if [[ -f "$d/key-bindings.zsh" ]]; then
      source "$d/key-bindings.zsh"
      __FZF_SOURCED=1
    fi
    if [[ -f "$d/completion.zsh" ]]; then
      source "$d/completion.zsh"
      __FZF_SOURCED=1
    fi
  done
}

if command -v fzf >/dev/null 2>&1; then
  # Prefer centralized BREW_PREFIX from env/macos.sh, then common locations
  typeset -a __FZF_BASES=(
    "${BREW_PREFIX:+${BREW_PREFIX}/opt/fzf}"
    "${HOMEBREW_PREFIX:+${HOMEBREW_PREFIX}/opt/fzf}"
    /opt/homebrew/opt/fzf
    /usr/local/opt/fzf
    /mingw64
    /ucrt64
    /usr
    "$HOME/.fzf"
    "${USERPROFILE//\\/\/}/scoop/apps/fzf/current"
  )
  for base in "${__FZF_BASES[@]}"; do
    [[ -n "$base" && -d "$base" ]] || continue
    __fzf_try_source "$base"
    (( __FZF_SOURCED )) && break
  done
fi
unset -f __fzf_try_source
unset __FZF_SOURCED __FZF_BASES

if command -v fzf >/dev/null 2>&1; then
  # Single-line FZF_DEFAULT_OPTS: third-party callers (navi, etc.) shell-parse
  # this variable and choke on embedded newlines or parenthesized actions
  # (e.g. execute-silent(...)). Persistent layout + safe binds only here;
  # per-context complexity (execute-silent for copy) lives in *_OPTS below.
  # Ctrl-R is left to atuin (its init in zsh.sh runs after this file).
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

# Ctrl-T: file picker with bat preview + ctrl-y to copy path.
export FZF_CTRL_T_OPTS=$'--preview "bat --color=always --style=numbers --line-range=:300 {} 2>/dev/null || eza -1 --color=always --icons=auto {} 2>/dev/null"\n--preview-window=right,60%,border-left\n--bind=ctrl-y:execute-silent(printf %s {} | pbcopy)+abort'

# Alt-c: directory jump with eza tree preview.
export FZF_ALT_C_OPTS=$'--preview "eza --tree --level=2 --color=always --icons=auto {} 2>/dev/null"\n--preview-window=right,50%,border-left\n--bind=ctrl-y:execute-silent(printf %s {} | pbcopy)+abort'

# --- end of fzf.sh -------------------------------------------------------
