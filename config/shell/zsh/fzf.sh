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
  export FZF_DEFAULT_OPTS="--smart-case
--color=bg+:${THEME_MONO1}
--color=fg+:${THEME_MONO7}
--color=hl:${THEME_MONO6},hl+:${THEME_MONO7}
--color=info:${THEME_MONO5},prompt:${THEME_MONO6}
--color=pointer:${THEME_MONO6}
--color=marker:${THEME_MONO6},spinner:${THEME_MONO5}
--color=header:${THEME_MONO5}"
fi

if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude .git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g !.git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

export FZF_CTRL_T_OPTS=$'--height 50%\n--preview "ls -lh --color=always {} 2>/dev/null || tree -L 1 -C {} 2>/dev/null"'

# --- end of fzf.sh -------------------------------------------------------
