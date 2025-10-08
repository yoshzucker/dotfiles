# --- z41_tool_fzf.sh -----------------------------------------------------
# fzf integration (for interactive shell)

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
  typeset -a __FZF_BASES=(
    "${HOMEBREW_PREFIX:-$(brew --prefix 2>/dev/null)}/opt/fzf"
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
  export FZF_DEFAULT_OPTS=" \
  --no-bold \
  --smart-case \
  --color=bg+:${THEME_BG_HIGHLIGHT}, \
  --color=fg+:${THEME_PRIMARY} \
  --color=hl:${THEME_SECONDARY},hl+:${THEME_SECONDARY} \
  --color=info:${THEME_PRIMARY},prompt:${THEME_PRIMARY} \
  --color=pointer:${THEME_PRIMARY} \
  --color=marker:${THEME_PRIMARY},spinner:${THEME_PRIMARY} \
  --color=header:${THEME_PRIMARY} \
  "
fi

if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude .git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g !.git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

# --- end of z41_tool_fzf.sh ----------------------------------------------
