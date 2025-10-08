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
  __fzf_opts_new=$(cat <<EOF
--no-bold
--smart-case
--color=bg+:${THEME_BG_HIGHLIGHT}
--color=fg+:${THEME_PRIMARY}
--color=hl:${THEME_SECONDARY},hl+:${THEME_SECONDARY}
--color=info:${THEME_PRIMARY},prompt:${THEME_PRIMARY}
--color=pointer:${THEME_PRIMARY}
--color=marker:${THEME_PRIMARY},spinner:${THEME_PRIMARY}
--color=header:${THEME_PRIMARY}
EOF
)
  export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:+$FZF_DEFAULT_OPTS$'\n'}${__fzf_opts_new}"
  unset __fzf_opts_new
fi

if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude .git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden -g !.git'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

export FZF_CTRL_T_OPTS=$'--height 50%\n--preview "ls -lh --color=always {} 2>/dev/null || tree -L 1 -C {} 2>/dev/null"'

typeset -ga FZF_FD_EXCLUDES=(
  --exclude .git
  --exclude .cache
  --exclude .DS_Store
  --exclude .Trash
  --exclude .local/share/Trash
  --exclude node_modules
  --exclude .venv
  --exclude __pycache__
  --exclude AppData
  --exclude '$Recycle.Bin'
  --exclude 'System Volume Information'
)

command -v fd >/dev/null 2>&1 || return 0
fcd() {
  local start_dir="${1:-.}"
  local dir
  dir=$(fd -t d --hidden --follow \
        "${FZF_FD_EXCLUDES[@]}" '' "$start_dir" \
          | fzf --select-1 --exit-0 \
                --preview 'ls -1 --color=always {} | head -20' \
                --height 40% --bind 'esc:cancel') || return
  cd -- "$dir" || return
}

fmv() {
  local -a src
  local dest
  local start_dir="${1:-.}"

  src=("${(@0)$(fd -t f -t d --hidden --follow "${FZF_FD_EXCLUDES[@]}" \
                                      --print0 '' "$start_dir" \
      | fzf --read0 --print0 --multi --select-1 --exit-0 \
            --preview 'ls -lh --color=always {} 2>/dev/null || echo dir' \
            --height 40% --bind 'esc:cancel')}") || return

  dest=$(fd -t d --hidden --follow "${FZF_FD_EXCLUDES[@]}" '' "$start_dir" \
           | fzf --prompt="Move to > " \
                 --select-1 --exit-0 \
                 --preview 'ls -1 --color=always {} | head -20' \
                 --height 40% --bind 'esc:cancel') || return

  print -r -- "\nMoving:"
  printf '  %s\n' "${src[@]}"
  print -r -- "→ $dest\n"

  src=("${(@)src:#$dest}")
  mv -iv -- "${src[@]}" "$dest"
}

# --- end of z41_tool_fzf.sh ----------------------------------------------
