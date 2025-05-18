# --- z41_tool_fzf.sh -----------------------------------------------------
# fzf integration (for interactive shell)

[ -n "$ZSH_VERSION" ] || return 0

if command -v fzf >/dev/null; then
  source "$(brew --prefix)/opt/fzf/shell/key-bindings.zsh"
  source "$(brew --prefix)/opt/fzf/shell/completion.zsh"

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

# --- end of z41_tool_fzf.sh ----------------------------------------------
