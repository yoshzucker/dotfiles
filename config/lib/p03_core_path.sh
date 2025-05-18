# --- p03_core_path.sh ----------------------------------------------------
# Extend PATH for local scripts and virtualenv

# Guard: source only, skip if already loaded
(return 0 2>/dev/null) || { echo "This script must be sourced."; exit 1; }

f="${BASH_SOURCE[0]:-${(%):-%N}}"
b="${f##*/}"
v="__LOADED_${b%.*}__"

if [ -n "${ZSH_VERSION:-}" ]; then
  eval "test -n \"\${$v:-}\""
  if [ $? -eq 0 ]; then
    return 0
  fi
else
  if [ -n "${!v:-}" ]; then
    return 0
  fi
fi

eval "$v=1"

[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.venv/bin" ] && export PATH="$HOME/.venv/bin:$PATH"

if command -v brew >/dev/null 2>&1; then
  BREW_PREFIX="$(brew --prefix)"
  [ -d "$BREW_PREFIX/opt/coreutils/libexec/gnubin" ] &&
    PATH="$BREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
  [ -d "$BREW_PREFIX/opt/findutils/libexec/gnubin" ] &&
    PATH="$BREW_PREFIX/opt/findutils/libexec/gnubin:$PATH"
fi

# --- end of p03_core_path.sh ---------------------------------------------
