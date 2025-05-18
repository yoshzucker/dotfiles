# --- p01_core_wsl_shell.sh -----------------------------------------------
# In WSL, ensure SHELL points to zsh

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

if [ "$PLATFORM" = 'wsl1' ] || [ "$PLATFORM" = 'wsl2' ]; then
  command -v zsh >/dev/null && export SHELL="$(command -v zsh)"
fi

# --- end of p01_core_wsl_shell.sh ----------------------------------------
