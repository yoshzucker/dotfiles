# --- p02_core_brew.sh ----------------------------------------------------
# Setup Homebrew environment

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

if [ "$DISTRIBUTION" = 'darwin' ] && [ -e /opt/homebrew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ "$DISTRIBUTION" = 'ubuntu' ] && [ -e /home/linuxbrew ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
  umask 002
fi

[ -e "$HOME/.curlrc" ] && export HOMEBREW_CURLRC=1

# --- end of p02_core_brew.sh ---------------------------------------------
