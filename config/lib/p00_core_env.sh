# --- p00_core_env.sh -----------------------------------------------------
# Detect OS, distribution, platform, and terminal type.
# Sets environment variables:
#   - os:           darwin | linux
#   - distribution: darwin | ubuntu | debian | msys2 | ...
#   - platform:     native | wsl1 | wsl2
#   - term_type:    ansi | iterm

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

# OS & Distribution Detection
os="unknown"
distribution="unknown"

if [ "$(uname)" = "Darwin" ]; then
  os="darwin"
  distribution="darwin"
elif [ -f /etc/os-release ]; then
  . /etc/os-release 2>/dev/null || true
  os="linux"
  distribution="${ID:-unknown}"
fi

# Platform Detection
platform="native"
if grep -qEi "(Microsoft|WSL)" /proc/version 2>/dev/null; then
  platform="wsl2"
  [ -z "${WSL_DISTRO_NAME:-}" ] && platform="wsl1"
fi

# Terminal Type Detection
case "$TERM_PROGRAM" in
  Apple_Terminal*) term_type="ansi"  ;;
  iTerm*)          term_type="iterm" ;;
  *)               term_type="ansi"  ;;
esac

# Set environment variables for terminal theme
theme_name="nord"           # or 'solarized'
theme_variant="dark"        # or 'light'

# Export all
export OS="$os"
export DISTRIBUTION="$distribution"
export PLATFORM="$platform"
export TERM_TYPE="$term_type"
export THEME_NAME="$theme_name"
export THEME_VARIANT="$theme_variant"

# --- end of p00_core_env.sh ----------------------------------------------
