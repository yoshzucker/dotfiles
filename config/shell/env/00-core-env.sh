# --- 00-core-env.sh ------------------------------------------------------
# Detect OS, distribution, platform and theme.
# Exports: OS, DISTRIBUTION, PLATFORM, THEME_NAME, THEME_VARIANT
# and derived THEME_* color variables (consumed by UI modules and tmux).
#
# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

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

# Set environment variables for terminal theme
theme_name="gensho"           # or 'solarized'
theme_variant="dark"        # or 'light'

# Export all
export OS="$os"
export DISTRIBUTION="$distribution"
export PLATFORM="$platform"
export THEME_NAME="$theme_name"
export THEME_VARIANT="$theme_variant"

# --- end of p00_core_env.sh ----------------------------------------------
