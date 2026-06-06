# --- 10-brew-env.sh ------------------------------------------------------
# Homebrew environment initialization (shellenv + prefix).
# Central place for BREW_PREFIX (consumed by 15-path, 45-fzf, etc.).
# Targets macOS (/opt/homebrew); no-op on MSYS2 where brew is absent.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

if [ -e /opt/homebrew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

[ -e "$HOME/.curlrc" ] && export HOMEBREW_CURLRC=1

if command -v brew >/dev/null 2>&1; then
  BREW_PREFIX="$(brew --prefix 2>/dev/null || true)"
  [ -n "$BREW_PREFIX" ] && export BREW_PREFIX
fi

# --- end of 10-brew-env.sh -----------------------------------------------
