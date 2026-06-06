# --- macos.sh ----------------------------------------------------------------
# macOS-specific: Homebrew init, BREW_PREFIX, GNU coreutils/findutils PATH.
# No-op on non-Darwin systems. Safe for bash and zsh.
# Exports: BREW_PREFIX, HOMEBREW_CURLRC (when ~/.curlrc exists)
# Modifies: PATH (Homebrew bin, gnubin)

_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c 'a-zA-Z0-9' '_')"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && . "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

[ "$(uname -s)" = "Darwin" ] || return 0

if [ -e /opt/homebrew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

[ -e "$HOME/.curlrc" ] && export HOMEBREW_CURLRC=1

if command -v brew >/dev/null 2>&1; then
  BREW_PREFIX="$(brew --prefix 2>/dev/null || true)"
  if [ -n "$BREW_PREFIX" ]; then
    export BREW_PREFIX
    [ -d "$BREW_PREFIX/opt/coreutils/libexec/gnubin" ] &&
      PATH="$BREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
    [ -d "$BREW_PREFIX/opt/findutils/libexec/gnubin" ] &&
      PATH="$BREW_PREFIX/opt/findutils/libexec/gnubin:$PATH"
    export PATH
  fi
fi

# --- end of macos.sh ---------------------------------------------------------
