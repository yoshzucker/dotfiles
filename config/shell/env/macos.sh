# --- macos.sh ----------------------------------------------------------------
# macOS-specific: Homebrew init, BREW_PREFIX, GNU coreutils/findutils PATH.
# No-op on non-Darwin systems. Safe for bash and zsh.
# Exports: BREW_PREFIX, HOMEBREW_CURLRC (when ~/.curlrc exists),
#          HOMEBREW_NO_ENV_HINTS
# Modifies: PATH (Homebrew bin, gnubin)

[ "$(uname -s)" = "Darwin" ] || return 0

if [ -e /opt/homebrew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

[ -e "$HOME/.curlrc" ] && export HOMEBREW_CURLRC=1

# Silence the "Adjust how often ... / Hide these hints with ..." footer that
# brew prints after auto-update. Auto-update itself stays enabled.
export HOMEBREW_NO_ENV_HINTS=1

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
