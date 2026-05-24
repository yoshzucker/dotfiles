# --- 15-path.sh ----------------------------------------------------------
# Extend PATH: ~/.local/bin, ~/.venv/bin, brew coreutils/findutils gnubin.
# Consumes BREW_PREFIX when available (from 10-brew-env.sh).

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.venv/bin" ] && export PATH="$HOME/.venv/bin:$PATH"

# Prefer BREW_PREFIX exported by 10-brew-env.sh
_brew_p="${BREW_PREFIX:-$(command -v brew >/dev/null 2>&1 && brew --prefix 2>/dev/null || true)}"
if [ -n "$_brew_p" ]; then
  [ -d "$_brew_p/opt/coreutils/libexec/gnubin" ] &&
    PATH="$_brew_p/opt/coreutils/libexec/gnubin:$PATH"
  [ -d "$_brew_p/opt/findutils/libexec/gnubin" ] &&
    PATH="$_brew_p/opt/findutils/libexec/gnubin:$PATH"
fi
unset _brew_p

# --- end of p03_core_path.sh ---------------------------------------------
