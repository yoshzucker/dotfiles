# --- common.sh ---------------------------------------------------------------
# Shared env: theme, timezone, and user-local PATH entries.
# Applies on macOS and MSYS2. Safe for bash and zsh.
# Exports: THEME_NAME, THEME_VARIANT, TZ
# Modifies: PATH

_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c 'a-zA-Z0-9' '_')"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && . "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

export THEME_NAME="gensho"   # or 'nord', 'solarized'
export THEME_VARIANT="dark"  # or 'light'
export TZ='Asia/Tokyo'

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.venv/bin"  ] && PATH="$HOME/.venv/bin:$PATH"
export PATH

# --- end of common.sh --------------------------------------------------------
