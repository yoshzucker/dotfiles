# --- 00-core-env.sh ------------------------------------------------------
# Set theme name and variant for downstream UI modules.
# Exports: THEME_NAME, THEME_VARIANT

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

theme_name="gensho"   # or 'nord', 'solarized'
theme_variant="dark"  # or 'light'

export THEME_NAME="$theme_name"
export THEME_VARIANT="$theme_variant"

# --- end of 00-core-env.sh -----------------------------------------------
