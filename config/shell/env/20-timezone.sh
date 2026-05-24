# --- 20-timezone.sh ------------------------------------------------------
# Default timezone (Asia/Tokyo) for consistent dates in all shells.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

export TZ='Asia/Tokyo'

# --- end of p04_core_timezone.sh -----------------------------------------
