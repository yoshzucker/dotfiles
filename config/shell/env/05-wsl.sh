# --- 05-wsl.sh ------------------------------------------------------------
# WSL-specific: force SHELL=zsh when running under WSL1/WSL2.
# Depends on PLATFORM exported by 00-core-env.sh.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

if [ "$PLATFORM" = 'wsl1' ] || [ "$PLATFORM" = 'wsl2' ]; then
  command -v zsh >/dev/null && export SHELL="$(command -v zsh)"
fi

# --- end of p01_core_wsl_shell.sh ----------------------------------------
