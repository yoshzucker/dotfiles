#!/usr/bin/env sh
# --- loader.sh -------------------------------------------------------------
# UL dotfiles — Option B: single source of truth for one-time module loading.
# Sourced by every env/*.sh and zsh/*.sh to avoid 20-line boilerplate duplication.
#
# Recommended usage in a module (place this block right after the header comment):
#
#   _module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c 'a-zA-Z0-9' '_')"
#   _script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
#   [ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
#   __load_guard "$_module_name" || return 0
#
# Why the explicit name? Capturing BASH_SOURCE[0] at the module's top level
# (before any function call) is reliable in both bash and zsh. Passing it in
# avoids the common "caller inside function sees wrong file" problem.
#
# This works whether the files are symlinked under ~/.config/shell/ or you are
# sourcing directly from the git checkout.
# ---------------------------------------------------------------------------

# __load_guard <module_basename_without_.sh>
# Call this after computing the module's identity at the *call site* (top of the .sh file).
# This is the reliable Option B implementation for zsh + bash.
__load_guard() {
  local name="$1"
  [ -z "$name" ] && return 0
  local var="__LOADED_${name}__"

  if [ -n "${ZSH_VERSION:-}" ]; then
    if [ -n "${(P)var:-}" ]; then
      return 1
    fi
    typeset -g "$var"=1
  else
    if [ -n "${!var:-}" ]; then
      return 1
    fi
    eval "$var=1"
  fi
  return 0
}

# Backward-compat shim (old modules that still call source_guard without arg will get a warning + no-op)
source_guard() {
  echo "WARNING: source_guard() called without explicit module name (in $(basename "${BASH_SOURCE[1]:-${(%):-%N}}")). Update the module to use __load_guard." >&2
  return 0
}

# --- end of loader.sh ------------------------------------------------------