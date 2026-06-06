# --- 30-msys.sh ----------------------------------------------------------
# MSYS2 helpers: pp/wp (POSIX<->Windows path), open() command.
# Only activates under msys2 distribution.

# Guard (Option B, reliable): name captured at module top-level before any function
_module_name="$(basename "${BASH_SOURCE[0]:-${(%):-%N}}" .sh | tr -c "a-zA-Z0-9" "_")"
_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%N}}")" && pwd)"
[ -f "$_script_dir/../loader.sh" ] && source "$_script_dir/../loader.sh"
__load_guard "$_module_name" || return 0

# to POSIX (C:\foo\Bar → /c/foo/Bar, \\server\share → //server/share)
pp() {
  local t="${1:-.}" p drive rest
  if [[ "$t" == ([A-Za-z]:[\\/]|\\\\*)* ]]; then
    p="${t//\\//}"                     # \ → /
    if [[ "$p" == ([A-Za-z]):/* ]]; then
      drive="${p%%:*}"; rest="${p#*:}" # C:/...
      drive="${drive:l}"               # c
      p="/${drive}${rest}"             # -> /c/...
    else
      :
    fi
  else
    p="${t:A}"
  fi
  print -r -- "$p"
}

# to Windows (/c/foo/Bar → C:\foo\Bar)
wp() {
  local t="${1:-.}" wpath ap dir base wdir
  if [[ "$t" == ([A-Za-z]:[\\/]|\\\\*)* ]]; then
    print -r -- "$t"
    return
  fi
  if [[ -d "$t" ]]; then
    wpath="$(cd -- "$t" 2>/dev/null && pwd -W)"
  else
    ap="${t:A}"                     # POSIX
    dir="${ap:h}"                   # Parent
    base="${ap:t}"                  # Filename
    wdir="$(cd -- "$dir" 2>/dev/null && pwd -W)"
    [[ -n "$wdir" ]] && wpath="$wdir\\$base"
  fi
  print -r -- "$wpath"
}

if command -v powershell.exe >/dev/null 2>&1; then
  open() {
    local p; p="$(wp "${1:-.}")"
    if [[ -z "$p" ]]; then
      print -u2 -- "open: not found or inaccessible: ${1:-.}"
      return 1
    fi

    powershell.exe -NoProfile -Command "Start-Process $p"
  }
fi

# --- end of p05_core_msys.sh -------------------------------------------------
