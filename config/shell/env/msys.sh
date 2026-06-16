# --- msys.sh -----------------------------------------------------------------
# MSYS2/Mintty helpers: pp/wp (POSIX<->Windows path), open() command,
# Scoop shims appended to PATH.
# No-op on non-MSYS2 systems. Safe for bash and zsh.
# Defines: pp(), wp(), open() (open only when powershell.exe is available)
# Modifies: PATH (Scoop shims appended at low priority)

[ -n "${MSYSTEM:-}" ] || return 0

# pp: Windows path -> POSIX  (C:\foo\Bar -> /c/foo/Bar, \\srv\sh -> //srv/sh)
pp() {
  local t="${1:-.}" p drive rest
  case "$t" in
    [A-Za-z]:[\\/]*)
      p="$(printf '%s' "$t" | tr '\\' '/')"
      drive="${p%%:*}"
      rest="${p#*:}"
      drive="$(printf '%s' "$drive" | tr '[:upper:]' '[:lower:]')"
      p="/${drive}${rest}"
      ;;
    \\\\*)
      p="$(printf '%s' "$t" | tr '\\' '/')"
      ;;
    *)
      p="$(cd "$t" 2>/dev/null && pwd -P || printf '%s' "$t")"
      ;;
  esac
  printf '%s\n' "$p"
}

# wp: POSIX path -> Windows  (/c/foo/Bar -> C:\foo\Bar)
wp() {
  local t="${1:-.}" wpath ap dir base wdir
  case "$t" in
    [A-Za-z]:[\\/]* | \\\\*)
      printf '%s\n' "$t"
      return
      ;;
  esac
  if [ -d "$t" ]; then
    wpath="$(cd -- "$t" 2>/dev/null && pwd -W)"
  else
    ap="$(realpath -sm "$t" 2>/dev/null || printf '%s/%s' "$(pwd)" "$t")"
    dir="$(dirname "$ap")"
    base="$(basename "$ap")"
    wdir="$(cd -- "$dir" 2>/dev/null && pwd -W)"
    [ -n "$wdir" ] && wpath="${wdir}\\${base}"
  fi
  printf '%s\n' "$wpath"
}

if command -v powershell.exe >/dev/null 2>&1; then
  open() {
    local p
    p="$(wp "${1:-.}")"
    if [ -z "$p" ]; then
      printf 'open: not found or inaccessible: %s\n' "${1:-.}" >&2
      return 1
    fi
    powershell.exe -NoProfile -Command "Start-Process $p"
  }
fi

# Scoop shims. Appended (not prepended) so pacman/ucrt64 binaries take
# precedence when both exist (e.g. fzf), while scoop-only tools (e.g. claude)
# still resolve. pp converts USERPROFILE (Windows path) to POSIX form.
if [ -n "${USERPROFILE:-}" ]; then
  __scoop_shims="$(pp "$USERPROFILE")/scoop/shims"
  [ -d "$__scoop_shims" ] && PATH="$PATH:$__scoop_shims"
  unset __scoop_shims
fi
[ -d /c/ProgramData/scoop/shims ] && PATH="$PATH:/c/ProgramData/scoop/shims"
export PATH

# --- end of msys.sh ----------------------------------------------------------
