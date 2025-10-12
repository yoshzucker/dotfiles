# --- p05_core_msys.sh --------------------------------------------------------
# POSIX <-> Windows path helpers and "open" (Windows only)
# deps: zsh, MSYS2 (pwd -W)

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

if command -v cmd.exe >/dev/null 2>&1 && [[ "${DISTRIBUTION:-}" == "msys2" ]]; then
  open() {
    local p; p="$(wp "${1:-.}")"
    if [[ -z "$p" ]]; then
      print -u2 -- "open: not found or inaccessible: ${1:-.}"
      return 1
    fi
    cmd.exe /C start "" "$p" >/dev/null 2>&1
  }
fi

# --- end of p05_core_msys.sh -------------------------------------------------
