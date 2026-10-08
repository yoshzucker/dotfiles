# --- msys.sh -----------------------------------------------------------------
# MSYS2: the part of /etc/profile a UCRT64 shell needs, pp/wp (POSIX<->Windows
# path), Scoop shims appended to PATH.
# No-op on non-MSYS2 systems.  zsh only: sourced by ~/.zshenv.
# Exports: MSYSTEM (UCRT64 unless already set) and what /etc/msystem exports
#          (MSYSTEM_PREFIX, MINGW_PREFIX, ...), SHELL, LANG (when no locale
#          variable is set)
# Defines: pp(), wp()
# Modifies: PATH (MSYS2 directories prepended, Scoop shims appended)

# `$OSTYPE', not `$MSYSTEM': the latter is what this file sets, for a shell
# started without it.  MSYS2's zsh is built as a Cygwin program and says
# `cygwin'; its bash says `msys'.
case "$OSTYPE" in
  msys*|cygwin*) ;;
  *) return 0 ;;
esac

# What /etc/profile does for a UCRT64 zsh, without starting anything.
# ~/.zshenv skips that file: on every login it runs `hostname' and `uname'
# and forks a subshell for each of five globs and for `which zsh' -- about
# nine processes, at a fifth of a second each, before the first prompt.
#
# MSYSTEM defaults here, so a launcher needs only to start zsh.  /etc/msystem
# is MSYS2's own table for it and only assigns.  The MSYS2 directories go in
# front of what common.sh put first and of the inherited Windows PATH, which
# is where /etc/profile put them with MSYS2_PATH_TYPE=inherit: pacman's
# binaries still win where both have one.  SHELL, because a shell started
# from Emacs inherits its cmdproxy.exe, and fzf previews and tmux run what it
# names.  LANG as /etc/profile.d/lang.sh set it, for a launcher that sets no
# locale at all.
#
# Left out as nothing here uses them: HOSTNAME, TMP/TEMP pointed at /tmp,
# the build variables (PKG_CONFIG_*, ACLOCAL_PATH, CONFIG_SITE), MANPATH and
# INFOPATH (man finds pages from PATH), XDG_DATA_DIRS (bash-completion), and
# the post-install scripts, which do their work on the first start only.
export MSYSTEM="${MSYSTEM:-UCRT64}"
. /etc/msystem
PATH="${MINGW_PREFIX:+$MINGW_PREFIX/bin:}/usr/local/bin:/usr/bin:/bin:$PATH"
export SHELL=/usr/bin/zsh
[ -n "${LC_ALL:-${LC_CTYPE:-$LANG}}" ] || export LANG=ja_JP.UTF-8

# pp: Windows path -> POSIX  (C:\foo\Bar -> /c/foo/Bar, \\srv\sh -> //srv/sh)
pp() {
  local t="${1:-.}" p drive rest
  case "$t" in
    [A-Za-z]:[\\/]*)
      p="$(printf '%s' "$t" | /usr/bin/tr '\\' '/')"
      drive="${p%%:*}"
      rest="${p#*:}"
      drive="$(printf '%s' "$drive" | /usr/bin/tr '[:upper:]' '[:lower:]')"
      p="/${drive}${rest}"
      ;;
    \\\\*)
      p="$(printf '%s' "$t" | /usr/bin/tr '\\' '/')"
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

# Scoop shims. Appended (not prepended) so pacman/ucrt64 binaries take
# precedence when both exist (e.g. fzf), while scoop-only tools (e.g. claude)
# still resolve.
#
# $HOME is already in POSIX form and normally names the same directory as
# USERPROFILE, so the common case needs no conversion at all.  pp would
# spend two `tr' processes on it, and starting one costs a fifth of a second
# here -- paid by every shell that starts, before the prompt appears.  The
# conversion stays as the fallback for a HOME that points elsewhere.
if [ -d "$HOME/scoop/shims" ]; then
  PATH="$PATH:$HOME/scoop/shims"
elif [ -n "${USERPROFILE:-}" ]; then
  __scoop_shims="$(pp "$USERPROFILE")/scoop/shims"
  [ -d "$__scoop_shims" ] && PATH="$PATH:$__scoop_shims"
  unset __scoop_shims
fi
[ -d /c/ProgramData/scoop/shims ] && PATH="$PATH:/c/ProgramData/scoop/shims"
export PATH

# --- end of msys.sh ----------------------------------------------------------
