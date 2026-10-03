# --- macos.sh ----------------------------------------------------------------
# macOS-specific: Homebrew environment, GNU coreutils/findutils PATH, and the
# system directories path_helper would have added.
# No-op on non-Darwin systems.  zsh only: sourced by ~/.zshenv.
# Exports: HOMEBREW_PREFIX, HOMEBREW_CELLAR, HOMEBREW_REPOSITORY, INFOPATH,
#          HOMEBREW_CURLRC (when ~/.curlrc exists), HOMEBREW_NO_ENV_HINTS
# Modifies: PATH (Homebrew bin, gnubin, /etc/paths), FPATH, MANPATH

# `$OSTYPE' rather than `uname': the shell already knows.  Asking the system
# means starting a process, which is a fifth of a second on the Windows
# machine -- spent by every shell that starts, only to learn that it is not a
# Mac.
case "$OSTYPE" in
  darwin*) ;;
  *) return 0 ;;
esac

# What `brew shellenv' prints, written out.  brew is a bash program, and
# running it to learn these takes 15 ms -- in every zsh, including each
# command Claude Code or Emacs hands to a shell.  The values depend only on
# where Homebrew is installed; compare with `brew shellenv' after a Homebrew
# upgrade that changes it.
if [ -d /opt/homebrew ]; then
  export HOMEBREW_PREFIX="/opt/homebrew"
  export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
  export HOMEBREW_REPOSITORY="/opt/homebrew"
  fpath[1,0]="/opt/homebrew/share/zsh/site-functions"
  export FPATH
  export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}"
  [ -z "${MANPATH-}" ] || { export MANPATH="${MANPATH%"${MANPATH##*[!:]}"}"; export MANPATH=":${MANPATH#"${MANPATH%%[!:]*}"}"; }
  export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"

  [ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" ] &&
    PATH="$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
  [ -d "$HOMEBREW_PREFIX/opt/findutils/libexec/gnubin" ] &&
    PATH="$HOMEBREW_PREFIX/opt/findutils/libexec/gnubin:$PATH"
fi

[ -e "$HOME/.curlrc" ] && export HOMEBREW_CURLRC=1

# Silence the "Adjust how often ... / Hide these hints with ..." footer that
# brew prints after auto-update. Auto-update itself stays enabled.
export HOMEBREW_NO_ENV_HINTS=1

# The directories /etc/zprofile's path_helper would add -- /usr/local/bin,
# the cryptexes, whatever an installer dropped into /etc/paths.d -- read
# here because ~/.zshenv skips that file.  Appended, where path_helper puts
# them first.  `$(<file)' reads without starting anything.
for __f in /etc/paths /etc/paths.d/*(N); do
  path+=(${(f)"$(<$__f)"})
done
unset __f
export PATH

# --- end of macos.sh ---------------------------------------------------------
