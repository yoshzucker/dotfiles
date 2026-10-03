# --- ~/.zshenv (UL dotfiles) -----------------------------------------------
# Sourced for *every* zsh invocation (interactive, non-interactive, login, etc.).
# Keep this file minimal: only early env, PATH, and exports.
#
# Modules are sourced explicitly (not via glob) so load order is clear and
# adding a new file requires a deliberate edit here.

# Performance profiling (optional)
# zmodload zsh/zprof && zprof

# No system startup file is read after this one.  Each of them starts
# processes -- path_helper and `locale' in the Mac's /etc/zprofile and
# /etc/zshrc; `hostname', `uname' and a subshell per glob in the
# /etc/profile that MSYS2's /etc/zsh/zprofile sources -- and both login
# profiles rebuild PATH with the system directories in front of everything
# set here, which leaves a login shell and a shell inside tmux running
# different `git's.  What those files contribute that is still wanted is
# set by env/macos.sh, env/msys.sh and zsh/zsh.sh instead, with builtins
# only.
unsetopt global_rcs

# One copy of each directory: the modules prepend, and a shell started from
# a shell already has everything once.  The first copy is the one kept, so
# prepending an entry that is already there moves it to the front.
typeset -U path fpath

# Source early environment modules (non-interactive safe)
[ -f ~/.config/shell/env/common.sh ] && source ~/.config/shell/env/common.sh
[ -f ~/.config/shell/env/macos.sh  ] && source ~/.config/shell/env/macos.sh
[ -f ~/.config/shell/env/msys.sh   ] && source ~/.config/shell/env/msys.sh

# Whatever this machine needs and no other does -- a proxy, a credential, a
# path that exists only here.  Untracked on purpose: it is a real file beside
# the symlinks the deploy makes, so it is outside the repository and cannot
# be committed by accident.  Last, so it can override anything above it.
[ -f ~/.config/shell/env/local.sh  ] && source ~/.config/shell/env/local.sh

# --- end ~/.zshenv ---------------------------------------------------------
