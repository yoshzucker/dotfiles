# --- ~/.zshenv (UL dotfiles) -----------------------------------------------
# Sourced for *every* zsh invocation (interactive, non-interactive, login, etc.).
# Keep this file minimal: only early env, PATH, and exports.
#
# Modules are sourced explicitly (not via glob) so load order is clear and
# adding a new file requires a deliberate edit here.

# Performance profiling (optional)
# zmodload zsh/zprof && zprof

# Source early environment modules (cross-shell, non-interactive safe)
[ -f ~/.config/shell/env/common.sh ] && source ~/.config/shell/env/common.sh
[ -f ~/.config/shell/env/macos.sh  ] && source ~/.config/shell/env/macos.sh
[ -f ~/.config/shell/env/msys.sh   ] && source ~/.config/shell/env/msys.sh

# Whatever this machine needs and no other does -- a proxy, a credential, a
# path that exists only here.  Untracked on purpose: it is a real file beside
# the symlinks the deploy makes, so it is outside the repository and cannot
# be committed by accident.  Last, so it can override anything above it.
[ -f ~/.config/shell/env/local.sh  ] && source ~/.config/shell/env/local.sh

# --- end ~/.zshenv ---------------------------------------------------------
