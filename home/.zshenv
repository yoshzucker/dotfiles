# --- ~/.zshenv (UL dotfiles) -----------------------------------------------
# Sourced for *every* zsh invocation (interactive, non-interactive, login, etc.).
# Keep this file minimal: only early env, PATH, exports, and sourcing of
# config/shell/env/*.sh (core detection, brew, path, tz, msys, wsl).

# Performance profiling (optional)
# zmodload zsh/zprof && zprof

# Prevent redundant global compinit (e.g. on Ubuntu)
skip_global_compinit=1

# Source early environment modules (cross-shell, non-interactive safe)
for f in ~/.config/shell/env/*.sh; do
  [ -f "$f" ] && source "$f"
done

# --- end ~/.zshenv ---------------------------------------------------------
