# --- ~/.zshrc ------------------------------------------------------------

# Source pXX scripts
for f in ~/.config/lib/p*.sh; do
  [ -f "$f" ] && source "$f"
done

# Source rXX scripts
for f in ~/.config/lib/z*.sh; do
  [ -f "$f" ] && source "$f"
done

# --- .zshrc ends here ----------------------------------------------------
