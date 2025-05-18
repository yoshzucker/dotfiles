# --- ~/.zprofile ------------------------------------------------------------

# Source pXX scripts
for f in ~/.config/lib/p*.sh; do
  [ -f "$f" ] && source "$f"
done

# --- .zprofile ends here ----------------------------------------------------
