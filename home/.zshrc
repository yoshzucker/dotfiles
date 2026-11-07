# --- ~/.zshrc (UL dotfiles) ----------------------------------------------
# Interactive shells only. Early env/PATH already provided by .zshenv.
# Modules are sourced explicitly so load order is visible.

source ~/.config/shell/colors.sh       # truecolor, terminfo (shared with bash)
source ~/.config/shell/zsh/zsh.sh      # compinit, prompt, keybinds (key section has final say)
source ~/.config/shell/aliases.sh      # emacs client, git, tool aliases (shared with bash)
source ~/.config/shell/zsh/term.sh    # terminal window size cycling (C-w e)

# --- .zshrc ends here ----------------------------------------------------
# Do NOT append CLI installer blocks below this line. PATH belongs in
# ~/.config/shell/env/common.sh; completion fpath entries belong in
# ~/.config/shell/zsh/zsh.sh next to the existing grok fpath line.
