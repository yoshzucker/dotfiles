# --- ~/.zshrc (UL dotfiles) ----------------------------------------------
# Interactive shells only. Early env/PATH already provided by .zshenv.
# Modules are sourced explicitly so load order is visible.

source ~/.config/shell/zsh/colors.sh   # theme, truecolor, OSC palette, THEME_MONO*
source ~/.config/shell/zsh/fzf.sh      # fzf widgets/opts (ZLE only, no compinit dep at source time)
source ~/.config/shell/zsh/zsh.sh      # compinit, prompt, keybinds (key section has final say)
source ~/.config/shell/zsh/aliases.sh  # eza/ls, emacs client, git, tool aliases
source ~/.config/shell/zsh/term.sh    # terminal window size cycling (C-w e)

# --- .zshrc ends here ----------------------------------------------------
# Do NOT append CLI installer blocks below this line. PATH belongs in
# ~/.config/shell/env/common.sh; completion fpath entries belong in
# ~/.config/shell/zsh/zsh.sh next to the existing grok fpath line.
