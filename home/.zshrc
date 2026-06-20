# --- ~/.zshrc (UL dotfiles) ----------------------------------------------
# Interactive shells only. Early env/PATH already provided by .zshenv.
# Modules are sourced explicitly so load order is visible.

source ~/.config/shell/zsh/colors.sh   # theme, truecolor, OSC palette, THEME_MONO*
source ~/.config/shell/zsh/fzf.sh      # fzf widgets/opts (ZLE only, no compinit dep at source time)
source ~/.config/shell/zsh/zsh.sh      # compinit, prompt, keybinds (key section has final say)
source ~/.config/shell/zsh/aliases.sh  # eza/ls, emacs client, git, tool aliases

# --- tmux server pre-warm (daemon model) ---------------------------------
# tmux's first server start is slow on Windows (~4.6s) because Defender scans
# the cygwin fork/exec; once the server is up, new sessions attach in ~0.3s.
# So we pre-start a detached keepalive session in the background at login,
# like an emacs daemon, paying that cost off the critical path. Plain `tmux`
# then reuses the warm server instantly (no alias needed). The keepalive
# session keeps the server alive even after you exit your own sessions; close
# it explicitly with `tmux kill-session -t _daemon` (or kill-server) when done.
# Runs only in a real login terminal, after colors.sh exported THEME_MONO*
# (the status bar reads those at server-start time).
if [[ -o interactive && -o login && -z $TMUX ]] && command -v tmux >/dev/null 2>&1; then
  (tmux has-session -t _daemon 2>/dev/null || tmux new-session -d -s _daemon) &!
fi

# --- .zshrc ends here ----------------------------------------------------
