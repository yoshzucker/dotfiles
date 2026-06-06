# --- ~/.zshrc (UL dotfiles) ----------------------------------------------
# Interactive shells only. Early env/PATH already provided by .zshenv.
# Modules are sourced explicitly so load order is visible.

source ~/.config/shell/zsh/colors.sh   # theme, truecolor, OSC palette, THEME_MONO*
source ~/.config/shell/zsh/zsh.sh      # history, setopt, compinit, prompt, keybinds
source ~/.config/shell/zsh/aliases.sh  # eza/ls, emacs client, git, tool aliases
source ~/.config/shell/zsh/fzf.sh      # fzf keybindings, completions, opts

# --- .zshrc ends here ----------------------------------------------------

# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
fpath=(~/.grok/completions/zsh $fpath)
# compinit is already done in zsh.sh (with -C and host-specific dump).
# Guard to avoid double work / conflicts.
if ! command -v compinit >/dev/null 2>&1 || [[ -z "${__GROK_COMPINIT_DONE:-}" ]]; then
  autoload -Uz compinit && compinit -C
  __GROK_COMPINIT_DONE=1
fi
# <<< grok installer <<<
