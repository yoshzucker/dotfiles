# --- ~/.zshrc (UL dotfiles) ----------------------------------------------
# Interactive shells only. Early env/PATH/THEME already provided by .zshenv.

# Source zsh interactive modules only (UI, prompt, aliases, fzf, ...)
for f in ~/.config/shell/zsh/*.sh; do
  [ -f "$f" ] && source "$f"
done

# --- .zshrc ends here ----------------------------------------------------

# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
fpath=(~/.grok/completions/zsh $fpath)
# compinit is already done in 30-zsh-ux.sh (with -C and host-specific dump).
# Guard to avoid double work / conflicts.
if ! command -v compinit >/dev/null 2>&1 || [[ -z "${__GROK_COMPINIT_DONE:-}" ]]; then
  autoload -Uz compinit && compinit -C
  __GROK_COMPINIT_DONE=1
fi
# <<< grok installer <<<
