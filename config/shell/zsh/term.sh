# --- term.sh --------------------------------------------------------------
# Terminal window operations — mirrors evil-window-map bindings in Emacs.
#   ^W e  cycle frame size   (my/cycle-frame-size  in my-ui-frame.el)
#   ^W m  toggle maximize    (toggle-frame-maximized in my-ui-frame.el)
#
# Note: terminal resize/maximize have no ANSI OSC standard. xterm XTWINOPS
# (CSI … t) is the de facto equivalent, supported by ghostty (macOS) and
# mintty (Windows).

[ -n "$ZSH_VERSION" ] || return 0

# ----- size cycling (^W e) -----

# 0-based index into the size list; advanced before each resize so state is
# updated immediately — no dependency on SIGWINCH propagating $COLUMNS/$LINES.
typeset -gi _term_size_idx=0

_cycle_term_size() {
  # Width×height pairs mirroring my/frame-size-list: (81 . 24) (81 . 34) (163 . 34)
  local -a cols=(81  81  163)
  local -a rows=(24  34  34)
  _term_size_idx=$(( (_term_size_idx + 1) % ${#cols[@]} ))
  # zsh arrays are 1-indexed; _term_size_idx is 0-based, so offset by 1.
  local i=$(( _term_size_idx + 1 ))
  # CSI 8 ; rows ; cols t — xterm XTWINOPS: resize window in character cells.
  printf '%s' $'\e[8;'"${rows[i]};${cols[i]}t"
}

# ^W is a prefix and nothing else, as C-w is in Emacs (evil-window-map, in
# every state).  Bound to backward-kill-word as well, it would make zsh wait
# KEYTIMEOUT -- 400 ms -- after every ^W to see whether an `e' or `m'
# followed; unbound, there is no timeout and the second key acts at once.
# Words are deleted with Alt-Backspace, as M-DEL does in Emacs.
bindkey -r '^W'

zle -N _cycle_term_size
bindkey '^We' _cycle_term_size

# ----- maximize toggle (^W m) -----

typeset -gi _term_maximized=0

_toggle_term_maximize() {
  if (( _term_maximized )); then
    _term_maximized=0
    printf '%s' $'\e[9;0t'   # CSI 9;0 t — restore window
  else
    _term_maximized=1
    printf '%s' $'\e[9;1t'   # CSI 9;1 t — maximize window
  fi
}

zle -N _toggle_term_maximize
bindkey '^Wm' _toggle_term_maximize

# --- end of term.sh -------------------------------------------------------
