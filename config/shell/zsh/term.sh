# --- term.sh --------------------------------------------------------------
# Terminal window operations — mirrors evil-window-map bindings in Emacs.
#   ^W e  cycle frame size   (my/cycle-frame-size  in my-ui-frame.el)
#   ^W m  toggle maximize    (toggle-frame-maximized in my-ui-frame.el)
#
# Outside tmux: ZLE bindings below handle ^We / ^Wm directly.
# Inside  tmux: C-w is captured by tmux's bind -n C-w and switches to the
#               prefix key table; matching `bind e` / `bind m` in tmux.conf
#               write the sequences to #{client_tty} instead.
#
# Note: terminal resize/maximize have no ANSI OSC standard. xterm XTWINOPS
# (CSI … t) is the de facto equivalent, supported by ghostty (macOS) and
# mintty (Windows).

[ -n "$ZSH_VERSION" ] || return 0

# Send a CSI sequence to the outer terminal.
# Outside tmux: printf directly (stdout = outer terminal in ZLE context).
# Inside  tmux: wrap in DCS passthrough so tmux forwards to the outer terminal.
#   tmux's bind e/m inject raw bytes via send-keys -H, which triggers these
#   ZLE widgets from within the tmux pane; DCS carries the sequence outward.
_term_send() {
  if [[ -n $TMUX ]]; then
    # ESC P tmux ; ESC <seq> ESC backslash — doubles the leading ESC in $1.
    printf '\033Ptmux;\033%s\033\\' "$1"
  else
    printf '%s' "$1"
  fi
}

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
  _term_send $'\e[8;'"${rows[i]};${cols[i]}t"
}

zle -N _cycle_term_size
# ^W is backward-kill-word; ^We is unambiguous: zsh waits KEYTIMEOUT (400 ms)
# after ^W and fires _cycle_term_size if 'e' follows, otherwise falls back.
bindkey '^We' _cycle_term_size

# ----- maximize toggle (^W m) -----

typeset -gi _term_maximized=0

_toggle_term_maximize() {
  if (( _term_maximized )); then
    _term_maximized=0
    _term_send $'\e[9;0t'   # CSI 9;0 t — restore window
  else
    _term_maximized=1
    _term_send $'\e[9;1t'   # CSI 9;1 t — maximize window
  fi
}

zle -N _toggle_term_maximize
bindkey '^Wm' _toggle_term_maximize

# --- end of term.sh -------------------------------------------------------
