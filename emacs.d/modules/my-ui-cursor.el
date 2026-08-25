;;; my-ui-cursor.el --- Cursor appearance and behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Cursor appearance and scrolling, and -- in a terminal -- the escape
;; sequence that makes the shape follow evil's state.

;;; Code:

;; Cursor appearance
(setq-default cursor-in-non-selected-windows nil)
(setq blink-cursor-blinks 1)

;; Scroll behavior
(setq scroll-conservatively 5
      scroll-margin 1)

;; Terminal-specific cursor settings
(unless (display-graphic-p)

  ;; DECSCUSR, which is what every terminal used here answers to -- mintty on
  ;; Windows, ghostty on macOS.  A terminal that wanted its own sequence would
  ;; need a table; none of them do.
  (defconst my/cursor-shapes
    '((line  . "\e[5 q")
      (block . "\e[1 q")))

  ;; Inside tmux a sequence meant for the terminal has to be handed through the
  ;; pane, or tmux reads it as its own.
  (defconst my/tmux-prefix "\ePtmux;\e")
  (defconst my/tmux-suffix "\e\\")

  (defun my/escape-seq (shape)
    "Return the escape sequence for cursor SHAPE, wrapped when inside tmux."
    (let ((seq (alist-get shape my/cursor-shapes))
          (tmux (getenv "TMUX")))
      (concat (if tmux my/tmux-prefix "")
              seq
              (if tmux my/tmux-suffix ""))))

  (defun my/set-cursor-line ()
    (send-string-to-terminal (my/escape-seq 'line)))

  (defun my/set-cursor-block ()
    (send-string-to-terminal (my/escape-seq 'block)))

  ;; Hook into Evil insert state transitions
  (my/add-hook
   (:hook evil-insert-state-entry-hook :func #'my/set-cursor-line)
   (:hook evil-insert-state-exit-hook :func #'my/set-cursor-block)))

(provide 'my-ui-cursor)
;;; my-ui-cursor.el ends here
