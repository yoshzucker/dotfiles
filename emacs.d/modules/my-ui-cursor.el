;;; my-ui-cursor.el --- Cursor appearance and behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up cursor appearance, scrolling behavior, mouse support,
;; and terminal cursor shape using ANSI escape sequences.

;;; Code:

;; Cursor appearance
(setq-default cursor-in-non-selected-windows nil)
(setq blink-cursor-blinks 1)

;; Scroll behavior
(setq scroll-conservatively 5
      scroll-margin 0)

;; Mouse support
(when (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode 1))
(when (and (not (display-graphic-p)) (fboundp 'xterm-mouse-mode))
  (xterm-mouse-mode 1))

;; Terminal-specific cursor settings
(unless (display-graphic-p)

  ;; Cursor shape escape sequences
  (defconst my/cursor-shape-table
    '((iterm . ((line  . "\e]50;CursorShape=1\x7")
                (block . "\e]50;CursorShape=0\x7")))
      (ansi  . ((line  . "\e[5 q")
                (block . "\e[1 q")))))

  ;; Tmux escape sequence wrapping
  (defconst my/tmux-prefix "\ePtmux;\e")
  (defconst my/tmux-suffix "\e\\")

  (defun my/escape-seq (shape)
    "Return appropriate escape sequence string for cursor SHAPE (:line or :block)."
    (let* ((term (if (equal (getenv "TERM_PROGRAM") "iTerm.app") 'iterm 'ansi))
           (seq  (cdr (assoc shape (alist-get term my/cursor-shape-table))))
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
