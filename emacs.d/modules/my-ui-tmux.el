;;; my-ui-tmux.el --- Window movement across the tmux boundary -*- lexical-binding: t; -*-

;;; Commentary:
;; What Emacs needs when it is the guest: running in a tmux pane, inside a
;; terminal.  `tmux-pane' carries evil's window motions past the edge of
;; Emacs into the neighbouring panes, so one set of keys moves through both.
;; It is gated on there being no display, because crossing that edge only
;; means anything when Emacs is in a pane itself; `emamux', which sends
;; commands the other way into a pane, works from a graphical Emacs too.
;;
;; The terminals that run the other way round, inside Emacs, are in
;; my-app-terminal.el.

;;; Code:

(use-package emamux
  :defer t
  :config
  (setq emamux:use-nearest-pane t))

;; tmux and evil integration
(use-package tmux-pane
  :after evil
  :if (not (display-graphic-p))
  :config
  (evil-define-command my/evil-omni-window-left (count)
    "Move the cursor to new COUNT-th window left of the current one."
    :repeat nil
    (interactive "p")
    (dotimes (_ count)
      (tmux-pane-omni-window-left)))
  
  (evil-define-command my/evil-omni-window-right (count)
    "Move the cursor to new COUNT-th window right of the current one."
    :repeat nil
    (interactive "p")
    (dotimes (_ count)
      (tmux-pane-omni-window-right)))
  
  (evil-define-command my/evil-omni-window-up (count)
    "Move the cursor to new COUNT-th window above the current one."
    :repeat nil
    (interactive "p")
    (dotimes (_ (or count 1))
      (tmux-pane-omni-window-up)))
  
  (evil-define-command my/evil-omni-window-down (count)
    "Move the cursor to new COUNT-th window below the current one."
    :repeat nil
    (interactive "p")
    (dotimes (_ (or count 1))
      (tmux-pane-omni-window-down)))

  (advice-add 'evil-window-left  :override #'my/evil-omni-window-left)
  (advice-add 'evil-window-right :override #'my/evil-omni-window-right)
  (advice-add 'evil-window-down  :override #'my/evil-omni-window-down)
  (advice-add 'evil-window-up    :override #'my/evil-omni-window-up))

(provide 'my-ui-tmux)
;;; my-ui-tmux.el ends here
