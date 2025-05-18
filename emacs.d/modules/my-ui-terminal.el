;;; my-ui-terminal.el --- UI-level terminal integration for pane movement -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhances terminal-based navigation by integrating Emacs with tmux panes
;; and overriding evil window movement commands.

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

(provide 'my-ui-terminal)
;;; my-ui-terminal.el ends here
