;;; my-ui-tabs.el --- Tab-bar configuration and integration with Evil -*- lexical-binding: t -*-
;;; Commentary:
;; Configures Emacs tab-bar UI and integrates it with Evil keybindings and commands.
;; Provides commands for tab creation, navigation, and buffer opening in new tabs.

;;; Code:
(require 'cl-lib)

(use-package tab-bar
  :after evil
  :config
  (tab-bar-mode 1)

  (my/define-key
   (:map evil-motion-state-map
         :key
         "gt" #'my/evil-tab-next
         "gT" #'my/evil-tab-previous)
   (:map evil-window-map
         :key
         "gf" #'my/evil-tab-find-file-at-point
         "gF" #'my/evil-tab-find-file-at-point-with-line))

  (evil-define-command my/evil-tab-new (&optional count)
    "Open COUNT new tabs. Defaults to 1 if COUNT is nil."
    :repeat nil
    (interactive "P")
    (let ((n (or count 1)))
      (unless tab-bar-mode
	    (tab-bar-mode 1))
      (dotimes (_ n)
	    (tab-new))))

  (evil-define-command my/evil-tab-edit (file)
    :repeat nil
    (interactive "<f>")
    (my/evil-tab-new)
    (evil-edit file))

  (evil-define-command my/evil-tab-find-file-at-point (&optional count)
    :repeat nil
    (let ((buffer (current-buffer)))
      (my/evil-tab-new)
      (switch-to-buffer buffer))
    (find-file-at-point))

  (evil-define-command my/evil-tab-find-file-at-point-with-line (&optional count)
    :repeat nil
    (let ((buffer (current-buffer)))
      (my/evil-tab-new)
      (switch-to-buffer buffer))
    (evil-find-file-at-point-with-line))

  (evil-define-command my/evil-tab-close (&optional count)
    :repeat nil
    (interactive "P")
    (let ((buffer (current-buffer)))
      (tab-close count)
      (switch-to-buffer buffer)))

  (evil-define-command my/evil-tab-only (&optional count)
    :repeat nil
    (interactive "P")
    (let ((buffer (current-buffer)))
      (tab-close-other count)
      (switch-to-buffer buffer)))

  (evil-define-command my/evil-tab-next (&optional count)
    "Move COUNT tabs forward (defaults to 1)."
    :repeat nil
    (interactive "P")
    (tab-next (or count 1)))
  
  (evil-define-command my/evil-tab-previous (&optional count)
    "Move COUNT tabs backward (defaults to 1)."
    :repeat nil
    (interactive "P")
    (tab-previous (or count 1)))

  (evil-define-command my/evil-tab-first ()
    :repeat nil
    (tab-select 0))

  (evil-define-command my/evil-tab-last ()
    :repeat nil
    (tab-select (1- (length (funcall tab-bar-tabs-function)))))

  (defun my/evil-quit ()
    "Smart quit like Vim: close window, tab, buffer, or Emacs."
    (interactive)
    (cond
     ;; A window that can go on its own: just close this one.  Not a count
     ;; of windows, because side windows (sill's row, a sidebar) are counted
     ;; too, and the one window beside them is the frame's main window,
     ;; which `delete-window' refuses.  `frame' means deleting the whole
     ;; frame, which is not what closing a window should do.
     ((eq (window-deletable-p) t)
      (delete-window))
     ;; Multiple tabs: close current tab
     ((> (length (funcall tab-bar-tabs-function)) 1)
      (tab-close))
     ;; Kill buffer if more than one user buffer is open
     ((> (length (buffer-list)) 1)
      (kill-current-buffer))
     ;; Last resort: exit Emacs
     (t
      (save-buffers-kill-terminal))))

  (evil-define-command my/evil-write-quit ()
    "Save the buffer, then quit the way `my/evil-quit' does."
    :repeat nil
    (save-buffer)
    (my/evil-quit))

  (evil-ex-define-cmd "tabnew" #'my/evil-tab-new)
  (evil-ex-define-cmd "tabe[dit]" #'my/evil-tab-edit)
  (evil-ex-define-cmd "tabc[lose]" #'my/evil-tab-close)
  (evil-ex-define-cmd "tabo[nly]" #'my/evil-tab-only)
  (evil-ex-define-cmd "tabn[ext]" #'my/evil-tab-next)
  (evil-ex-define-cmd "tabp[revious]" #'my/evil-tab-previous)
  (evil-ex-define-cmd "tabN[ext]" #'my/evil-tab-previous)
  (evil-ex-define-cmd "tabfir[st]" #'my/evil-tab-first)
  (evil-ex-define-cmd "tabl[ast]" #'my/evil-tab-last)
  (evil-ex-define-cmd "q[uit]" #'my/evil-quit)
  (evil-ex-define-cmd "wq" #'my/evil-write-quit))

;; A new tab opens on *scratch*, a blank page, rather than on a second view
;; of the buffer it was opened from.
(setq tab-bar-new-tab-choice "*scratch*")

(provide 'my-ui-tabs)
;;; my-ui-tabs.el ends here
