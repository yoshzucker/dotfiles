;;; my-ui-tabs.el --- Tab-bar configuration and integration with Evil -*- lexical-binding: t -*-
;;; Commentary:
;; Configures Emacs tab-bar UI and integrates it with Evil keybindings and commands.
;; Provides commands for tab creation, navigation, and buffer opening in new tabs.

;;; Code:

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
     ;; Multiple windows: just close this one
     ((> (count-windows) 1)
      (delete-window))
     ;; Multiple tabs: close current tab
     ((> (length (funcall tab-bar-tabs-function)) 1)
      (tab-close))
     ;; Kill buffer if more than one user buffer is open
     ((> (length (buffer-list)) 1)
      (kill-this-buffer))
     ;; Last resort: exit Emacs
     (t
      (save-buffers-kill-terminal))))

  (evil-ex-define-cmd "tabnew" #'my/evil-tab-new)
  (evil-ex-define-cmd "tabe[dit]" #'my/evil-tab-edit)
  (evil-ex-define-cmd "tabc[lose]" #'my/evil-tab-close)
  (evil-ex-define-cmd "tabo[nly]" #'my/evil-tab-only)
  (evil-ex-define-cmd "tabn[ext]" #'my/evil-tab-next)
  (evil-ex-define-cmd "tabp[revious]" #'my/evil-tab-previous)
  (evil-ex-define-cmd "tabN[ext]" #'my/evil-tab-previous)
  (evil-ex-define-cmd "tabfir[st]" #'my/evil-tab-first)
  (evil-ex-define-cmd "tabl[ast]" #'my/evil-tab-last)
  (evil-ex-define-cmd "q[uit]" #'my/evil-quit))

(use-package tabspaces
  :after consult
  :config
  (setq tabspaces-include-buffers '("*GNU Emacs*" "*scratch*" "*Messages*"))

  (consult-customize consult--source-buffer :hidden t :default nil)
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace)

  (defun my--consult-tabspaces ()
    "Deactivate isolated buffers when not using tabspaces."
    (cond (tabspaces-mode
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
          (t
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

  (my/add-hook
   (:hook after-init-hook :func #'tabspaces-mode)
   (:hook tabspaces-mode-hook :func #'my--consult-tabspaces))

  (defun my/switch-scratch-tab-new-around (orig-fn &rest args)
    "Switch to *scratch* buffer for the new tab if `tabspaces-mode' is active."
    (if (and tabspaces-mode (get-buffer "*scratch*"))
        (let ((tab-bar-new-tab-choice "*scratch*"))
          (apply orig-fn args))
      (apply orig-fn args)))

  (advice-add 'my/evil-tab-new :around #'my/switch-scratch-tab-new-around))

(provide 'my-ui-tabs)
;;; my-ui-tabs.el ends here
