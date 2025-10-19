;;; my-misc-workspace.el --- Workspace launcher (tabs & panes) -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a one-shot workspace: open a named tab and arrange Dayflow, Agenda, and Deft.

;;; Code
(my/define-key (:map global-map :key "C-c n w" #'my/create-workspace))

(defvar my/workspace-tab-name "*workspace*"
  "Tab name to host the workspace tools.")

(defun my/create-workspace ()
  "Switch to the workspace tab and show Dayflow (bottom), Agenda, and Deft (left)."
  (interactive)
  (my/cycle-frame-size 2)

  (tab-bar-mode 1)
  (my/tab-close my/workspace-tab-name)
  (my/tab-switch-or-create my/workspace-tab-name)

  ;; Dayflow (below selected), scoped via let-bound display rules
  (when (require 'org-dayflow nil 'noerror)
    (let ((display-buffer-alist
           (append
            '(("\\*Org Dayflow.*\\*"
               (display-buffer-below-selected)
               (inhibit-same-window . t)
               (window-height . 10)
               (window-parameters . ((no-delete-other-windows . t)
                                     (window-size-fixed . height)))))
            display-buffer-alist))
          (org-dayflow-high-density t))
      (org-dayflow-display 'day)
      (when-let* ((buf (seq-find (lambda (b)
                                   (string-match-p "\\*Org Dayflow.*\\*" (buffer-name b)))
                                 (buffer-list)))
                  (win (get-buffer-window buf t)))
        (set-window-parameter win 'no-delete-other-windows t)
        (set-window-parameter win 'window-size-fixed 'height)
        (set-window-dedicated-p win t))))

  ;; Agenda (no frame reorg)
  (when (require 'org-agenda nil 'noerror)
    (let ((org-agenda-window-setup 'other-window)
          (org-agenda-restore-windows-after-quit t))
      (org-agenda-list)))

  ;; Deft (left sidebar)
  (when (require 'deft nil 'noerror)
    (my/deft-sidebar-open)))

(provide 'my-misc-workspace)
;;; my-misc-workspace.el ends here
