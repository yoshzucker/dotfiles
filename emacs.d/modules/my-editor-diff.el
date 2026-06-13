;;; my-editor-diff.el --- Diff and ediff configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Ediff with plain (single-frame) window setup and inline VC change
;; indicators via diff-hl, integrated with Magit and dired.

;;; Code:

(use-package ediff
  :straight nil
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally)

  (setq ediff-keep-variants nil
        ediff-diff-options "-w"
        ediff-highlight-all-diffs nil)

  ;; ediff splits/destroys windows; capture the pre-session layout and
  ;; restore it on quit so the user lands back where they started.
  (defvar my/ediff-window-config nil
    "Window configuration saved before an ediff session starts.")

  (defun my/ediff-save-window-config ()
    (setq my/ediff-window-config (current-window-configuration)))

  (defun my/ediff-restore-window-config ()
    (when my/ediff-window-config
      (set-window-configuration my/ediff-window-config)
      (setq my/ediff-window-config nil)))

  (my/add-hook
   (:hook ediff-before-setup-hook
          :func #'my/ediff-save-window-config)
   (:hook ediff-quit-hook
          :func #'my/ediff-restore-window-config)))

;; evil-collection ships an ediff integration that is enabled automatically
;; by (evil-collection-init) in my-editor-evil.el, so no extra binding is
;; required here.

(use-package diff-hl
  :after magit
  :hook ((prog-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (diff-hl-flydiff-mode 1)

  (my/define-key
   (:map diff-hl-mode-map
         :state normal
         :key
         "]c" #'diff-hl-next-hunk
         "[c" #'diff-hl-previous-hunk)))

(provide 'my-editor-diff)
;;; my-editor-diff.el ends here
