;;; my-app-misc.el --- Miscellaneous App integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; This file handles integrations with external applications like Obsidian.

;;; Code:

(use-package obsidian
  :if (eq system-type 'darwin)
  :init
  (setq obsidian-directory
        (expand-file-name "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/complexvault/"))
  :config
  (my/define-key
   (:map obsidian-mode-map
         :prefix "C-c"
         :key
         "C-n" #'obsidian-capture
         "C-l" #'obsidian-insert-link
         "C-o" #'obsidian-follow-link-at-point
         "C-p" #'obsidian-jump
         "C-n" #'obsidian-jump-back
         "C-b" #'obsidian-backlink-jump))

  (global-obsidian-mode 1))

(provide 'my-app-misc)
;;; my-app-misc.el ends here
