;;; my-files-persistence.el --- Session persistence and file tracking -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles session persistence and file tracking configuration.

;;; Code:

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package no-littering
  :config
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backup/")))
	make-backup-files t
	backup-by-copying t
	version-control t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2))

(use-package recentf
  :after no-littering
  :config
  (setq recentf-max-saved-items 300
	find-file-visit-truename nil))

(use-package recentf-ext
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package super-save
  :diminish (super-save-mode " ss")
  :config
  (setq auto-save-default nil
	super-save-auto-save-when-idle t
	super-save-idle-duration 15)
  (super-save-mode 1))

(provide 'my-files-persistence)
;;; my-files-persistence.el ends here
