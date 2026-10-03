;;; my-files-persistence.el --- Session persistence and file tracking -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles session persistence and file tracking configuration.

;;; Code:

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package recentf
  :after no-littering
  :config
  (setq recentf-max-saved-items 1000
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

;; No lock files on Windows.
;;
;; A buffer takes a lock on its file at the first change after a save and
;; gives it up at the next save, and super-save makes that a cycle: every
;; window switch and every quiet spell ends one, so the next keystroke begins
;; another.  On macOS a lock is a symbolic link and costs nothing.  On Windows
;; it is an ordinary file created beside the one being edited -- in a synced
;; folder, as most of them are here, a new file for the antivirus to open and
;; the sync client to notice, paid for on the first keystroke after each save.
;;
;; What the lock buys is a warning when a second Emacs starts editing the
;; same file, and there is one Emacs here.  A change made on disk by anything
;; else is still caught when saving, which compares the file's time with the
;; one it was read at.
(when (eq system-type 'windows-nt)
  (setq create-lockfiles nil))

(provide 'my-files-persistence)
;;; my-files-persistence.el ends here
