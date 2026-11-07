;;; my-files-persistence.el --- Session persistence and file tracking -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles session persistence and file tracking configuration.

;;; Code:

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package recentf
  :straight nil
  :after no-littering
  :config
  (setq recentf-max-saved-items 1000
	find-file-visit-truename nil)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Recent means seen, not only opened: a file brought back into a window
  ;; moves to the top again.
  (defun my/recentf-push-visible-files ()
    "Add the file of every window in this frame to `recentf-list'."
    (walk-windows
     (lambda (win)
       (when-let* ((file (buffer-local-value 'buffer-file-name (window-buffer win))))
         (recentf-add-file file)))))
  (add-hook 'window-configuration-change-hook #'my/recentf-push-visible-files)

  ;; Directories opened in dired count too, stored with the trailing slash
  ;; dired gives them -- which is what tells a directory from a file when
  ;; the list is sorted (`my/recentf-sort-key').
  (defun my/recentf-add-dired-directory ()
    "Add this dired buffer's directory to `recentf-list'."
    (when (and (stringp dired-directory)
               (string-empty-p (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook #'my/recentf-add-dired-directory)

  (recentf-mode 1))

(use-package files
  :straight nil
  :config
  ;; Saved to the file itself when Emacs is left alone for fifteen seconds,
  ;; and not when a window is switched or the focus leaves.
  ;;
  ;; A save is more than the write.  `after-save-hook' formats the buffer,
  ;; reindexes an Org file into org-roam, and the
  ;; formatting alone holds a two-thousand-line Emacs Lisp file for fifty
  ;; to ninety milliseconds while it reindents.  Saving on a switch put all
  ;; of that between the key and the other window appearing; saving when
  ;; idle puts it where nobody is waiting.  What is given up is the file on
  ;; disk being current the moment the window changes: for up to fifteen
  ;; seconds afterwards it holds what was last saved.
  ;; No separate #auto-save# files beside them: the file itself is current.
  (setq auto-save-default nil
        auto-save-visited-interval 15)
  (auto-save-visited-mode 1))

;; No lock files on Windows.
;;
;; A buffer takes a lock on its file at the first change after a save and
;; gives it up at the next save, and saving when idle makes that a cycle:
;; every quiet spell ends one, so the next keystroke begins another.  On
;; macOS a lock is a symbolic link and costs nothing.  On Windows
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
