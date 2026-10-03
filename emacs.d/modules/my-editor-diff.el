;;; my-editor-diff.el --- Diff and ediff configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Ediff with plain (single-frame) window setup and inline VC change
;; indicators via diff-hl, integrated with Magit and dired -- the indicators
;; everywhere but Windows.

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
  ;; Not on Windows.  What diff-hl draws is the answer to git, asked by
  ;; starting git and waiting for it: whether the file is tracked and how it
  ;; differs, when a file is opened and again when it is saved -- and saving
  ;; is every window switch, under super-save -- and with flydiff a `diff' at
  ;; every pause in typing.  Each is a few milliseconds on macOS and a quarter
  ;; of a second on Windows, where the same changes are a magit status away.
  :unless (eq system-type 'windows-nt)
  ;; The first file opened in one of these modes is what loads diff-hl.  In
  ;; `:init' and not behind anything: a hook that waits for another package
  ;; to load leaves every buffer opened before it without the mode.
  :defer t
  :init
  (defun my/diff-hl-mode-in-file ()
    "Turn on `diff-hl-mode' where there is a file for it to compare.
*scratch* is in a mode derived from `prog-mode' and is there from the
start, so without the test diff-hl, vc and diff-mode would load with
every session, to mark the changes of a buffer that has no file."
    (when buffer-file-name
      (diff-hl-mode 1)))

  (my/add-hook
   (:hook prog-mode-hook conf-mode-hook :func #'my/diff-hl-mode-in-file)
   (:hook dired-mode-hook :func #'diff-hl-dired-mode))
  :config
  ;; Here and not in `:init', because diff-hl autoloads no part of it: a
  ;; magit refresh before any file had loaded diff-hl would call a function
  ;; that is not there yet.  There would be nothing for it to redraw either.
  (my/add-hook
   (:hook magit-post-refresh-hook :func #'diff-hl-magit-post-refresh))

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
