;;; my-emacs-ops.el --- Operational settings for core Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures how Emacs operates as a process:
;; garbage collection, server, startup behavior, warnings, and other global runtime adjustments.

;;; Code:

(setq backup-directory-alist '((".*" . "~/.saves"))
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      garbage-collection-messages t)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package gcmh
  :diminish gcmh-mode
  :config
  (gcmh-mode 1))

(use-package immortal-scratch
  :config
  (immortal-scratch-mode 1))

(provide 'my-emacs-ops)
;;; my-emacs-ops.el ends here
