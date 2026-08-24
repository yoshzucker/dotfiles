;;; my-files-remote.el --- Remote access and integration tools -*- lexical-binding: t -*-
;;; Commentary:
;; Provides configuration for accessing and interacting with remote environments.
;; Covers TRAMP, WSL, SSH, and containerized systems such as Docker.

;;; Code:

(use-package tramp
  :straight nil
  ;; Loaded when a remote file name is first opened, which most sessions never
  ;; do.  Emacs autoloads Tramp off the file-name handler regardless, so
  ;; nothing here is needed before then -- and on Windows the eager load cost
  ;; nearly a second and a half of every startup, against a fifth of one on
  ;; macOS.  What it does at load time is look at the world it might have to
  ;; reach, and that is the part Windows charges for.
  :defer t
  :config
  (setenv "SHELL" "/bin/bash")

  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (defun my/tramp-ssh-hint ()
    "TRAMP via SSH: C-x C-f /ssh:user@host:/path"
    (interactive)
    (message "TRAMP: C-x C-f /ssh:user@host:/path")))

(provide 'my-files-remote)
;;; my-files-remote.el ends here
