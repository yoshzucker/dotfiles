;;; my-files-remote.el --- Remote access and integration tools -*- lexical-binding: t -*-
;;; Commentary:
;; Provides configuration for accessing and interacting with remote environments.
;; Covers TRAMP, WSL, SSH, and containerized systems such as Docker.

;;; Code:

(use-package tramp
  :straight nil
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
