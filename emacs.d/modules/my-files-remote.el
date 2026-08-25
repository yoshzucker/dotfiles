;;; my-files-remote.el --- Remote access and integration tools -*- lexical-binding: t -*-
;;; Commentary:
;; Reaching files that are not on this machine.  Tramp is the whole of it, and
;; it is reached by naming the place in the file name rather than by any
;; command of its own:
;;
;;   C-x C-f /ssh:user@host:/path     another machine
;;   C-x C-f /sudo::/path             another user, here
;;   C-x C-f /docker:name:/path       inside a container

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
                tramp-file-name-regexp)))

(provide 'my-files-remote)
;;; my-files-remote.el ends here
