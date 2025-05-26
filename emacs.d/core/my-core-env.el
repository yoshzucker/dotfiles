;;; my-core-env.el --- Environment detection and locale setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Sets up essential environment configurations.

;;; Code:

(setq default-directory "~/"
      command-line-default-directory "~/"
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Load shell environment variables (for GUI Emacs) 
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

(defconst my/wsl-p
  (and (eq system-type 'gnu/linux)
       (file-readable-p "/proc/version")
       (let ((case-fold-search t))
         (string-match-p "microsoft"
                         (with-temp-buffer
                           (insert-file-contents "/proc/version")
                           (buffer-string)))))
  "Non-nil if running under Windows Subsystem for Linux.")

(when (native-comp-available-p)
  (add-hook 'kill-emacs-hook #'native-compile-prune-cache))

(provide 'my-core-env)
;;; my-core-env.el ends here
