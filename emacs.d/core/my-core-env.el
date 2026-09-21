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
;;
;; A login shell, not an interactive one.  The package's default asks for both,
;; which runs the whole of ~/.zshrc to read a PATH that ~/.zshenv has already
;; finished setting -- the same twenty-four entries either way, checked against
;; each other.  What the interactive half adds is the prompt, the completion
;; system, and the palette `colors.sh' writes to the terminal: those escape
;; sequences arrive in the output this package is parsing, which it survives
;; but should not have to.  Measured at 74 ms against 29 ms.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
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
