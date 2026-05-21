;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides AI agent configuration using agent-shell and other.

;;; Code:
(use-package agent-shell
  :after evil
  :config
  (my/define-key
   (:map agent-shell-mode-map :state normal :key "RET" #'comint-send-input))
  
  (defun my/ensure-system-package (command install-cmd &optional description)
    "Ensure COMMAND exists by running INSTALL-CMD if it is not found."
    (unless (executable-find command)
      (message "Installing %s..." (or description command))
      (shell-command install-cmd)))
  
  (when (eq system-type 'windows-nt)
    (dolist (path (list (expand-file-name "~/scoop/apps/nodejs/current")
                        (expand-file-name "~/scoop/apps/nodejs/current/bin")))
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ";" (getenv "PATH"))))
    
    (add-to-list 'process-coding-system-alist
                 '("claude-agent-acp" utf-8-dos . cp932-dos)))
  
  (let ((packages
         (append
          (cond ((eq system-type 'darwin)
                 '(("npm" . "brew install nodejs")))
                ((eq system-type 'windows-nt)
                 '(("npm" . "scoop install nodejs")))
                (t nil))
          '(("claude-agent-acp" . "npm install -g @agentclientprotocol/claude-agent-acp --ignore-scripts")))))
    (dolist (package packages)
      (my/ensure-system-package (car package) (cdr package))))
  
  (setq agent-shell-prefer-session-resume nil))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
