;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides AI agent configuration using agent-shell and other.

;;; Code:
(use-package agent-shell
  :after evil
  :config
  (my/define-key
   (:map agent-shell-mode-map :state normal :key "RET" #'comint-send-input))
  
  (when (eq system-type 'windows-nt)
    (dolist (path (list (expand-file-name "~/scoop/apps/nodejs/current")
                        (expand-file-name "~/scoop/apps/nodejs/current/bin")
                        (expand-file-name "~/scoop/apps/msys2/current/usr/bin")))
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ";" (getenv "PATH"))))
    
    (add-to-list 'process-coding-system-alist
                 '("claude-agent-acp" utf-8-dos . cp932-dos)))
  
  (let ((packages
         (append
          (cond ((eq system-type 'darwin)
                 '(("npm" . "brew install nodejs")))
                ((eq system-type 'windows-nt)
                 `(("npm" . "scoop install nodejs")
                   ("msys2" . "scoop install msys2")
                   ("diff" . ,(concat (getenv "USERPROFILE")
                                      \\scoop\\apps\\msys2\\current\\usr\\bin\\bash.exe
                                      " -lc \"pacman -S --noconfirm diffutils\""))))
                (t nil))
          '(("claude-agent-acp" .
             "npm install -g @agentclientprotocol/claude-agent-acp --ignore-scripts")))))
    (dolist (package packages)
      (my/ensure-system-package (car package) (cdr package))))
  
  (setq agent-shell-prefer-session-resume nil))

(use-package ob-agent-shell
  :straight (:host github :repo "eddof13/ob-agent-shell")
  :after (agent-shell org)
  :config
  (add-to-list 'org-babel-load-languages '(agent-shell . t))
  (add-to-list 'org-src-lang-modes '("agent-shell" . text)))  ;; Prevent font-lock errors

(use-package agent-shell-org-transcript
  :straight (:host github :repo "lllShamanlll/agent-shell-org-transcript")
  :after (agent-shell org-roam))

(use-package agent-shell-manager
  :straight (:host github :repo "jethrokuan/agent-shell-manager")
  :after agent-shell
  :config
  (my/define-key (:map global-map :key "C-c m" #'agent-shell-manager-toggle)))

(use-package knockknock
  :straight (:host github :repo "konrad1977/knockknock"))

(use-package agent-shell-knockknock
  :straight (:host github :repo "xenodium/agent-shell-knockknock")
  :after (agent-shell knockkock)
  :config
  (agent-shell-knockknock-mode 1))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
