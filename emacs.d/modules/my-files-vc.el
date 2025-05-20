;;; my-files-vc.el --- Version control and project management -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration for version control systems (VC), Git, and project navigation.
;; Includes Projectile for project discovery and Magit for Git interaction.

;;; Code:

(use-package magit
  :after evil
  :defer t
  :config
  (my/define-key
   (:map magit-mode-map
         :state motion
         :key
         "g:" #'execute-extended-command
         "gf" #'find-file-at-point
         "gs" #'consult-buffer))

  (when (eq system-type 'windows-nt)
    (my/add-hook
     (:hook after-init-hook
            :func (lambda ()
                    (add-to-list 'process-coding-system-alist
                                 '("git" utf-8 . cp932))))
     (:hook git-commit-mode-hook
            :func (lambda ()
                    (set-buffer-file-coding-system 'utf-8))))))

(use-package git-timemachine
  :defer t)

(use-package projectile
  :diminish (projectile-mode " pjt")
  :config
  (setq projectile-find-dir-includes-top-level t
	    projectile-switch-project-action 'projectile-find-dir
	    projectile-mode-line-prefix " pjt")
  (my/define-key
   (:map evil-normal-state-map :key "gp" #'projectile-command-map))
  (projectile-mode 1))

(provide 'my-files-vc)
;;; my-files-vc.el ends here
