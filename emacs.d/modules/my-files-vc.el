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
         "gh" #'my/find-file-at-start-point
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

(use-package grep
  :if (eq system-type 'windows-nt)
  :config
  (let ((find (expand-file-name "~/scoop/shims/find.exe")))
    (if (file-exists-p find)
        (setq find-program find)
      (user-error "find not found. install findutils with scoop."))))

(provide 'my-files-vc)
;;; my-files-vc.el ends here
