;;; my-files-vc.el --- Version control and project management -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration for version control systems (VC), Git, and project navigation.
;; Includes Projectile for project discovery and Magit for Git interaction.

;;; Code:

;; What opening a file asks git, and what it is asked for.
;;
;; `vc-refresh-state' runs on `find-file-hook', and for a file in a Git
;; repository it spawns git four times: `ls-files' to see whether the file is
;; registered, `status' for its state, `rev-parse' for the working revision and
;; `symbolic-ref' for the branch.  Almost none of that is git working -- a bare
;; `git --version' costs 8 ms here -- it is four process spawns, which is the
;; expensive thing about starting a program at all and several times more so on
;; Windows.  Measured on macOS: 35 ms to open a .el, 48 ms a .md, against 1.8 ms
;; with this off.  consult-buffer's preview opens a file per candidate, so it
;; was paying that for every candidate moved over.
;;
;; What the four buy is the branch name in the mode line, and nothing else.
;; `vc-backend' still answers, so `C-x v' and diff-hl still find the repository
;; and work exactly as before -- checked against a repository with edits in it,
;; where diff-hl found the same two hunks either way.  Magit never used any of
;; it.  The branch is a magit buffer away, and magit is how this repository is
;; actually worked in.
(remove-hook 'find-file-hook #'vc-refresh-state)

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
         "gh" #'my/find-file-from-base
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
