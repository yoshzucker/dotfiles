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
                    (set-buffer-file-coding-system 'utf-8))))

    ;; A status that can be waited for.  Starting a process costs 174 ms on
    ;; this machine -- `cmd /c exit' costs that, so it is the making of a
    ;; process and not anything about git -- and a full status starts
    ;; sixty-four of them, which is seven and a half seconds.  magit already
    ;; answers three fifths of its own questions from a cache; the rest are
    ;; distinct, so the only thing left to do is ask fewer.
    ;;
    ;; What is kept is what a commit needs: which branch and how far it is
    ;; from its upstream, the files nothing is tracking yet, what has
    ;; changed, and what is staged.  The ahead-and-behind *counts* stay in
    ;; the upstream line -- it is the *lists* of those commits that go -- so
    ;; "there is something to push" is still on the screen.
    ;;
    ;; The interrupted-operation sections stay whatever they cost.  Being
    ;; told that a merge or a rebase is half done is the one thing a status
    ;; is for that cannot be worked out from the files, and a saving made
    ;; there would be paid for on the worst possible day.
    ;; Taken before the cut, which is the whole of its value: read after,
    ;; it is a copy of the short list and the key below toggles nothing.
    (defvar my/magit-status-full
      (cons (default-value 'magit-status-sections-hook)
            (default-value 'magit-status-headers-hook))
      "The section and header lists magit came with, before they were cut.")

    (setq magit-status-headers-hook
          '(magit-insert-error-header
            magit-insert-head-branch-header
            magit-insert-upstream-branch-header)
          magit-status-sections-hook
          '(magit-insert-status-headers
            magit-insert-merge-log
            magit-insert-rebase-sequence
            magit-insert-am-sequence
            magit-insert-sequencer-sequence
            magit-insert-bisect-output
            magit-insert-bisect-rest
            magit-insert-bisect-log
            magit-insert-untracked-files
            magit-insert-unstaged-changes
            magit-insert-staged-changes))

    ;; And the rest when it is wanted.  Everything dropped above is an
    ;; answer to a question asked occasionally -- what have I not pushed,
    ;; what is waiting to be pulled, what did I stash, which tag is nearest
    ;; -- so it is one key away rather than in every status.
    (defun my/magit-status-toggle-everything ()
      "Draw this status with every section magit has, or with the few again."
      (interactive)
      (if (local-variable-p 'magit-status-sections-hook)
          (progn (kill-local-variable 'magit-status-sections-hook)
                 (kill-local-variable 'magit-status-headers-hook)
                 (message "The short status: branch, untracked, unstaged, staged"))
        (setq-local magit-status-sections-hook (car my/magit-status-full))
        (setq-local magit-status-headers-hook (cdr my/magit-status-full))
        (message "Everything: unpushed, unpulled, stashes, tags -- slower"))
      (magit-refresh-buffer))

    (my/define-key
     (:map magit-status-mode-map
           :key
           "C-c C-a" #'my/magit-status-toggle-everything))
))

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
