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

  ;; magit keeps the answers to its git calls, but only for the length of a
  ;; refresh -- and a status does a good deal before the refresh begins.  In
  ;; that stretch it asks `rev-parse --show-toplevel' and `--show-cdup'
  ;; eight times each, because the memory those answers would go in has not
  ;; been opened yet.  Opened around the whole of it, sixteen of them become
  ;; two.
  ;;
  ;; Nothing can go stale inside it: what is remembered is where the
  ;; repository is, and it is not going to move between the moment a status
  ;; is asked for and the moment it is drawn.
  ;;
  ;; Everywhere, not just where it shows.  It costs four milliseconds here
  ;; and five seconds on a machine where starting a process is the expense.
  (define-advice magit-status-setup-buffer
      (:around (orig &rest args) my/hold-the-refresh-cache)
    "Keep magit's answers for the whole of a status, not only its refresh."
    (let ((magit--refresh-cache (or magit--refresh-cache (list (cons 0 0)))))
      (apply orig args)))

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


    ;; The branch line, its upstream and the distance between them, from one
    ;; process instead of two sections' worth.  magit asks those separately
    ;; -- `magit-insert-head-branch-header' and
    ;; `magit-insert-upstream-branch-header' -- which is right where starting
    ;; a process is cheap and is 2.4 of the 8.9 seconds here.
    ;;
    ;; `git status --porcelain=v2 --branch' answers all three at once, and it
    ;; is the only porcelain that does.  What is given up is the subject line
    ;; of the commit at HEAD, which magit reads with a second call; the hash
    ;; comes free in the same output, and the subject is one `l' away.
    (defun my/magit-insert-branch-header ()
      "Insert the branch, what it tracks, and how far apart they are.
Asked in a single `git status', because on this machine the asking is the
expense and not the answering."
      (let (oid head upstream ahead behind)
        (with-temp-buffer
          (when (eq 0 (process-file "git" nil t nil "status" "--porcelain=v2"
                                    "--branch" "--untracked-files=no"))
            (goto-char (point-min))
            (while (re-search-forward "^# branch\\.\\([a-z]+\\) \\(.*\\)$" nil t)
              (let ((field (match-string 1))
                    (value (match-string 2)))
                (pcase field
                  ("oid" (setq oid value))
                  ("head" (setq head value))
                  ("upstream" (setq upstream value))
                  ("ab" (when (string-match "\\`\\+\\([0-9]+\\) -\\([0-9]+\\)\\'" value)
                          (setq ahead (string-to-number (match-string 1 value))
                                behind (string-to-number (match-string 2 value))))))))))
        (when head
          (magit-insert-section (branch head)
            (insert (format "%-10s" "Head: "))
            (when (and oid (not (equal oid "(initial)")))
              (insert (propertize (substring oid 0 (min 7 (length oid)))
                                  'font-lock-face 'magit-hash)
                      ?\s))
            (insert (propertize head 'font-lock-face
                                (if (equal head "(detached)")
                                    'magit-head 'magit-branch-local)))
            (insert ?\n)))
        (when upstream
          (magit-insert-section (branch upstream)
            (insert (format "%-10s" "Merge: "))
            (insert (propertize upstream 'font-lock-face 'magit-branch-remote))
            (when (and ahead behind (> (+ ahead behind) 0))
              (insert (format "  (%s)"
                              (string-join
                               (delq nil
                                     (list (and (> ahead 0) (format "ahead %d" ahead))
                                           (and (> behind 0) (format "behind %d" behind))))
                               ", "))))
            (insert ?\n)))))

    (setq magit-status-headers-hook
          '(magit-insert-error-header
            my/magit-insert-branch-header)
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
