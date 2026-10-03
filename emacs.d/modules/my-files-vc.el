;;; my-files-vc.el --- Version control and project management -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides integration for version control systems (VC), Git, and project navigation.
;; Includes Projectile for project discovery and Magit for Git interaction.

;;; Code:

;; Git, and no other version control.
;;
;; Asking which backend a file is under tries each in turn, and Git is seventh
;; of the eight Emacs knows.  The six before it look for their own marks --
;; RCS and SCCS directories, CVS/Entries, and for Subversion and Bazaar a walk
;; up every parent directory -- and find nothing, on every file asked about.
;; Nothing here is kept in any of them.
(setq vc-handled-backends '(Git))

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

;; What opening a file asks git, again: `magit-auto-revert-mode'.
;;
;; magit keeps buffers in step with files that git operations rewrite -- a
;; checkout, a pull, a rebase -- by turning `auto-revert-mode' on in each
;; buffer whose file git tracks.  Deciding which those are is the cost: every
;; file opened asks git for the repository and then for the file, several
;; processes, and asks twice, because `normal-mode' runs
;; `after-change-major-mode-hook' once for `fundamental-mode' before the real
;; mode's `kill-all-local-variables' throws the first answer away.  Four
;; hundred milliseconds a process in a repository on a synced folder on
;; Windows, so seconds to open a file there.  And on Windows it can fail: git
;; is MSYS2's and answers in /c/ paths, which `magit-toplevel' does not
;; translate in its `--show-cdup' branch -- the one taken when the directory
;; was reached through a junction -- and the error, from the first of the two
;; runs, leaves the file in `fundamental-mode'.
;;
;; git is not the only thing that rewrites files under an open buffer.  A
;; synced folder does it whenever the other machine saves.  So every file
;; buffer follows its file instead, and there is nothing to decide:
;; `global-auto-revert-mode' watches each file for a change and reverts an
;; unmodified buffer when one comes, and magit, seeing it on, turns its own
;; mode off and asks git nothing.  Measured: no difference to opening a file,
;; and a fifth of a millisecond for the periodic check over thirty-seven
;; buffers.
;;
;; On before magit loads, so that magit starts with its own mode off.
(global-auto-revert-mode 1)

;; And what saving a file asks git.  `basic-save-buffer' calls `vc-after-save',
;; which for a file under Git runs `status' to learn whether it is now edited,
;; and redraws the mode line's VC part with the answer -- after, the first time
;; a file is saved in a session, `ls-files' to learn whether it is under Git at
;; all.  Two to four hundred milliseconds on Windows for every save, which
;; super-save makes every buffer switch.
;;
;; The mode line never shows that part here: the refresh above is gone, so it
;; was never drawn to begin with.  So the save forgets the state instead of
;; recomputing it, and whatever asks next -- `C-x v', diff-hl -- computes it
;; then, from git, as it would have.
(define-advice vc-after-save (:override () my/forget-rather-than-ask)
  "Mark the saved file's VC state unknown, without asking the backend."
  (when buffer-file-name
    (vc-file-setprop buffer-file-name 'vc-state nil)
    (vc-file-setprop buffer-file-name 'vc-checkout-time nil)
    (when (bound-and-true-p vc-dir-buffers)
      (vc-dir-resynch-file buffer-file-name))))

;; Its partner before the save asks git the same question for nothing.
;; `vc-before-save' keeps a copy of the pristine file for backends that make
;; version backups, and asks first whether the file is up to date -- a
;; `status' -- and only then whether the backend makes backups, which Git
;; never does.  So the order is turned round, and the backend is the one
;; already known for the file rather than one looked up for the save: a file
;; no VC command has touched this session gets no backup, which on a
;; repository that keeps them is the one thing given up.
(define-advice vc-before-save (:override () my/backups-first)
  "Keep a version backup only where the file's known backend makes them."
  (let* ((file buffer-file-name)
         (backend (and file (vc-file-getprop file 'vc-backend))))
    (ignore-errors
      (unless (file-exists-p file)
        (vc-file-clearprops file))
      (and backend
           (not (eq backend 'none))
           (vc-call-backend backend 'make-version-backups-p file)
           (vc-up-to-date-p file)
           (eq (vc-checkout-model backend (list file)) 'implicit)
           (vc-make-version-backup file)))))

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

  ;; magit remembers the answers to its git calls, but only for the length of
  ;; a refresh -- and a status does a good deal of work before the refresh
  ;; begins.  In that stretch it asks `rev-parse --show-toplevel' ten times
  ;; and `--show-cdup' nine, outside the memory those answers would have gone
  ;; in.  Held open around the whole call, nineteen of them become two, and
  ;; the status starts twenty-nine processes where it started forty-six.
  ;;
  ;; Nothing can go stale inside that window: what is remembered is where the
  ;; repository is, and it does not move between a status being asked for and
  ;; being drawn.
  ;;
  ;; Everywhere rather than only where it shows -- four milliseconds on a
  ;; machine where starting a process is cheap, and two seconds on one where
  ;; it is not.
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

    ;; A status short enough to wait for.  Starting a process costs 261 ms
    ;; here -- `cmd /c exit' costs that, so it is the making of a process and
    ;; nothing about git -- and a status as magit ships it starts enough of
    ;; them to take ten seconds.  What follows brings that to five and a
    ;; half, measured side by side in one run.
    ;;
    ;; Two things are done about it and they are different in kind.  Asking
    ;; one process to answer several questions is free: the branch header
    ;; below does that.  Asking for fewer sections is not free, and what goes
    ;; is chosen by what a commit needs.
    ;;
    ;; Kept: which branch and how far it is from its upstream, the files
    ;; nothing tracks yet, what has changed, what is staged.  The
    ;; ahead-and-behind counts stay in the upstream line -- it is the lists
    ;; of those commits that go -- so "there is something to push" is still
    ;; on the screen.
    ;;
    ;; Kept whatever it costs: the interrupted-operation sections.  Being
    ;; told that a merge or a rebase is half done is the one thing a status
    ;; is for that cannot be worked out from the files, and a saving made
    ;; there would be paid for on the worst possible day.
    ;;
    ;; Gone: the lists of unpushed and unpulled commits, the stashes, the
    ;; nearest tag, the diff-filter reminder.  Each answers a question asked
    ;; occasionally, so `C-c C-a' asks it.

    (defun my/magit-insert-branch-header ()
      "Insert the branch, what it tracks, and how far apart they are.

`git status --porcelain=v2 --branch' answers all three in one process,
where magit asks them across `magit-insert-head-branch-header' and
`magit-insert-upstream-branch-header' and spends six -- which is right
where starting a process is cheap, and more than a second of the ten
where it is not.

What is given up is the subject line of the commit at HEAD, which magit
reads with a call of its own.  The hash comes free in the same output and
the subject is one \\<magit-status-mode-map>\\[magit-log] away."
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

    ;; Read before the cut below, which is the whole of its value: read
    ;; after, it is a copy of the short lists and the key toggles nothing.
    (defvar my/magit-status-full
      (cons (default-value 'magit-status-sections-hook)
            (default-value 'magit-status-headers-hook))
      "The section and header lists magit came with, before they were cut.")

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
           "C-c C-a" #'my/magit-status-toggle-everything))))

(use-package git-timemachine
  :defer t)

(use-package grep
  :if (eq system-type 'windows-nt)
  ;; The first grep or find command loads it, and this is in time for that.
  :defer t
  :config
  (let ((find (expand-file-name "~/scoop/shims/find.exe")))
    (if (file-exists-p find)
        (setq find-program find)
      (user-error "find not found. install findutils with scoop."))))

(provide 'my-files-vc)
;;; my-files-vc.el ends here
