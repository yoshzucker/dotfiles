;;; my-core-straight.el --- Keeping straight's clones current and tidy -*- lexical-binding: t; -*-

;;; Commentary:
;; Commands for the clones and builds straight keeps: fetching them all at
;; once, writing the lockfile before anything moves and moving to it after,
;; and pruning what no recipe names.
;; Core rather than a module, so they are there before any module loads --
;; a package broken by its last update is fixed by updating it again.

;;; Code:

(require 'straight)

;; Fetching every package, without waiting for each in turn.
;;
;; `straight-fetch-all' runs one `git fetch' per repository, one after the
;; next, each paying a network round trip.  Measured at a second apiece over
;; a hundred and ninety repositories, and the same second whether the
;; repository is four megabytes or forty -- the cost is the handshake and not
;; the history, which is also why a shallow clone would not help.  Three
;; minutes of it, where several at once take a quarter of one.

(defun my/straight--recipes-by-repo ()
  "Return a table from local repository name to the recipes naming it.

A list and not one recipe, because several packages can share a clone --
magit and magit-section do -- and a repository is only safe to treat as
ordinary if it is ordinary for every package that reads it."
  (let ((table (make-hash-table :test #'equal)))
    (maphash (lambda (_package recipe)
               (straight--with-plist recipe (local-repo)
                 (when local-repo (push recipe (gethash local-repo table)))))
             straight--recipe-cache)
    table))

(defvar my/straight-fetch-jobs 16
  "How many `git fetch' processes to have in flight at once.

Enough to hide the round trips behind each other, and not so many that a
rate limit or a laptop fan becomes the thing being measured.  The work is
waiting rather than computing, so this is not a count of cores.")

(defvar my/straight-fetch-timeout 60
  "Seconds to let one `git fetch' run before giving up on it.

Long enough that a slow repository on a slow line finishes, short enough
that a stuck one does not become the whole command.  One fetch here takes
a second.")

(defvar my/straight-fetch-pipe-wait 5
  "Seconds to go on trying to start a fetch that has no pipes to start with.

Only counted while no fetch of ours is running: the descriptors the
finished ones held are closed a little after they finish, and this is
how long that is given.")

(defconst my/straight-fetch-environment
  '("GIT_TERMINAL_PROMPT=0"
    "GIT_SSH_COMMAND=ssh -o BatchMode=yes -o ConnectTimeout=10")
  "Environment that stops `git fetch\=' from asking anybody anything.

Nothing here is cloned with an account: the recipes name public
repositories and git reads them anonymously.  That is not the same as
never being asked a question.  A host answers 401 rather than 404 for a
repository it will not show -- renamed, deleted, made private -- and git
then asks who you are; and the packages written here are cloned over
ssh, which wants an unknown host key confirmed the first time.  Neither
question can be answered: git writes to the terminal, and a fetch run
from a command loop has none.  With sixteen in flight the count stops
and nothing says why.

So they are told not to ask.  What was a command that hung is then a
repository in the refused list, with a name, which is something to go
and fix.  BatchMode does not stop an agent from answering: a key already
unlocked is still offered.")

(defun my/straight-fetch-at-once (&optional repos)
  "Run `git fetch' in every straight repository, several at a time.
With REPOS, a list of repository names, in those alone.

Return a plist.  `:refused' names the repositories git would not fetch,
newest first; `:abandoned' the ones killed for taking longer than
`my/straight-fetch-timeout'; `:moved' the ones something actually
arrived in, for the message that says how much did.  git says it for
free: onto a pipe it prints when a ref moves and is silent
when none did.

Says how far it has got as it goes.  Sixteen seconds here is one machine
on one network, and neither is the slow case: a Windows box walking its
own antivirus for every object it writes takes a multiple of it, and a
command that goes quiet for minutes is one nobody can tell from a hung
one.  `make-progress-reporter' rate-limits the echo area itself, so this
costs a redraw every fifth of a second and not one per repository.

Whatever is still running is killed on the way out, so `C-g' leaves no
git processes behind to finish into a command that has gone."
  (unless (executable-find "git")
    (user-error "No git on PATH"))
  (let* (;; The repositories a recipe points at.  Walking the directory
         ;; instead fetched
         ;; every clone that has ever been made here -- fifty-seven of a
         ;; hundred and ninety-one were for packages nothing declares any
         ;; more, and each one is a round trip to a server for an answer
         ;; nothing reads.
         (wanted (my/straight--recipes-by-repo))
         (queue (seq-filter
                 (lambda (dir)
                   (and (let ((name (file-name-nondirectory dir)))
                          (if repos (member name repos) (gethash name wanted)))
                        (file-directory-p dir)
                        ;; A worktree keeps a file there rather than a
                        ;; directory, and a repository of mine is a symlink
                        ;; to one I edit -- both are repositories to fetch.
                        (file-exists-p (expand-file-name ".git" dir))))
                 (directory-files (straight--repos-dir) t
                                  directory-files-no-dot-files-regexp)))
         (total (length queue))
         (began (float-time))
         (reporter (make-progress-reporter
                    (format "straight: fetching %d repositories..." total)
                    0 total))
         ;; Told not to ask anything, because there is nobody to ask.
         (process-environment (append my/straight-fetch-environment
                                      process-environment))
         (procs nil)
         (failed nil) (moved nil) (abandoned nil)
         ;; Which repositories something arrived in.  `--quiet' is gone so
         ;; that git will say: onto a pipe it writes a line when a ref moves
         ;; and nothing at all when none did, which is the question the message
         ;; wants answered and costs no process to ask.  Asking it
         ;; instead with `git rev-parse --remotes' before and after was two
         ;; more processes per repository -- eight milliseconds each here and
         ;; a multiple of that on a machine where spawning is the expense,
         ;; which is exactly the machine this is for.
         (spoke (make-hash-table :test #'equal))
         ;; Since when no fetch could be started with none of ours running.
         (starved nil))
    (unwind-protect
        (progn
          ;; No sentinels, and nothing counted that is not counted from
          ;; the processes themselves.  A tally kept by sentinels is a tally
          ;; that can be lost -- one missed leaves it above zero for good,
          ;; the loop never ends, and the timeout below cannot save it
          ;; because there is no live process left to kill.  Deriving the
          ;; count instead removes that, and then ending the loop the moment
          ;; nothing is live introduces the opposite fault: a process is
          ;; dead before its sentinel runs, so a sentinel is the wrong place
          ;; to learn how it ended.  Both go away by asking the process:
          ;; `process-exit-status' answers for a dead one whether or not
          ;; anything was notified.
          (while (or queue (seq-some #'process-live-p procs))
            (let ((full nil))
              (while (and queue (not full)
                          (< (seq-count #'process-live-p procs)
                             my/straight-fetch-jobs))
                (let* ((dir (pop queue))
                       (name (file-name-nondirectory (directory-file-name dir)))
                       (default-directory dir)
                       ;; Windows gives Emacs sixty-four descriptors for
                       ;; everything, and each fetch holds two pipes.  They
                       ;; are let go below as each one finishes, but Emacs
                       ;; may not have closed them yet, and the server and a
                       ;; language server or two hold theirs.  So "Too many
                       ;; open files" is a reason to wait and not to stop:
                       ;; the repository goes back on the queue and is tried
                       ;; again on the next turn.  Only when it goes on for
                       ;; `my/straight-fetch-pipe-wait' with nothing of ours
                       ;; running is it something waiting will not mend.
                       (proc
                        (condition-case err
                            (make-process
                             :name (concat "straight-fetch-" name)
                             ;; Aborted rather than waited on when the bytes
                             ;; stop arriving: git has no timeout of its own
                             ;; over HTTP, so a transfer that stalls part way
                             ;; -- a proxy that inspects every connection, a
                             ;; link that drops -- waits forever.  Under a
                             ;; kilobyte a second for twenty seconds is not a
                             ;; slow clone, it is a dead one.
                             :command '("git"
                                        "-c" "http.lowSpeedLimit=1024"
                                        "-c" "http.lowSpeedTime=20"
                                        "fetch")
                             :noquery t
                             :connection-type 'pipe
                             :buffer nil
                             ;; Drained rather than read: what it says does
                             ;; not matter, only that it said anything, and a
                             ;; process whose output nobody takes can block
                             ;; on a full pipe.
                             :filter (lambda (_proc _chunk) (puthash name t spoke)))
                          (file-error
                           (unless (seq-some #'process-live-p procs)
                             (setq starved (or starved (float-time)))
                             (when (> (- (float-time) starved)
                                      my/straight-fetch-pipe-wait)
                               (signal (car err) (cdr err))))
                           (push dir queue)
                           (setq full t)
                           nil))))
                  (when proc
                    (setq starved nil)
                    (push proc procs)
                    (process-put proc 'my/straight-repo name)
                    (process-put proc 'my/straight-began (float-time))))))
            ;; Short, because this loop is also what starts the next process
            ;; as each one finishes.
            (accept-process-output nil 0.05)
            (progress-reporter-update
             reporter (- total (length queue)
                         (seq-count #'process-live-p procs)))
            ;; Each finished one let go of now, its pipes with it.  A dead
            ;; process keeps them until Emacs gets round to closing them,
            ;; and on Windows that was slow enough for a fifth of a hundred
            ;; and twenty-eight to use up every descriptor there was.  What
            ;; it still has to say is read first, and how it ended kept,
            ;; since a deleted process no longer says.
            (dolist (proc procs)
              (unless (or (process-live-p proc)
                          (process-get proc 'my/straight-exit))
                (ignore-errors (accept-process-output proc 0))
                (process-put proc 'my/straight-exit (process-exit-status proc))
                (delete-process proc)))
            ;; And what notices one that is never going to finish.  A
            ;; killed process has an exit status like any other, so it is
            ;; classified below with the rest and named in the report.
            (dolist (proc procs)
              (when (and (process-live-p proc)
                         (> (- (float-time)
                               (process-get proc 'my/straight-began))
                            my/straight-fetch-timeout))
                (push (process-get proc 'my/straight-repo) abandoned)
                (delete-process proc))))
          ;; Each one asked how it ended, now that none of them is running.
          ;; Whatever they said reached the filter on the way: the loop
          ;; above cannot leave while a process is live, and the waiting it
          ;; does there is what reads their output.
          (dolist (proc (reverse procs))
            (let ((name (process-get proc 'my/straight-repo)))
              (if (eq 0 (or (process-get proc 'my/straight-exit)
                            (process-exit-status proc)))
                  (when (gethash name spoke) (push name moved))
                (push name failed))))
          (progress-reporter-done reporter)
          (message "straight: fetched %d repositories in %.0fs; %d changed%s%s"
                   total (- (float-time) began) (length moved)
                   (if failed
                       (format "; %d refused: %s" (length failed)
                               (string-join (reverse failed) ", "))
                     "")
                   (if abandoned
                       (format "; %d gave no answer in %ds: %s"
                               (length abandoned) my/straight-fetch-timeout
                               (string-join (reverse abandoned) ", "))
                     ""))
          (list :refused failed :moved moved :abandoned abandoned))
      (dolist (proc procs)
        (when (process-live-p proc)
          (set-process-sentinel proc #'ignore)
          (delete-process proc))))))

(defun my/straight--origin-default-branch (dir)
  "Return the branch origin calls default in the repository at DIR.

Read rather than asked for: `refs/remotes/origin/HEAD' is a symbolic ref,
which git keeps as a one-line file naming what it points at.  nil when
there is no such file -- a clone made without one, or a repository not
shaped like these -- and then the caller has no business guessing."
  (let ((head (expand-file-name ".git/refs/remotes/origin/HEAD" dir)))
    (when (file-readable-p head)
      (with-temp-buffer
        (insert-file-contents head)
        (goto-char (point-min))
        (when (looking-at "ref: refs/remotes/origin/\\(.+\\)$")
          (string-trim (match-string 1)))))))

(defun my/straight--ref (dir ref)
  "Return the commit REF names in the repository at DIR, or nil.

Read and not asked for.  A ref is a file holding a commit, until git
tidies the loose ones into `packed-refs\=', so both places are looked in.
REF is a branch, which holds a commit; HEAD, which does not, is
`my/straight--head-branch\='s to read."
  (let ((loose (expand-file-name (concat ".git/" ref) dir)))
    (if (file-readable-p loose)
        (with-temp-buffer
          (insert-file-contents loose)
          (string-trim (buffer-string)))
      (let ((packed (expand-file-name ".git/packed-refs" dir)))
        (when (file-readable-p packed)
          (with-temp-buffer
            (insert-file-contents packed)
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^\\([0-9a-f]\\{40,\\}\\) " (regexp-quote ref) "$")
                   nil t)
              (match-string 1))))))))

(defun my/straight--head-branch (dir)
  "Return the branch HEAD is on in the repository at DIR, or nil.

nil for a detached HEAD, which names a commit rather than a branch."
  (let ((head (expand-file-name ".git/HEAD" dir)))
    (when (file-readable-p head)
      (with-temp-buffer
        (insert-file-contents head)
        (goto-char (point-min))
        (when (looking-at "ref: refs/heads/\\(.+\\)$")
          (string-trim (match-string 1)))))))

(defun my/straight--head-commit (dir)
  "Return the commit HEAD is at in the repository at DIR, or nil.

Read and not asked for, like `my/straight--ref'.  HEAD on a branch names
the branch; a detached one holds the commit itself."
  (if-let* ((branch (my/straight--head-branch dir)))
      (my/straight--ref dir (concat "refs/heads/" branch))
    (let ((head (expand-file-name ".git/HEAD" dir)))
      (when (file-readable-p head)
        (with-temp-buffer
          (insert-file-contents head)
          (let ((commit (string-trim (buffer-string))))
            (and (string-match-p "\\`[0-9a-f]\\{40,\\}\\'" commit) commit)))))))

(defun my/straight--commits-between (dir from to)
  "Return the one-line log of the commits DIR gained going FROM TO.

Newest first, as git gives them: whatever is in TO and not in FROM."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (when (eq 0 (call-process "git" nil t nil "log" "--oneline" "--no-decorate"
                                (concat from ".." to)))
        (split-string (buffer-string) "\n" t)))))

(defun my/straight--repo-holds-work-p (path)
  "Say whether PATH holds anything that is not also on its remote.

Three ways it can.  Something uncommitted, something stashed, or a commit
on a branch that no remote has.  A repository with none of them can be
cloned again and lose nothing but the time; one with any of them cannot.

A directory that is not a repository at all counts as holding work, and
the check has to be made rather than left to git: asked from a directory
with no `.git' of its own, git answers for whichever repository is above
it -- and there is one above these, so every such directory would report
its neighbours' untracked files as its own."
  (let ((default-directory (file-name-as-directory path)))
    (or (not (file-exists-p (expand-file-name ".git" path)))
        (file-exists-p (expand-file-name ".git/refs/stash" path))
        (with-temp-buffer
          (and (eq 0 (call-process "git" nil t nil "status" "--porcelain"))
               (> (buffer-size) 0)))
        (with-temp-buffer
          (and (eq 0 (call-process "git" nil t nil
                                   "log" "--branches" "--not" "--remotes"
                                   "--oneline" "-1"))
               (> (buffer-size) 0))))))

(defun my/straight-repo-status ()
  "Say what every directory under straight's repos is, as an alist.

The cdr is one of:

  `named'     a recipe in this session points at it
  `linked'    a symbolic link -- a package written here, kept elsewhere
  `straight'  straight itself
  `built-in'  Emacs ships it; `straight-built-in-pseudo-packages' names it,
              and the clone is what was fetched before that was true
  `holds-work' nothing names it, but it holds something its remote does not
  `orphan'    none of the above, and nothing in it that a fetch would not
              bring back"
  (let ((named (let (repos)
                 (maphash (lambda (_ recipe)
                            (when-let* ((repo (plist-get recipe :local-repo)))
                              (cl-pushnew repo repos :test #'equal)))
                          straight--recipe-cache)
                 repos))
        (built-in (mapcar #'symbol-name
                          (bound-and-true-p straight-built-in-pseudo-packages)))
        (directory (straight--repos-dir)))
    (mapcar
     (lambda (name)
       (let ((path (expand-file-name name directory)))
         (cons name
               (cond ((file-symlink-p path)      'linked)
                     ((equal name "straight.el") 'straight)
                     ((member name named)        'named)
                     ((member name built-in)     'built-in)
                     ;; Asked only of the ones that would otherwise go, since
                     ;; it is two git processes each and the answer changes
                     ;; nothing for a repository that is staying.
                     ((my/straight--repo-holds-work-p path) 'holds-work)
                     (t                          'orphan)))))
     (seq-filter (lambda (name) (file-directory-p (expand-file-name name directory)))
                 (directory-files directory nil "\\`[^.]")))))

(defun my/straight-prune-repos (&optional list-only)
  "Delete the cloned repositories that nothing in this configuration names.

Shows what it proposes and asks.  With LIST-ONLY it only shows.

Three kinds of directory are never touched, whatever the listing says.  A
symbolic link is a package written here and kept somewhere else, and
deleting one recursively would take the source with it rather than the
link.  straight's own repository is what would be doing the deleting.  And
a clone holding something uncommitted, stashed, or on a branch no remote
has is the one case where deleting costs more than a fetch -- that is
asked of each candidate, not assumed.

`named' is wider than what is written here, and deliberately: a recipe is
registered for every dependency as well, so `transient' is named although
nothing writes it -- magit asks for it.  Forty-five of the named repos are
like that.

What makes the answer trustworthy is two things, both checked rather than
assumed.  Every `use-package' form here is at top level and every module
is read, so each registers its recipe whether or not the package is used:
a `:if' that is false stops the package loading and not the recipe
(a package behind `:if (eq system-type 'darwin)' keeps its recipe on
Windows).  And a dependency is registered when its parent's recipe is, not
when the parent loads, so a deferred package does not hide what it needs
(magit is unloaded in a fresh session and transient is named all the same).

So it has to be run from a session that finished starting.  From `emacs
-Q', or before the modules have been read, nothing has registered anything
and everything looks abandoned; it refuses rather than offer that."
  (interactive "P")
  (when (< (hash-table-count straight--recipe-cache) 20)
    (user-error
     "Only %d recipes are registered -- this session has not read the modules"
     (hash-table-count straight--recipe-cache)))
  (let* ((status (my/straight-repo-status))
         (kind (lambda (k) (mapcar #'car (seq-filter (lambda (e) (eq (cdr e) k)) status))))
         (orphans (funcall kind 'orphan))
         (linked (funcall kind 'linked))
         (holding (funcall kind 'holds-work))
         (buffer (get-buffer-create "*straight repositories*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d directories under %s\n\n"
                        (length status) (straight--repos-dir)))
        (dolist (kind '(named linked straight built-in holds-work orphan))
          (let ((names (sort (mapcar #'car (seq-filter (lambda (e) (eq (cdr e) kind)) status))
                             #'string<)))
            (when names
              (insert (format "%s (%d)\n  " kind (length names))
                      (string-join names " ") "\n\n"))))
        (insert "`named' is every recipe this session registered: what is written in\n"
                "a `use-package' form, and everything those depend on -- transient is\n"
                "named because magit asks for it.  Neither a false `:if' nor a\n"
                "deferred package hides one, so `orphan' is what nothing here asks\n"
                "for, at first hand or second.\n\n"
                "Never deleted: `linked' (a package written here, kept in ~/Developer),\n"
                "`straight' (what would be doing the deleting), and `holds-work' --\n"
                "a clone with something uncommitted, stashed, or on no remote.\n\n"
                "`built-in' is Emacs' own and the clone is a leftover, though straight\n"
                "may fetch it again.\n\n"
                "So everything offered below can be cloned again and lose nothing but\n"
                "the time.\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)
    (cond
     (list-only
      (message "%d orphaned of %d" (length orphans) (length status)))
     ((null orphans)
      (message "Nothing to prune"))
     ;; A question that can be answered.  Asking whether to delete a list of
     ;; fifty names invites a judgement nobody has -- half of them arrived as
     ;; somebody else's dependency.  What can be judged is the guarantee.
     ((yes-or-no-p
       (format (concat "Delete %d abandoned %s?  "
                       "Each is committed, unstashed and on its remote; "
                       "%d link%s, straight itself%s are kept. ")
               (length orphans)
               (if (= 1 (length orphans)) "repository" "repositories")
               (length linked) (if (= 1 (length linked)) "" "s")
               (if holding
                   (format " and %d holding local work" (length holding))
                 "")))
      (let (failed)
        (dolist (name orphans)
          (let ((path (expand-file-name name (straight--repos-dir))))
            (condition-case err
                (progn
                  ;; Git keeps its objects read-only, and on Windows that is
                  ;; enough to stop a delete.  Elsewhere the mode of a file
                  ;; does not, and walking the tree to say so costs time for
                  ;; nothing.
                  (when (eq system-type 'windows-nt)
                    (dolist (file (directory-files-recursively path "" t))
                      (ignore-errors (set-file-modes file #o700))))
                  (delete-directory path 'recursive delete-by-moving-to-trash))
              (error (push (cons name (error-message-string err)) failed)))))
        ;; Written into the report as well as said.  The echo area holds the
        ;; outcome until the next thing prints, and moving fifty directories
        ;; to the trash gives several other things the chance -- so the one
        ;; line saying what happened is the first line of the buffer that is
        ;; already on screen, where it stays.
        (let ((outcome
               (format "Pruned %d, kept %d.%s  %s\n"
                       (- (length orphans) (length failed))
                       (- (length status) (length orphans))
                       (if failed
                           (format "  %d refused: %s." (length failed)
                                   (string-join (mapcar #'car failed) " "))
                         "")
                       (if delete-by-moving-to-trash
                           "They are in the trash, so this is reversible."
                         "`delete-by-moving-to-trash' is nil, so they are gone."))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert outcome
                      (make-string (1- (length outcome)) ?-) "\n"
                      "The listing below is how things stood before that.\n\n")))
          (message "%s" (string-trim outcome))))))))

(defun my/straight-build-status ()
  "Say what every directory under straight\\='s build tree is, as an alist.

The cdr is `named\\=' when a recipe in this session registered a package of
that name, `orphan\\=' when none did, and `hollow\\=' when one did but the
directory holds no library by that name.

Named by *package*, where the repositories are named by `:local-repo\\=' --
`dash\\=' is built from a repository called dash.el, `magit-section\\=' from
one called magit.  Comparing the two trees by name answers a question
neither of them was asked.

A hollow directory is what a collision between two `:local-repo\\=' names
leaves behind.  Sharing one repository between packages is ordinary and
straight allows it -- magit and magit-section come from the same clone --
so it cannot tell that apart from two unrelated projects whose
repositories happen to share a name, and the package asked for second is
built from the first one\\='s checkout.  What comes out is a directory with
the right name over somebody else\\='s files, and the package that needed
it fails to load in whatever way its caller happens to report -- which in
one case was not at all."
  (let ((packages (let (ps)
                    (maphash (lambda (package _) (push (format "%s" package) ps))
                             straight--recipe-cache)
                    ps))
        (directory (straight--build-dir)))
    (mapcar
     (lambda (name)
       (cons name
             (cond
              ((not (member name packages)) 'orphan)
              ;; Asked of `load-path' rather than of the directory listing,
              ;; because loadable is the question: a library found anywhere
              ;; but here is one this directory is not providing.
              ((let ((library (locate-library name)))
                 (and library
                      (string-prefix-p (file-name-as-directory
                                        (expand-file-name name directory))
                                       (expand-file-name library))))
               'named)
              (t 'hollow))))
     (seq-filter (lambda (name) (file-directory-p (expand-file-name name directory)))
                 (directory-files directory nil "\\`[^.]")))))

(defun my/straight-prune-builds (&optional list-only)
  "Delete the built copies that no package in this configuration answers to.

Shows what it proposes and asks.  With LIST-ONLY it only shows.

Sibling of `my/straight-prune-repos\\=', and the easier half: a build
directory is made rather than fetched -- symbolic links into the repository
and the byte-compiled files beside them -- so nothing here is the only copy
of anything.  Deleting one that is still wanted costs a rebuild and no more,
which is why this asks about the guarantee rather than about the names.

Recursive deletion is safe over those links: `delete-directory\\=' removes a
symbolic link as a link and does not follow it, so the repository it points
into is untouched."
  (interactive "P")
  (when (< (hash-table-count straight--recipe-cache) 20)
    (user-error
     "Only %d recipes are registered -- this session has not read the modules"
     (hash-table-count straight--recipe-cache)))
  (let* ((status (my/straight-build-status))
         (orphans (mapcar #'car (seq-filter (lambda (e) (eq (cdr e) 'orphan)) status)))
         (buffer (get-buffer-create "*straight builds*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d directories under %s\n\n"
                        (length status) (straight--build-dir)))
        (dolist (kind '(named hollow orphan))
          (let ((names (sort (mapcar #'car (seq-filter (lambda (e) (eq (cdr e) kind)) status))
                             #'string<)))
            (when names
              (insert (format "%s (%d)\n  " kind (length names))
                      (string-join names " ") "\n\n"))))
        (insert "Named by package, not by repository: `dash' is built from a\n"
                "repository called dash.el and `magit-section' from one called\n"
                "magit, so the two trees cannot be compared by name.\n\n"
                "Nothing here is the only copy of anything -- a build directory is\n"
                "links into the repository and the compiled files beside them -- so\n"
                "the worst a mistake costs is a rebuild.\n\n"
                "A hollow one is not deleted here: it is a name over the\n"
                "wrong files, and what fixes it is a `:local-repo' of its\n"
                "own in the recipe, after which the rebuild follows.\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)
    (cond
     (list-only (message "%d of %d build directories answer to nothing"
                         (length orphans) (length status)))
     ((null orphans) (message "Nothing to prune"))
     ((yes-or-no-p
       (format "Delete %d built %s nothing answers to?  A rebuild is all one costs. "
               (length orphans)
               (if (= 1 (length orphans)) "copy" "copies")))
      (let (failed)
        (dolist (name orphans)
          (let ((path (expand-file-name name (straight--build-dir))))
            (condition-case err
                (progn
                  (when (eq system-type 'windows-nt)
                    (dolist (file (directory-files-recursively path "" t))
                      (ignore-errors (set-file-modes file #o700))))
                  (delete-directory path 'recursive delete-by-moving-to-trash))
              (error (push (cons name (error-message-string err)) failed)))))
        (let ((outcome
               (format "Pruned %d, kept %d.%s  %s\n"
                       (- (length orphans) (length failed))
                       (- (length status) (length orphans))
                       (if failed
                           (format "  %d refused: %s." (length failed)
                                   (string-join (mapcar #'car failed) " "))
                         "")
                       (if delete-by-moving-to-trash
                           "They are in the trash, so this is reversible."
                         "`delete-by-moving-to-trash' is nil, so they are gone."))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert outcome
                      (make-string (1- (length outcome)) ?-) "\n"
                      "The listing below is how things stood before that.\n\n")))
          (message "%s" (string-trim outcome))))))))

;; Moving forward, looked at first.  `my/straight-update-lockfile' fetches,
;; and writes into the lockfile the commit each repository would move to --
;; and moves nothing.  What would arrive is then read: the lockfile's diff,
;; and the commits in `my/straight-update-lockfile-buffer', also left in
;; `my/straight-update-lockfile-file' for whatever reads files rather than
;; buffers.  A line put back keeps that package where it is.
;; `my/straight-apply-lockfile' then moves every repository to exactly what
;; was read, and nothing that arrived upstream since; the lockfile is
;; committed once the packages have been tried.

(defvar my/straight-lockfile-wait-days 7
  "Days a commit has to have been upstream before the lockfile takes it.
A release that was taken over is usually found and pulled within days, and
a commit too new to be taken is taken the next time.  The age is the
committer date, which whoever makes the commit sets: this keeps the
ordinary new commit out, and is no defence against one dated to look old.")

(defconst my/straight-update-lockfile-buffer "*straight-update-lockfile*"
  "The buffer `my/straight-update-lockfile' says what it wrote in.")

(defconst my/straight-update-lockfile-file
  (locate-user-emacs-file "var/straight-update-lockfile.txt")
  "Where `my/straight-update-lockfile' leaves the text of its report.")

(defun my/straight--git-output (dir &rest args)
  "Return what git prints for ARGS in DIR, trimmed, or nil when it fails."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (when (eq 0 (apply #'call-process "git" nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun my/straight--candidate-commit (dir locked days)
  "Return (COMMIT . WHY) for the repository at DIR, locked at LOCKED.

COMMIT is what the lockfile should name: the newest commit on origin's
default branch at least DAYS old, when that is ahead of LOCKED.  Otherwise
LOCKED, and WHY says why it stays.  A working tree linked in from
~/Developer is taken at its HEAD, whatever its age: it is a package written here, and its newest
commit is wanted at once."
  (cond
   ((file-symlink-p (directory-file-name dir))
    (cons (or (my/straight--head-commit dir) locked) nil))
   (t
    (let* ((branch (my/straight--origin-default-branch dir))
           (newest (and branch
                        (my/straight--git-output
                         dir "rev-list" "-1"
                         (format "--before=%d.days.ago" days)
                         (concat "origin/" branch)))))
      (cond
       ((not branch) (cons locked "origin names no default branch"))
       ((or (not newest) (string-empty-p newest))
        (cons locked (format "nothing upstream is %d days old" days)))
       ((equal newest locked) (cons locked nil))
       ((not (my/straight--git-output dir "merge-base" "--is-ancestor"
                                      locked newest))
        (cons locked "the lockfile is not behind it"))
       (t (cons newest nil)))))))

(defun my/straight--write-lockfile (versions)
  "Write VERSIONS, an alist of repository and commit, as straight's lockfile.
The same text `straight-freeze-versions' writes, so the diff shows only
the lines that moved."
  (let ((file (straight--versions-file
               (or (alist-get nil straight-profiles) "default.el"))))
    (with-temp-file file
      (insert (format "(%s)\n:epsilon\n"
                      (mapconcat (apply-partially #'format "%S")
                                 (cl-sort (copy-sequence versions)
                                          #'string-lessp :key #'car)
                                 "\n "))))))

(defun my/straight-update-lockfile (&optional days)
  "Fetch, and write into the lockfile what each repository would move to.

Nothing moves.  Each repository is written at the newest commit on its
origin's default branch that is at least DAYS old -- a prefix argument, or
`my/straight-lockfile-wait-days' -- and a working tree linked from
~/Developer at its HEAD.  What would arrive is listed in
`my/straight-update-lockfile-buffer' and `my/straight-update-lockfile-file'.  Read it,
put back any line of the lockfile that should not move, then
`my/straight-apply-lockfile'."
  (interactive "P")
  (let ((days (if days (prefix-numeric-value days) my/straight-lockfile-wait-days))
        candidates moved kept)
    (my/straight-fetch-at-once)
    (pcase-dolist (`(,repo . ,locked) (straight--lockfile-read-all))
      (let* ((dir (straight--repos-dir repo))
             (pick (if (file-directory-p dir)
                       (my/straight--candidate-commit dir locked days)
                     (cons locked "not cloned here"))))
        (push (cons repo (car pick)) candidates)
        (cond
         ((not (equal (car pick) locked))
          (push (list repo locked (car pick)
                      (my/straight--commits-between dir locked (car pick)))
                moved))
         ((cdr pick) (push (cons repo (cdr pick)) kept)))))
    (my/straight--write-lockfile candidates)
    (setq moved (nreverse moved)
          kept (nreverse kept))
    (let ((report
           (with-temp-buffer
             (insert (format-time-string "%Y-%m-%d %H:%M  ")
                     (format "%d to move, commits at least %d days old (own packages at HEAD)\n"
                             (length moved) days))
             (pcase-dolist (`(,repo ,from ,to ,commits) moved)
               (insert (format "\n%s (%s..%s)\n" repo
                               (substring from 0 7) (substring to 0 7)))
               (dolist (commit commits)
                 (insert "  " commit "\n")))
             (when kept
               (insert "\nKept where they are:\n")
               (pcase-dolist (`(,repo . ,why) kept)
                 (insert (format "  %s: %s\n" repo why))))
             (insert "\nNothing has moved.  Read this and the lockfile's diff, put back"
                     "\nany line that should stay, then M-x my/straight-apply-lockfile.\n")
             (buffer-string))))
      (make-directory (file-name-directory my/straight-update-lockfile-file) t)
      (with-temp-file my/straight-update-lockfile-file (insert report))
      (with-current-buffer (get-buffer-create my/straight-update-lockfile-buffer)
        (special-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min)))
        (display-buffer (current-buffer)))
      (message "straight: %d updated in the lockfile; nothing has moved"
               (length moved)))))

;; A follower of the lockfile -- any machine without init.el's
;; `my/straight-pull-marker' -- is moved by `my/straight-apply-lockfile' and by
;; nothing else.  Every command that would take a repository off the
;; lockfile -- or rewrite the lockfile from a machine that only reads it --
;; refuses there, so a pull typed out of habit on a follower does nothing
;; instead of bringing in what was never tried.
(defconst my/straight-moving-commands
  '(straight-pull-package straight-pull-package-and-deps straight-pull-all
    straight-pull-recipe-repositories
    straight-merge-package straight-merge-package-and-deps straight-merge-all
    straight-normalize-package straight-normalize-all
    straight-push-package straight-push-all
    straight-freeze-versions
    my/straight-update-lockfile)
  "Commands that move a repository off the lockfile, or rewrite it.")

(defvar my/straight-follow-lockfile)
(defvar my/straight-pull-marker)

(defun my/straight-refuse-on-follower (orig &rest args)
  "Call ORIG with ARGS, unless this machine follows the lockfile."
  (if my/straight-follow-lockfile
      (user-error "Packages follow the lockfile here; `my/straight-apply-lockfile' moves them (%s would allow pulling)"
                  (abbreviate-file-name my/straight-pull-marker))
    (apply orig args)))

(defun my/straight-guard-moving-commands ()
  "Put `my/straight-refuse-on-follower' around every moving command."
  (dolist (command my/straight-moving-commands)
    (advice-add command :around #'my/straight-refuse-on-follower)))

(my/straight-guard-moving-commands)

(defun my/straight-apply-lockfile ()
  "Put every repository at the commit the lockfile names.

Only the repositories that are not there already are handed to
`straight-thaw-versions'.  Where HEAD is is read from the files git
keeps, with no process; straight, given a repository, runs git over and
over -- its remotes, its worktree, its branches -- before it checks
anything out, and on Windows, where every process is expensive, doing
that for a hundred repositories that are already in place was most of
the wait.

Of those, the ones that lack their commit are fetched first, all at once,
so that straight finds the commits there instead of fetching for them one
repository at a time.  The rest are not asked: a commit, once here, is
what the lockfile names whatever the server now has.  The repositories
that moved are registered as modified, which is how straight on Windows
learns that a package has to be built again: the walk that would notice
at startup is turned off there.  They are rebuilt when Emacs next starts.

A working tree linked in from ~/Developer is left alone.  Checking out a
commit there is `git reset --hard' on the branch being written, and the
commits made after the lockfile was written would drop off it."
  (interactive)
  (let* ((head (lambda (repo) (my/straight--head-commit (straight--repos-dir repo))))
         (versions (seq-filter
                    (lambda (cell)
                      (let ((dir (straight--repos-dir (car cell))))
                        (and (file-directory-p dir)
                             (not (file-symlink-p (directory-file-name dir)))
                             (not (equal (funcall head (car cell)) (cdr cell))))))
                    (straight--lockfile-read-all)))
         (missing (seq-keep
                   (lambda (cell)
                     (unless (my/straight--git-output
                              (straight--repos-dir (car cell))
                              "cat-file" "-e" (concat (cdr cell) "^{commit}"))
                       (car cell)))
                   versions))
         (before (mapcar (lambda (cell) (cons (car cell) (funcall head (car cell))))
                         versions))
         (moved nil))
    (when versions
      (when missing (my/straight-fetch-at-once missing))
      (cl-letf (((symbol-function 'straight--lockfile-read-all) (lambda () versions)))
        (straight-thaw-versions)))
    (dolist (cell before)
      (unless (equal (cdr cell) (funcall head (car cell)))
        (straight-register-repo-modification (car cell))
        (push (car cell) moved)))
    (message "straight: %s"
             (if moved
                 (format "%d moved to the lockfile (%s); restart Emacs to rebuild them"
                         (length moved) (string-join (nreverse moved) ", "))
               "everything is at the lockfile"))))

(provide 'my-core-straight)
;;; my-core-straight.el ends here
