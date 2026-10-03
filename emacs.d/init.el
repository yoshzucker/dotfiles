;;; init.el --- Minimal Emacs initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el and load configuration.

;;; Code:

;; Left at the standard nil: Org signals ordinary refusals with `error' -- "the
;; span is already day", "command not allowed in this line" -- and with this on,
;; each of them opens a backtrace over the buffer you were reading.  Debugging
;; wants it, so `M-x toggle-debug-on-error' turns it on for as long as it helps,
;; and `emacs --debug-init' still covers startup.

;; What `load' probes for, on a machine where probing is expensive.
;;
;; Emacs finds a library by walking `load-path' and trying, in each directory,
;; every combination of `load-suffixes' and `load-file-rep-suffixes'.  With Auto
;; Compression mode on -- it is on by default -- that means `.elc' `.elc.gz'
;; `.el' `.el.gz' and the module suffixes doubled the same way: eight probes per
;; directory.  straight puts one build directory per package at the front of the
;; walk, and Emacs's own lisp sits at the end, so every bundled library is found
;; only after several hundred probes for names that were never going to be there.
;;
;; That is most of the startup on Windows, where one probe costs 0.22ms against
;; 0.003ms on macOS.  Measured there: 172 directories, 143ms to look one library
;; up, and `mail-prsvr' -- thirty-three lines -- taking 0.10s of a 39.7s startup,
;; all of it spent arriving.  Without the compressed half the same lookup took
;; 67ms.
;;
;; `jka-compr-load-suffixes' is the setting that adds that half, and setting it
;; through Custom runs `jka-compr-update', so the derived `load-file-rep-suffixes'
;; follows instead of being poked behind its owner's back.  Auto Compression mode
;; stays on: .gz files open and save exactly as before, and only the search for a
;; *library* stops considering them.
;;
;; Sound only where no lisp is shipped gzipped, which is worth testing rather
;; than assuming -- a distribution compresses all of its lisp or none of it, and
;; `subr.el' is the one file all of them have.
(let* ((subr (locate-library "subr"))
       (lisp (and subr (file-name-directory subr))))
  (when (and lisp (not (file-exists-p (expand-file-name "subr.el.gz" lisp))))
    (setopt jka-compr-load-suffixes nil)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; How straight decides a package needs rebuilding.  Must come before the first
;; `straight-use-package' below, and needs nothing earlier than that.
;;
;; The default includes `find-at-startup', and the bulk walk it names does not
;; happen when straight loads -- `straight--modifications' is a `memq' over
;; this variable, consulted per package inside
;; `straight--package-might-be-modified-p', and the walk itself is
;; `straight--make-package-modifications-available', a transaction step that
;; runs once, for the first package checked.  That first package is
;; `use-package', two lines down, which is why setting this after it had no
;; effect: the walk was already done and cached for the rest of the session.
;;
;; It is worth avoiding.  On Windows it walks every file of every cloned
;; repository before anything is loaded: thirty-one seconds to start, against
;; a sixth of a second for `emacs -Q'.  `check-on-save' catches every edit made
;; in this Emacs, which is all of them in ordinary use; `find-when-checking'
;; still walks a repository, but only the one being asked about and only when
;; it is asked about.  What is given up is noticing a repository edited by
;; something else while Emacs was not looking, which `M-x straight-check-all'
;; answers on demand.
;;
;; `check-on-save' is two things, and only one of them is wanted here.  It is
;; the `before-save-hook' that marks a repository modified when a file inside
;; it is saved -- and it is also what makes straight read those marks back at
;; startup, which is how `my/straight-pull-all' gets what it fast-forwards
;; rebuilt.  So it stays in the list, and the hook is taken off: no package is
;; edited on this machine, and the hook costs every save two `file-truename's,
;; twenty or thirty milliseconds on Windows, to find that the file is not in a
;; package.  Should one be edited here, `M-x straight-rebuild-package' is the
;; step the hook would have taken.
(when (eq system-type 'windows-nt)
  (setq straight-check-for-modifications '(check-on-save find-when-checking))
  (straight-live-modifications-mode -1))

;; Where Emacs's own lisp sits in `load-path'.
;;
;; A library is found by walking `load-path' in order, probing each directory
;; for each candidate suffix.  straight prepends one build directory per
;; package, so Emacs's own lisp ends up behind all of them, and every bundled
;; library is found only after probing every one: measured on Windows at 67ms a
;; lookup against 0.4ms per directory, which was most of what a startup spent.
;;
;; Only a package that ships a library Emacs also bundles needs to be ahead of
;; Emacs's lisp.  Those few stay in front; the rest go behind it, leaving
;;
;;     [the shadowing packages] [Emacs's own lisp] [everything else]
;;
;; Behind, but at the head of it.  straight prepending is not arbitrary: a
;; package is required immediately after it is registered, so the front is
;; exactly where the directory about to be searched belongs, and appending
;; instead put every package at the back of the queue at the one moment it was
;; wanted -- measured at 57ms to find a package against 0.4ms before.
;;
;; Getting that list wrong is quiet.  The bundled copy wins, the installed
;; package becomes unreachable, and for something like `compat' that looks like
;; nothing at all until a function turns out to be missing.  So the list is
;; checked rather than trusted: `my/straight-warn-if-shadowing' runs on
;; `straight-use-package-post-build-functions', which fires on every install and
;; on every rebuild after a pull -- the only moments the answer can change.

(defconst my/emacs-own-load-path
  (let ((ours (expand-file-name user-emacs-directory)))
    (seq-remove (lambda (dir) (string-prefix-p ours (expand-file-name dir)))
                load-path))
  "Every directory of Emacs's bundled lisp, and nothing else.
Read before straight has added a package, so whatever lives under
`user-emacs-directory' at this point belongs to straight rather than Emacs.")

(defconst my/straight-packages-before-emacs-lisp
  '("bind-key" "compat" "eglot" "eldoc" "external-completion" "flymake"
    "jsonrpc" "let-alist" "map" "org" "peg" "project" "seq" "svg"
    "transient" "use-package" "xref")
  "Packages that must stay ahead of Emacs's own lisp in `load-path'.
Each ships at least one library Emacs also bundles, so behind Emacs's lisp
the bundled copy would win and the installed package would be unreachable.
Every other package straight builds is appended instead.")

(defun my/emacs-bundled-libraries ()
  "Return a hash table whose keys name every library Emacs itself bundles."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (dir my/emacs-own-load-path names)
      (dolist (file (ignore-errors (directory-files dir nil "\\.elc?\\'")))
        (puthash (file-name-base file) t names)))))

(defun my/straight-shadowed-libraries (package &optional bundled)
  "Return the libraries PACKAGE ships that Emacs also bundles.
BUNDLED is a table from `my/emacs-bundled-libraries', built here when
omitted.  Pass one when asking about many packages: building it walks every
directory of Emacs's lisp."
  (let ((bundled (or bundled (my/emacs-bundled-libraries)))
        (dir (straight--build-dir package))
        found)
    (dolist (file (and (file-directory-p dir)
                       (directory-files dir nil "\\.el\\'"))
                  (nreverse found))
      (let ((name (file-name-base file)))
        (when (gethash name bundled)
          (push name found))))))

(defun my/straight-warn-if-shadowing (package &rest _)
  "Warn when PACKAGE shadows Emacs's own lisp without being declared to."
  (unless (member package my/straight-packages-before-emacs-lisp)
    (when-let* ((shadowed (my/straight-shadowed-libraries package)))
      (display-warning
       'straight
       (format (concat "%s ships %s, which Emacs also bundles.  Its build "
                       "directory is appended to `load-path', so Emacs's copy "
                       "wins and the installed %s is unreachable.\n"
                       "Add %S to `my/straight-packages-before-emacs-lisp'.")
               package (string-join shadowed ", ") package package)
       :warning))))

(add-hook 'straight-use-package-post-build-functions
          #'my/straight-warn-if-shadowing)

(defun my/straight-check-load-path-shadows ()
  "Report every built package that shadows Emacs's own lisp undeclared.
The post-build hook covers packages built from here on; this covers the ones
already sitting in straight's build directory, which it has no reason to
rebuild."
  (interactive)
  (let* ((bundled (my/emacs-bundled-libraries))
         (build (straight--build-dir))
         (packages (seq-filter
                    (lambda (name)
                      (file-directory-p (expand-file-name name build)))
                    (directory-files build nil "\\`[^.]")))
         (undeclared
          (seq-filter
           (lambda (package)
             (and (not (member package my/straight-packages-before-emacs-lisp))
                  (my/straight-shadowed-libraries package bundled)))
           packages)))
    (if undeclared
        (message "Shadowing Emacs's own lisp, undeclared: %s"
                 (string-join undeclared " "))
      (message "No package shadows Emacs's own lisp undeclared (%d checked)"
               (length packages)))))

(define-advice straight--add-package-to-load-path
    (:around (orig recipe) my/behind-emacs-own-lisp)
  "Add the package's directory just behind Emacs's own lisp.
A package that has to shadow Emacs's lisp is prepended as straight would.
Anything else lands at the head of what follows Emacs's lisp, so it is still
the first package directory searched when it is required a moment later."
  (let* ((package (plist-get recipe :package))
         (dir (directory-file-name (straight--build-dir package)))
         (last-own (car (last my/emacs-own-load-path)))
         (at (and last-own (seq-position load-path last-own))))
    (cond
     ((member dir load-path) load-path)
     ;; No recognisable block of Emacs's own lisp to sit behind -- leave the
     ;; ordering to straight rather than guess at a position.
     ((or (null at)
          (member package my/straight-packages-before-emacs-lisp))
      (funcall orig recipe))
     (t (setq load-path (append (seq-take load-path (1+ at))
                                (list dir)
                                (seq-drop load-path (1+ at))))))))

;; Updating every package, without waiting for each in turn.
;;
;; `straight-pull-all' is `straight-fetch-all' followed by
;; `straight-merge-all', and the first of those is the whole of the wait: one
;; `git fetch' per repository, one after the next, each paying a network round
;; trip.  Measured at a second apiece over a hundred and ninety repositories,
;; and the same second whether the repository is four megabytes or forty --
;; the cost is the handshake and not the history, which is also why a shallow
;; clone would not help.  Three minutes of it, and the merging that follows
;; never touches the network at all.

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

(defun my/straight-fetch-at-once ()
  "Run `git fetch' in every straight repository, several at a time.

Return a plist.  `:refused' names the repositories git would not fetch,
newest first; `:abandoned' the ones killed for taking longer than
`my/straight-fetch-timeout'; `:moved' the ones something actually
arrived in.  The last is for the report and not for the merge, which
asks what is behind instead -- see `my/straight--behind-repos'.  git
says it for free: onto a pipe it prints when a ref moves and is silent
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
  (let* (;; The echo area is this command\='s while it runs.  A progress
         ;; report that something else overwrites four times a second is
         ;; worse than none: the eye reads flicker as a fault.  Garbage
         ;; collection is the one that narrates over it here --
         ;; `garbage-collection-messages\=' is on globally, which is right
         ;; for a session and wrong for a command that has taken the line.
         (garbage-collection-messages nil)
         ;; And fewer of them to narrate.  Collecting does not stop the
         ;; fetching -- git is a separate process and goes on writing while
         ;; Emacs is busy -- it stops Emacs noticing that a fetch finished
         ;; and starting the next.  Measured at one collection of ten
         ;; milliseconds across the whole of a hundred and ninety
         ;; repositories, so this is for the quiet rather than the speed.
         ;;
         ;; Only until something lowers it again: `gcmh-mode\=' drops the
         ;; threshold from its own idle timer, and a wait this long looks
         ;; idle.  What that costs is collections, not messages, and they
         ;; are silent now.
         (gc-cons-threshold (max gc-cons-threshold (* 256 1024 1024)))
         ;; The repositories a recipe points at.  Walking the directory
         ;; instead fetched
         ;; every clone that has ever been made here -- fifty-seven of a
         ;; hundred and ninety-one were for packages nothing declares any
         ;; more, and each one is a round trip to a server for an answer
         ;; nothing reads.
         (wanted (my/straight--recipes-by-repo))
         (queue (seq-filter
                 (lambda (dir)
                   (and (gethash (file-name-nondirectory dir) wanted)
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
         ;; and nothing at all when none did, which is the question the merge
         ;; half wants answered and costs no process to ask.  Asking it
         ;; instead with `git rev-parse --remotes' before and after was two
         ;; more processes per repository -- eight milliseconds each here and
         ;; a multiple of that on a machine where spawning is the expense,
         ;; which is exactly the machine this is for.
         (spoke (make-hash-table :test #'equal)))
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
            (while (and queue
                        (< (seq-count #'process-live-p procs)
                           my/straight-fetch-jobs))
              (let* ((dir (pop queue))
                     (name (file-name-nondirectory (directory-file-name dir)))
                     (default-directory dir))
                (push
                 (make-process
                  :name (concat "straight-fetch-" name)
                  ;; Aborted rather than waited on when the bytes stop
                  ;; arriving: git has no timeout of its own over HTTP, so a
                  ;; transfer that stalls part way -- a proxy that inspects
                  ;; every connection, a link that drops -- waits forever.
                  ;; Under a kilobyte a second for twenty seconds is not a
                  ;; slow clone, it is a dead one.
                  :command '("git"
                             "-c" "http.lowSpeedLimit=1024"
                             "-c" "http.lowSpeedTime=20"
                             "fetch")
                  :noquery t
                  :connection-type 'pipe
                  :buffer nil
                  ;; Drained rather than read: what it says does not matter,
                  ;; only that it said anything, and a process whose output
                  ;; nobody takes can block on a full pipe.
                  :filter (lambda (_proc _chunk) (puthash name t spoke)))
                 procs)
                (process-put (car procs) 'my/straight-repo name)
                (process-put (car procs) 'my/straight-began (float-time))))
            ;; Short, because this loop is also what starts the next process
            ;; as each one finishes.
            (accept-process-output nil 0.05)
            (progress-reporter-update
             reporter (- total (length queue)
                         (seq-count #'process-live-p procs)))
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
              (if (eq 0 (process-exit-status proc))
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

;; Merging a repository that is simply behind.
;;
;; `straight-merge-package' asks git thirty-seven questions to fast-forward
;; one repository -- the merge is one of them, four more ask what origin
;; calls its default branch and get the same answer four times, and the
;; whole interrogation runs twice because the function that does the merge
;; returns nil afterwards to force a re-check.  Measured at 0.30s here and a
;; quarter of a second per process on Windows, so nine seconds a repository
;; and most of five minutes for a pull that moved thirty-one of them.
;;
;; Nearly all of those questions have the same answer every time, because
;; nearly every repository is in the same state: a clean worktree on the
;; branch the remote calls default, with commits on the remote and none of
;; its own.  `git status --porcelain=v2 --branch' says all four of those
;; things in one process, and what git leaves out of it -- a merge or a
;; rebase half-finished, submodules, which URL origin points at -- is files
;; on disk, which cost nothing to read.
;;
;; So: two processes where the repository is plainly behind, and everything
;; else handed to straight exactly as before.  The fall-through is the whole
;; safety argument -- a repository that has diverged, has work in it, is on
;; another branch or is not shaped like these assume still gets every
;; question straight would have asked, and the same popup to answer it.

(defconst my/straight--interrupted-files
  '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD" "BISECT_LOG"
    "rebase-merge" "rebase-apply")
  "Names under .git that mean git is part-way through something.
straight asks about these with `git ls-files --unmerged' and a look at the
git directory; they are files, so looking is free.")

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

(defun my/straight--origin-url (dir)
  "Return the URL origin is set to in the repository at DIR.

Out of .git/config rather than out of git, which makes this free and also
fallible -- so it answers only where the file is plain.  Two sections for
the same remote, or an `include' that could put the URL somewhere else,
and this returns nil, which sends the repository to straight rather than
letting a misread URL decide anything."
  (let ((config (expand-file-name ".git/config" dir)))
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (unless (re-search-forward "^\\[include" nil t)
          (goto-char (point-min))
          (when (re-search-forward "^\\[remote \"origin\"\\][ \t]*$" nil t)
            (let ((body (point))
                  (end (save-excursion
                         (or (and (re-search-forward "^\\[" nil t)
                                  (match-beginning 0))
                             (point-max)))))
              (unless (save-excursion
                        (re-search-forward "^\\[remote \"origin\"\\][ \t]*$"
                                           nil t))
                (goto-char body)
                (when (re-search-forward "^[ \t]*url[ \t]*=[ \t]*\\(.+\\)$"
                                         end t)
                  (string-trim (match-string 1)))))))))))

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

(defun my/straight--level-p (dir branch)
  "Non-nil when DIR is on BRANCH and BRANCH is where origin\='s is.

The question that decides whether a repository is worth spending a
process on, and it costs none to ask: three files say where HEAD is and
what two refs point at.  So it can be asked of every repository there
is, which is the point -- see `my/straight--behind-repos\='."
  (and (equal (my/straight--head-branch dir) branch)
       (let ((here (my/straight--ref dir (concat "refs/heads/" branch)))
             (there (my/straight--ref dir (concat "refs/remotes/origin/" branch))))
         (and here there (equal here there)))))

(defun my/straight--behind-repos ()
  "Return the local repositories that are not where their remote is.

Every declared one is looked at, because looking is free.

Which is the fix for asking the fetch instead.  A fetch says what it
brought, and a repository whose merge then did not happen -- it asked
something and the answer was to skip it, or Emacs was quit, or the merge
was never reached -- is brought nothing the next time either, because the
ref is already there.  The fetch would never mention it again and the
repository would sit a version behind for good.  What is behind is a
question that answers itself correctly however the last run ended."
  (let (repos)
    (maphash (lambda (repo _recipes)
               (let* ((dir (straight--repos-dir repo))
                      (branch (my/straight--origin-default-branch dir)))
                 (unless (and branch (my/straight--level-p dir branch))
                   (push repo repos))))
             (my/straight--recipes-by-repo))
    (sort repos #'string<)))

(defun my/straight--ordinary-recipe-p (recipe checked-out url)
  "Say whether RECIPE asks for nothing beyond CHECKED-OUT tracking origin at URL.

Everything straight would go on to reconcile is a reason to say no here:
a fork has a second remote to merge from, a `:branch' or `:remote' other
than what is checked out means straight would move something, and a URL
that has drifted from the recipe means straight would reset it before
merging.  The URL comparison is straight's own, so a recipe that says ssh
where the clone says https still counts as naming the same place."
  (let ((branch (plist-get recipe :branch)))
    (and (eq (or (plist-get recipe :type) straight-default-vc) 'git)
         (null (plist-get recipe :fork))
         (equal (or (plist-get recipe :remote)
                    straight-vc-git-default-remote-name)
                "origin")
         (or (null branch) (equal branch checked-out))
         (ignore-errors
           (straight-vc-git--urls-compatible-p
            url (straight-vc-git--encode-url (plist-get recipe :repo)
                                             (plist-get recipe :host)
                                             (plist-get recipe :protocol)))))))

(defun my/straight--behind-count (branch)
  "Return how far `default-directory' is behind BRANCH's remote, or nil.

nil for anything that is not simply behind: work in the worktree, a
commit the remote has not got, a HEAD somewhere other than BRANCH, or no
tracking branch at all.  Zero is an answer and not a refusal -- the
repository is where the remote is and there is nothing to merge.

One process for the lot.  `--porcelain=v2 --branch' prefixes its headers
with `#\\=' and gives every uncommitted file, tracked or not, a line
without one, so the absence of such a line is the worktree being clean."
  (straight--process-with-result
      (straight--process-run "git" "status" "--porcelain=v2" "--branch")
    (when success
      (let ((head nil) (upstream nil) (behind nil) (clean t))
        (dolist (line (split-string (or stdout "") "\n" t))
          (cond
           ((string-match "\\`# branch\\.head \\(.+\\)\\'" line)
            (setq head (match-string 1 line)))
           ((string-match "\\`# branch\\.upstream \\(.+\\)\\'" line)
            (setq upstream (match-string 1 line)))
           ((string-match "\\`# branch\\.ab \\+0 -\\([0-9]+\\)\\'" line)
            (setq behind (string-to-number (match-string 1 line))))
           ((string-prefix-p "#" line))
           (t (setq clean nil))))
        (and clean behind
             (equal head branch)
             (equal upstream (concat "origin/" branch))
             behind)))))

(defun my/straight--fast-forward (local-repo recipes)
  "Fast-forward LOCAL-REPO if it is plainly behind origin, and say whether.

Non-nil means the repository is where its remote is and nothing further
need look at it.  nil means it is not the ordinary case, and the caller
must hand it to straight, which will work out what it is and ask.

RECIPES is every recipe naming LOCAL-REPO; all of them have to be
ordinary, because they share the one checkout."
  (let* ((dir (straight--repos-dir local-repo))
         (branch (my/straight--origin-default-branch dir))
         (url (and branch (my/straight--origin-url dir))))
    (and branch url
         (file-directory-p (expand-file-name ".git" dir))
         ;; A recipe of mine is a symlink to a checkout I edit, and the
         ;; questions straight asks about those are the ones worth asking.
         (not (file-symlink-p (directory-file-name dir)))
         (not (file-exists-p (expand-file-name ".gitmodules" dir)))
         (not (seq-some (lambda (name)
                          (file-exists-p
                           (expand-file-name (concat ".git/" name) dir)))
                        my/straight--interrupted-files))
         recipes
         (seq-every-p (lambda (recipe)
                        (my/straight--ordinary-recipe-p recipe branch url))
                      recipes)
         (let* ((default-directory dir)
                (behind (my/straight--behind-count branch)))
           (cond
            ((null behind) nil)
            ((zerop behind) t)
            (t (when (straight--process-run-p
                      "git" "merge" "--ff-only" (concat "origin/" branch))
                 ;; straight is not watching.  It rebuilds a package whose
                 ;; repository changed, and on Windows it finds out only
                 ;; from this marker: the walk that would notice at startup
                 ;; costs half a minute there and is turned off, so a merge
                 ;; nobody registers is a package that stays as it was
                 ;; built however far the repository moves.  straight\='s own
                 ;; merge writes it; so must this one.
                 (straight-register-repo-modification local-repo)
                 t)))))))

(defun my/straight-pull-all (&optional from-upstream predicate)
  "Pull all packages, fetching them all at once rather than one after another.

A drop-in for `straight-pull-all', and the same two halves in the same
order: everything is fetched, then everything is merged.  Only the fetching
is done differently, by several git processes at once, which is what turns
three minutes of round trips into a quarter of one.

Nothing about what gets asked changes.  Every question the merge puts up --
a dirty worktree, a branch that has diverged, a merge left half-done --
comes from `straight-vc-git--ensure-local', which is local by design and
runs on the merge side.  They are asked after the waiting instead of
scattered through it, which is the only difference a reader will notice.

PREDICATE filters by package name as it does there.  Given one, this hands
the whole job to `straight-pull-all': a subset is a handful of round trips,
and there is nothing in a handful for parallelism to hide.  FROM-UPSTREAM
hands it over too -- a fork has a second remote to merge from, which is
none of the ordinary case below."
  (interactive "P")
  (if (or predicate from-upstream)
      (straight-pull-all from-upstream predicate)
    (let* ((garbage-collection-messages nil)
           ;; What the fetch could not reach.  Said again here because the
           ;; fetch says it and is then talked over: the merge finishes a
           ;; moment later and its message is the one left on screen.  And
           ;; a repository that was not fetched looks, to everything after
           ;; this, exactly like one with nothing to fetch -- it is where
           ;; its remote was last known to be, so it is skipped, silently,
           ;; and stays a version behind with nothing to show for it.
           (unreachable (plist-get (my/straight-fetch-at-once) :refused))
           (aside (if unreachable
                      (format "; %d not reached: %s" (length unreachable)
                              (string-join (reverse unreachable) ", "))
                    "")))
      ;; Only the repositories that are behind.  Merging one costs
      ;; thirty-seven git processes -- the merge itself is one of them and
      ;; the rest are questions about which branch, whose remote and what is
      ;; an ancestor of what -- so asking that of a repository that is
      ;; already where its remote is costs a third of a second for a certain
      ;; answer of no.  Over a hundred and thirteen of them it is half a
      ;; minute here and four times that on Windows.
      (let ((behind (my/straight--behind-repos)))
        (if (null behind)
            (message "straight: nothing to merge%s" aside)
          (let* ((recipes (my/straight--recipes-by-repo))
                 (total (length behind))
                 (reporter (make-progress-reporter
                            (format "straight: merging %d repositories..." total)
                            0 total))
                 (began (float-time))
                 (done 0)
                 (left nil))
            (dolist (repo behind)
              (unless (my/straight--fast-forward repo (gethash repo recipes))
                (push repo left))
              (progress-reporter-update reporter (cl-incf done)))
            (progress-reporter-done reporter)
            (message "straight: merged %d of %d repositories in %.0fs%s%s"
                     (- total (length left)) total (- (float-time) began)
                     (if left
                         (format "; %d to look at: %s" (length left)
                                 (string-join (reverse left) ", "))
                       "")
                     aside)
            ;; And those, the way they have always been done.  straight
            ;; narrates one repository at a time here rather than counting,
            ;; which is the right way round now: this is the list that can
            ;; stop and ask, and a name is what the question will be about.
            ;;
            ;; `straight-merge-all' takes the predicate by package and this
            ;; list is by repository, which are not the same thing: several
            ;; packages can share one.
            (when left
              (straight-merge-all
               from-upstream
               (lambda (package)
                 (when-let* ((recipe (gethash package straight--recipe-cache))
                             (local-repo (plist-get recipe :local-repo)))
                   (member local-repo left)))))))))))

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
(corfu-terminal keeps its recipe under a window system, where its `:if' is
nil).  And a dependency is registered when its parent's recipe is, not
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


;; Install and use use-package via straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Add core and module directories to load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load essential core modules
(require 'my-core-encoding)
(require 'my-core-env)
(require 'my-core-keys)
(require 'my-core-utils)

;; Load all module files in "modules/" directory
(dolist (file (directory-files (expand-file-name "modules" user-emacs-directory) t "\\.el$"))
  (let ((feature (intern (file-name-sans-extension (file-name-nondirectory file)))))
    (require feature)))

;; Load personal configuration if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;;; init.el ends here
