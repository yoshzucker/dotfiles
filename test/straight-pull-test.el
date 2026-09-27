;;; straight-pull-test.el --- Tests for the fast half of a bulk pull  -*- lexical-binding: t; -*-

;;; Commentary:

;; `my/straight--fast-forward' decides whether a repository can be brought
;; up to date with two git processes instead of straight's thirty-seven.
;; Saying yes when it should have said no rewrites somebody's repository, so
;; what these test is mostly the no: every shape that has to fall through to
;; straight does, and the one shape that does not is left where its remote
;; is.
;;
;; Against real repositories in a temporary directory, because the thing
;; being tested is what git says about a working tree -- a stub of git would
;; only test the stub.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)
(require 'subr-x)

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "straight/repos/straight.el"
                                 (or (getenv "STRAIGHT_DIR")
                                     (expand-file-name "~/.emacs.d")))))
(require 'straight)

(defvar straight-pull-test--loaded
  (my-test-load (expand-file-name
                 "emacs.d/init.el"
                 (locate-dominating-file
                  (or load-file-name buffer-file-name default-directory)
                  "emacs.d"))
                "my/straight")
  "How many definitions were taken out of init.el.")

(ert-deftest straight-pull-test-definitions-are-there ()
  "The names these tests are about still exist under those names."
  (should (> straight-pull-test--loaded 0))
  (dolist (fn '(my/straight--fast-forward
                my/straight--behind-count
                my/straight--origin-default-branch
                my/straight--origin-url
                my/straight--ordinary-recipe-p
                my/straight--recipes-by-repo
                my/straight--ref
                my/straight--head-branch
                my/straight--level-p
                my/straight--behind-repos
                my/straight-fetch-at-once
                my/straight-pull-all))
    (should (fboundp fn)))
  (should (boundp 'my/straight-fetch-timeout))
  (should (boundp 'my/straight-fetch-environment)))

;;;; A repository to try it on

(defvar straight-pull-test--root nil
  "Directory holding the origin and the clone for one test.")

(defun straight-pull-test--git (dir &rest args)
  "Run git with ARGS in DIR, failing the test if git does."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %s failed in %s: %s" (string-join args " ") dir
               (buffer-string)))
      (string-trim (buffer-string)))))

(defun straight-pull-test--commit (dir text)
  "Add a commit to DIR whose file content is TEXT."
  (with-temp-file (expand-file-name "file.txt" dir) (insert text "\n"))
  (straight-pull-test--git dir "add" "file.txt")
  (straight-pull-test--git dir "commit" "-m" text))

(defun straight-pull-test--setup (&optional behind)
  "Make an origin and a clone of it, and return the clone's directory.

The clone is BEHIND commits short of origin, three by default: the fast
path has nothing to do with a repository that is already level, so the
interesting default is one that is not."
  (let* ((root (make-temp-file "straight-pull-test" t))
         (origin (expand-file-name "origin" root))
         (repos (expand-file-name "straight/repos" root))
         (clone (expand-file-name "pkg" repos)))
    (setq straight-pull-test--root root)
    (make-directory origin t)
    (make-directory repos t)
    (straight-pull-test--git origin "init" "--initial-branch=main")
    (straight-pull-test--git origin "config" "user.email" "test@example.invalid")
    (straight-pull-test--git origin "config" "user.name" "Test")
    (straight-pull-test--commit origin "one")
    (straight-pull-test--git root "clone" origin clone)
    (straight-pull-test--git clone "config" "user.email" "test@example.invalid")
    (straight-pull-test--git clone "config" "user.name" "Test")
    (dotimes (n (or behind 3))
      (straight-pull-test--commit origin (format "remote-%d" n)))
    (straight-pull-test--git clone "fetch")
    clone))

(defun straight-pull-test--teardown ()
  "Remove what `straight-pull-test--setup' made."
  (when (and straight-pull-test--root
             (string-prefix-p temporary-file-directory straight-pull-test--root))
    (delete-directory straight-pull-test--root t))
  (setq straight-pull-test--root nil))

(defun straight-pull-test--recipe (clone &rest overrides)
  "A recipe naming CLONE's origin, with OVERRIDES merged in."
  (append overrides
          (list :package "pkg" :local-repo "pkg" :type 'git
                :repo (straight-pull-test--git clone "config" "--get"
                                               "remote.origin.url"))))

(defmacro straight-pull-test--with (clone &rest body)
  "Bind CLONE to a fresh repository, run BODY, and clean up.

`straight--repos-dir' is a function over `straight-base-dir', so pointing
that at the temporary tree is what makes the code under test look there."
  (declare (indent 1))
  `(let* ((,clone (straight-pull-test--setup))
          (straight-base-dir (file-name-as-directory straight-pull-test--root))
          (straight--process-log nil)
          (straight--process-warn nil))
     (unwind-protect (progn ,@body)
       (straight-pull-test--teardown))))

(defun straight-pull-test--level-p (clone)
  "Say whether CLONE is at the same commit as its origin."
  (equal (straight-pull-test--git clone "rev-parse" "HEAD")
         (straight-pull-test--git clone "rev-parse" "origin/main")))

;;;; What it reads without running git

(ert-deftest straight-pull-test-reads-the-default-branch ()
  "The branch origin calls default comes out of the symbolic ref file."
  (straight-pull-test--with clone
    (should (equal (my/straight--origin-default-branch clone) "main"))))

(ert-deftest straight-pull-test-no-default-branch-without-the-ref ()
  "A clone with no origin/HEAD gives no answer rather than a guess."
  (straight-pull-test--with clone
    (delete-file (expand-file-name ".git/refs/remotes/origin/HEAD" clone))
    (should-not (my/straight--origin-default-branch clone))))

(ert-deftest straight-pull-test-reads-the-origin-url ()
  "The URL comes out of .git/config."
  (straight-pull-test--with clone
    (should (equal (my/straight--origin-url clone)
                   (straight-pull-test--git clone "config" "--get"
                                            "remote.origin.url")))))

(ert-deftest straight-pull-test-no-url-when-config-includes ()
  "A config that can pull settings in from elsewhere is not read here."
  (straight-pull-test--with clone
    (let ((config (expand-file-name ".git/config" clone)))
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (insert "[include]\n\tpath = other\n")
        (write-region (point-min) (point-max) config)))
    (should-not (my/straight--origin-url clone))))

;;;; What one `git status' answers

(ert-deftest straight-pull-test-counts-how-far-behind ()
  "A clean clone three commits back says three."
  (straight-pull-test--with clone
    (let ((default-directory clone))
      (should (equal (my/straight--behind-count "main") 3)))))

(ert-deftest straight-pull-test-zero-when-level ()
  "Level with the remote is an answer of zero, not a refusal."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
    (let ((default-directory clone))
      (should (equal (my/straight--behind-count "main") 0)))))

(ert-deftest straight-pull-test-refuses-a-dirty-worktree ()
  "An edited file is not the ordinary case."
  (straight-pull-test--with clone
    (with-temp-file (expand-file-name "file.txt" clone) (insert "edited\n"))
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-an-untracked-file ()
  "Nor is a file git has never been told about."
  (straight-pull-test--with clone
    (with-temp-file (expand-file-name "stray.txt" clone) (insert "stray\n"))
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-a-diverged-branch ()
  "A commit of its own means the merge would not be a fast-forward."
  (straight-pull-test--with clone
    (straight-pull-test--commit clone "local")
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-another-branch ()
  "HEAD somewhere other than the branch asked about."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "checkout" "-b" "side")
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-another-branch-tracking-the-same-remote ()
  "A branch by another name, even where it tracks the right remote branch.

straight would check the default branch out first, and what is behind
`origin/main\=' here is `other\=', so merging in place would leave the
repository on a branch straight did not put it on."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "checkout" "-b" "other" "--track" "origin/main")
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-a-branch-tracking-elsewhere ()
  "The count is against whatever the branch tracks, which need not be origin.

Without this the count could say three behind `other/main\=' and the merge
would then fast-forward to `origin/main\=', which is a different commit and
a question nobody asked."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "remote" "add" "other"
                             (expand-file-name "origin" straight-pull-test--root))
    (straight-pull-test--git clone "fetch" "other")
    (straight-pull-test--git clone "branch" "--set-upstream-to=other/main" "main")
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

(ert-deftest straight-pull-test-refuses-a-detached-head ()
  "A detached HEAD has no branch and no tracking branch."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "checkout" "--detach" "HEAD")
    (let ((default-directory clone))
      (should-not (my/straight--behind-count "main")))))

;;;; A fetch that never answers

(ert-deftest straight-pull-test-gives-up-on-a-fetch-that-hangs ()
  "One repository that never answers does not become the whole command.

A `git fetch' with nobody to ask waits for an answer that cannot
arrive -- for credentials, or for an unknown host key to be accepted --
and says nothing while it waits, so with sixteen in flight the count
simply stops and the only way out is `C-g'.  git is told not to ask; this
is the net under that, for every other way a fetch can stop answering.

The git here is a script that sleeps, which is the same thing from the
outside and does not need a network to arrange."
  (straight-pull-test--with clone
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (my/straight-fetch-timeout 1)
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nsleep 30\n"))
      (set-file-modes fake #o755)
      (let* ((began (float-time))
             (result (my/straight-fetch-at-once))
             (took (- (float-time) began)))
        (should (member "pkg" (plist-get result :abandoned)))
        (should (member "pkg" (plist-get result :refused)))
        (should-not (plist-get result :moved))
        ;; Back well inside the sleep it would otherwise have waited out.
        (should (< took 15))))))

(ert-deftest straight-pull-test-the-fetches-run-with-that-environment ()
  "And the fetches are actually started with it.

Naming the variables somewhere is not the same as git being handed them,
and it is the handing over that stops the hang.  So the git here writes
down what it was given."
  (straight-pull-test--with clone
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (seen (expand-file-name "seen" straight-pull-test--root))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake
        (insert "#!/bin/sh\n"
                "printf '%s|%s|%s\\n' \"$GIT_TERMINAL_PROMPT\" \"$GIT_SSH_COMMAND\" \"$*\" > "
                seen "\n"))
      (set-file-modes fake #o755)
      (my/straight-fetch-at-once)
      (should (file-readable-p seen))
      (let ((line (with-temp-buffer (insert-file-contents seen)
                                    (buffer-string))))
        (should (string-prefix-p "0|" line))
        (should (string-match-p "BatchMode=yes" line))
        ;; Most of what is fetched here comes over HTTP with no account
        ;; involved, and git has no timeout of its own there.
        (should (string-match-p "http\\.lowSpeedLimit=[0-9]+" line))
        ;; And soon enough to matter: a stalled transfer has to give up
        ;; before `my/straight-fetch-timeout\=' kills it, so what comes back
        ;; is git\='s own account of the failure rather than a process that
        ;; was cut off saying nothing.
        (should (string-match "http\\.lowSpeedTime=\\([0-9]+\\)" line))
        (should (< (string-to-number (match-string 1 line))
                   my/straight-fetch-timeout))))))

(ert-deftest straight-pull-test-does-not-need-anything-to-be-notified ()
  "Neither ending nor classifying waits on a process sentinel.

A tally kept by sentinels can be lost, and then the loop spins on a
number that never comes down -- a hang the timeout cannot reach, since
there is no live process left to kill.  Deriving the count fixes that
and breaks the other half: a process is dead before its sentinel runs,
so a loop that ends the moment nothing is live can end before anything
has been told how it ended, and a failed fetch is quietly counted a
success.  Asking the process settles both.

Every sentinel is taken away here, which is that at its worst, and the
fetch still has to come back knowing it failed."
  (straight-pull-test--with clone
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (real (symbol-function 'make-process)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nexit 1\n"))
      (set-file-modes fake #o755)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (let ((rest args) (kept nil))
                     (while rest
                       (unless (eq (car rest) :sentinel)
                         (setq kept (append kept (list (car rest) (cadr rest)))))
                       (setq rest (cddr rest)))
                     (apply real kept)))))
        (let ((result (with-timeout (10 :hung) (my/straight-fetch-at-once))))
          (should-not (eq result :hung))
          (should (member "pkg" (plist-get result :refused))))))))

(ert-deftest straight-pull-test-notices-that-something-arrived ()
  "A fetch that says a ref moved is reported as having moved one.

git is silent onto a pipe when nothing changed and prints when
something did, which is how this is known without asking a second
time.  What it prints reaches the filter, so being told is a matter of
that output having been read before the answer is worked out."
  (straight-pull-test--with clone
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake
        (insert "#!/bin/sh\n"
                "echo '   abc1234..def5678  main -> origin/main'\n"
                "exit 0\n"))
      (set-file-modes fake #o755)
      (let ((result (my/straight-fetch-at-once)))
        (should (equal (plist-get result :moved) '("pkg")))
        (should-not (plist-get result :refused))))))

(ert-deftest straight-pull-test-says-nothing-moved-when-git-is-silent ()
  "And one that prints nothing is not."
  (straight-pull-test--with clone
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nexit 0\n"))
      (set-file-modes fake #o755)
      (let ((result (my/straight-fetch-at-once)))
        (should-not (plist-get result :moved))
        (should-not (plist-get result :refused))))))

(ert-deftest straight-pull-test-tells-git-not-to-ask ()
  "The environment the fetches run in leaves git nobody to ask."
  (should (member "GIT_TERMINAL_PROMPT=0" my/straight-fetch-environment))
  (should (seq-find (lambda (entry)
                      (and (string-prefix-p "GIT_SSH_COMMAND=" entry)
                           (string-match-p "BatchMode=yes" entry)))
                    my/straight-fetch-environment)))

(ert-deftest straight-pull-test-says-which-repositories-were-not-reached ()
  "A repository the fetch could not reach is named at the end.

It is otherwise invisible twice over.  The fetch says so and the merge
talks over it a moment later; and a repository that was not fetched is
indistinguishable, afterwards, from one with nothing to fetch -- it is
where its remote was last known to be, so nothing merges it and nothing
mentions it.  This is the case that has to be tested with the repository
level, because that is the one where the merge would otherwise say
nothing at all."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
    (let* ((bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (said nil))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nexit 1\n"))
      (set-file-modes fake #o755)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) said)))))
        (my/straight-pull-all))
      (should (seq-find (lambda (line)
                          (and (string-match-p "not reached" line)
                               (string-match-p "pkg" line)))
                        said)))))

(ert-deftest straight-pull-test-names-them-alongside-what-did-merge ()
  "Including when there was something to merge, which is the other message.

A repository can be both: out of reach now and behind from before, in
which case it still merges from the refs it already has -- and is still
worth naming, because what it merged is not what is on the remote."
  (straight-pull-test--with clone
    (let* ((real (executable-find "git"))
           (bin (expand-file-name "bin" straight-pull-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (said nil))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      ;; Refuses to fetch and does everything else, which is the shape of a
      ;; repository whose remote cannot be reached from here.
      (with-temp-file fake
        (insert "#!/bin/sh\n"
                ;; The subcommand is no longer argv[1]: there are -c
                ;; options in front of it now.
                "for a; do [ \"$a\" = fetch ] && exit 1; done\n"
                "exec " real " \"$@\"\n"))
      (set-file-modes fake #o755)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) said)))))
        (my/straight-pull-all))
      (should (straight-pull-test--level-p clone))
      (should (seq-find (lambda (line)
                          (and (string-match-p "merged 1 of 1" line)
                               (string-match-p "not reached: pkg" line)))
                        said)))))

;;;; What the ref files say, with no git at all

(ert-deftest straight-pull-test-reads-a-loose-ref ()
  "A branch that is still a file of its own."
  (straight-pull-test--with clone
    (should (equal (my/straight--ref clone "refs/heads/main")
                   (straight-pull-test--git clone "rev-parse" "HEAD")))))

(ert-deftest straight-pull-test-reads-a-packed-ref ()
  "And one git has tidied away into packed-refs."
  (straight-pull-test--with clone
    (let ((head (straight-pull-test--git clone "rev-parse" "HEAD")))
      (straight-pull-test--git clone "pack-refs" "--all")
      (should-not (file-exists-p (expand-file-name ".git/refs/heads/main" clone)))
      (should (equal (my/straight--ref clone "refs/heads/main") head)))))

(ert-deftest straight-pull-test-reads-the-branch-head-is-on ()
  "HEAD names a branch, until it does not."
  (straight-pull-test--with clone
    (should (equal (my/straight--head-branch clone) "main"))
    (straight-pull-test--git clone "checkout" "--detach" "HEAD")
    (should-not (my/straight--head-branch clone))))

(ert-deftest straight-pull-test-level-is-the-two-refs-agreeing ()
  "Behind is not level; caught up is."
  (straight-pull-test--with clone
    (should-not (my/straight--level-p clone "main"))
    (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
    (should (my/straight--level-p clone "main"))))

(ert-deftest straight-pull-test-level-wants-head-on-that-branch ()
  "A branch level with its remote, while HEAD is somewhere else."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
    (straight-pull-test--git clone "checkout" "-q" "--detach" "HEAD")
    (should-not (my/straight--level-p clone "main"))))

(ert-deftest straight-pull-test-behind-repos-does-not-need-the-fetch-to-say-so ()
  "A repository the last run left behind is found again on the next one.

This is the whole reason the merge asks what is behind rather than what
the fetch brought: the second fetch of an already-fetched ref brings
nothing, so a merge that did not happen would never be retried."
  (straight-pull-test--with clone
    (let ((straight--recipe-cache (make-hash-table :test #'equal)))
      (puthash "pkg" (straight-pull-test--recipe clone) straight--recipe-cache)
      (should (equal (my/straight--behind-repos) '("pkg")))
      (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
      (should-not (my/straight--behind-repos)))))

;;;; Which recipes count as ordinary

(ert-deftest straight-pull-test-plain-recipe-is-ordinary ()
  "A recipe naming the same place, with nothing else asked for."
  (straight-pull-test--with clone
    (should (my/straight--ordinary-recipe-p
             (straight-pull-test--recipe clone) "main"
             (my/straight--origin-url clone)))))

(ert-deftest straight-pull-test-fork-is-not-ordinary ()
  "A fork has a second remote to merge from."
  (straight-pull-test--with clone
    (should-not (my/straight--ordinary-recipe-p
                 (straight-pull-test--recipe clone :fork t) "main"
                 (my/straight--origin-url clone)))))

(ert-deftest straight-pull-test-other-branch-is-not-ordinary ()
  "A recipe asking for a branch that is not checked out."
  (straight-pull-test--with clone
    (should-not (my/straight--ordinary-recipe-p
                 (straight-pull-test--recipe clone :branch "develop") "main"
                 (my/straight--origin-url clone)))))

(ert-deftest straight-pull-test-other-url-is-not-ordinary ()
  "A recipe whose URL has drifted from the clone's."
  (straight-pull-test--with clone
    (should-not (my/straight--ordinary-recipe-p
                 (straight-pull-test--recipe clone :repo "/nowhere/else.git")
                 "main" (my/straight--origin-url clone)))))

;;;; The whole decision

(ert-deftest straight-pull-test-fast-forwards-the-ordinary-case ()
  "Behind, clean, on the default branch: brought level."
  (straight-pull-test--with clone
    (should-not (straight-pull-test--level-p clone))
    (should (my/straight--fast-forward "pkg" (list (straight-pull-test--recipe clone))))
    (should (straight-pull-test--level-p clone))))

(ert-deftest straight-pull-test-tells-straight-the-repository-moved ()
  "A merge straight did not do is one straight has to be told about.

It rebuilds a package whose repository changed, and on Windows the only
thing that tells it so is this marker -- the startup walk that would
otherwise notice costs half a minute there and is turned off."
  (straight-pull-test--with clone
    (let ((marker (expand-file-name "straight/modified/pkg"
                                    straight-pull-test--root))
          (straight-safe-mode nil))
      (should-not (file-exists-p marker))
      (should (my/straight--fast-forward
               "pkg" (list (straight-pull-test--recipe clone))))
      (should (file-exists-p marker)))))

(ert-deftest straight-pull-test-says-yes-when-already-level ()
  "Nothing to merge is still nothing for straight to do."
  (straight-pull-test--with clone
    (straight-pull-test--git clone "merge" "--ff-only" "origin/main")
    (should (my/straight--fast-forward "pkg" (list (straight-pull-test--recipe clone))))))

(ert-deftest straight-pull-test-hands-back-a-dirty-repository ()
  "Work in the worktree goes to straight, and is left alone."
  (straight-pull-test--with clone
    (with-temp-file (expand-file-name "file.txt" clone) (insert "edited\n"))
    (should-not (my/straight--fast-forward "pkg" (list (straight-pull-test--recipe clone))))
    (should-not (straight-pull-test--level-p clone))))

(ert-deftest straight-pull-test-hands-back-anything-half-finished ()
  "Every state git leaves a marker for is straight's to ask about.

Only two of these does git refuse to merge over of its own accord.  It
will fast-forward straight across an interrupted revert, bisect or
rebase, which is exactly the kind of thing straight stops and asks
about -- so the marker has to be looked for here rather than left to
git to complain."
  (dolist (marker '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD"
                    "BISECT_LOG" "rebase-apply" "rebase-merge"))
    (straight-pull-test--with clone
      (let ((path (expand-file-name (concat ".git/" marker) clone)))
        (if (member marker '("rebase-apply" "rebase-merge"))
            (make-directory path)
          (with-temp-file path
            (insert (straight-pull-test--git clone "rev-parse" "HEAD") "\n"))))
      (should-not (my/straight--fast-forward
                   "pkg" (list (straight-pull-test--recipe clone))))
      (should-not (straight-pull-test--level-p clone)))))

(ert-deftest straight-pull-test-hands-back-a-repository-with-submodules ()
  "Submodules need updating after the merge, which is not done here.

The file is hidden from `git status\=' on purpose: an untracked file would
make the worktree dirty, and then this would pass without the check it is
about ever running."
  (straight-pull-test--with clone
    (with-temp-file (expand-file-name ".gitmodules" clone) (insert "\n"))
    (with-temp-file (expand-file-name ".git/info/exclude" clone)
      (insert ".gitmodules\n"))
    (let ((default-directory clone))
      (should (equal (my/straight--behind-count "main") 3)))
    (should-not (my/straight--fast-forward "pkg" (list (straight-pull-test--recipe clone))))
    (should-not (straight-pull-test--level-p clone))))

(ert-deftest straight-pull-test-hands-back-a-symlinked-checkout ()
  "A clone that is a link to a checkout of mine is straight's to ask about.

`my-app-agent.el\=' and the rest point straight at repositories under
~/Developer, where a branch of my own is the point rather than a
surprise -- so the questions straight asks about them are the ones worth
asking, even when the shape looks ordinary."
  (straight-pull-test--with clone
    (let ((elsewhere (expand-file-name "elsewhere" straight-pull-test--root)))
      (rename-file clone elsewhere)
      (make-symbolic-link elsewhere clone)
      (should-not (my/straight--fast-forward
                   "pkg" (list (straight-pull-test--recipe clone))))
      (should-not (straight-pull-test--level-p clone)))))

(ert-deftest straight-pull-test-hands-back-when-one-recipe-of-several-is-not-ordinary ()
  "Packages share a checkout, so one awkward recipe speaks for the clone."
  (straight-pull-test--with clone
    (should-not (my/straight--fast-forward
                 "pkg" (list (straight-pull-test--recipe clone)
                             (straight-pull-test--recipe clone :fork t))))
    (should-not (straight-pull-test--level-p clone))))

(ert-deftest straight-pull-test-hands-back-with-no-recipe-at-all ()
  "A clone nothing declares is not one to touch."
  (straight-pull-test--with clone
    (should-not (my/straight--fast-forward "pkg" nil))
    (should-not (straight-pull-test--level-p clone))))

(provide 'straight-pull-test)
;;; straight-pull-test.el ends here
