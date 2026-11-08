;;; straight-fetch-test.el --- Tests for fetching every repository at once  -*- lexical-binding: t; -*-

;;; Commentary:

;; `my/straight-fetch-at-once' fetches every straight repository several at
;; a time, for `my/straight-update-lockfile' and `my/straight-apply-lockfile',
;; and the helpers beside it read refs and HEAD from the files git keeps.
;; What these test is the fetch that hangs or runs out of descriptors, what
;; it says arrived, and what the ref files say.
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

(defvar straight-fetch-test--loaded
  (my-test-load (expand-file-name
                 "emacs.d/core/my-core-straight.el"
                 (locate-dominating-file
                  (or load-file-name buffer-file-name default-directory)
                  "emacs.d"))
                "my/straight")
  "How many definitions were taken out of my-core-straight.el.")

(ert-deftest straight-fetch-test-definitions-are-there ()
  "The names these tests are about still exist under those names."
  (should (> straight-fetch-test--loaded 0))
  (dolist (fn '(my/straight--origin-default-branch
                my/straight--recipes-by-repo
                my/straight--ref
                my/straight--head-branch
                my/straight-fetch-at-once
                my/straight--head-commit
                my/straight--commits-between))
    (should (fboundp fn)))
  (should (boundp 'my/straight-fetch-timeout))
  (should (boundp 'my/straight-fetch-environment)))

;;;; A repository to try it on

(defvar straight-fetch-test--root nil
  "Directory holding the origin and the clone for one test.")

(defun straight-fetch-test--git (dir &rest args)
  "Run git with ARGS in DIR, failing the test if git does."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %s failed in %s: %s" (string-join args " ") dir
               (buffer-string)))
      (string-trim (buffer-string)))))

(defun straight-fetch-test--commit (dir text)
  "Add a commit to DIR whose file content is TEXT."
  (with-temp-file (expand-file-name "file.txt" dir) (insert text "\n"))
  (straight-fetch-test--git dir "add" "file.txt")
  (straight-fetch-test--git dir "commit" "-m" text))

(defun straight-fetch-test--setup (&optional behind)
  "Make an origin and a clone of it, and return the clone's directory.

The clone is BEHIND commits short of origin, three by default: the fast
path has nothing to do with a repository that is already level, so the
interesting default is one that is not."
  (let* ((root (make-temp-file "straight-fetch-test" t))
         (origin (expand-file-name "origin" root))
         (repos (expand-file-name "straight/repos" root))
         (clone (expand-file-name "pkg" repos)))
    (setq straight-fetch-test--root root)
    (make-directory origin t)
    (make-directory repos t)
    (straight-fetch-test--git origin "init" "--initial-branch=main")
    (straight-fetch-test--git origin "config" "user.email" "test@example.invalid")
    (straight-fetch-test--git origin "config" "user.name" "Test")
    (straight-fetch-test--commit origin "one")
    (straight-fetch-test--git root "clone" origin clone)
    (straight-fetch-test--git clone "config" "user.email" "test@example.invalid")
    (straight-fetch-test--git clone "config" "user.name" "Test")
    (dotimes (n (or behind 3))
      (straight-fetch-test--commit origin (format "remote-%d" n)))
    (straight-fetch-test--git clone "fetch")
    clone))

(defun straight-fetch-test--teardown ()
  "Remove what `straight-fetch-test--setup' made."
  (when (and straight-fetch-test--root
             (string-prefix-p temporary-file-directory straight-fetch-test--root))
    (delete-directory straight-fetch-test--root t))
  (setq straight-fetch-test--root nil))

(defun straight-fetch-test--recipe (clone &rest overrides)
  "A recipe naming CLONE's origin, with OVERRIDES merged in."
  (append overrides
          (list :package "pkg" :local-repo "pkg" :type 'git
                :repo (straight-fetch-test--git clone "config" "--get"
                                               "remote.origin.url"))))

(defmacro straight-fetch-test--with (clone &rest body)
  "Bind CLONE to a fresh repository, run BODY, and clean up.

`straight--repos-dir' is a function over `straight-base-dir', so pointing
that at the temporary tree is what makes the code under test look there."
  (declare (indent 1))
  `(let* ((,clone (straight-fetch-test--setup))
          (straight-base-dir (file-name-as-directory straight-fetch-test--root))
          (straight--process-log nil)
          (straight--process-warn nil))
     (unwind-protect (progn ,@body)
       (straight-fetch-test--teardown))))

;;;; What it reads without running git

(ert-deftest straight-fetch-test-reads-the-default-branch ()
  "The branch origin calls default comes out of the symbolic ref file."
  (straight-fetch-test--with clone
    (should (equal (my/straight--origin-default-branch clone) "main"))))

(ert-deftest straight-fetch-test-no-default-branch-without-the-ref ()
  "A clone with no origin/HEAD gives no answer rather than a guess."
  (straight-fetch-test--with clone
    (delete-file (expand-file-name ".git/refs/remotes/origin/HEAD" clone))
    (should-not (my/straight--origin-default-branch clone))))

;;;; A fetch that never answers

(ert-deftest straight-fetch-test-gives-up-on-a-fetch-that-hangs ()
  "One repository that never answers does not become the whole command.

A `git fetch' with nobody to ask waits for an answer that cannot
arrive -- for credentials, or for an unknown host key to be accepted --
and says nothing while it waits, so with sixteen in flight the count
simply stops and the only way out is `C-g'.  git is told not to ask; this
is the net under that, for every other way a fetch can stop answering.

The git here is a script that sleeps, which is the same thing from the
outside and does not need a network to arrange."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (my/straight-fetch-timeout 1)
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
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

(ert-deftest straight-fetch-test-the-fetches-run-with-that-environment ()
  "And the fetches are actually started with it.

Naming the variables somewhere is not the same as git being handed them,
and it is the handing over that stops the hang.  So the git here writes
down what it was given."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (seen (expand-file-name "seen" straight-fetch-test--root))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
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

(ert-deftest straight-fetch-test-does-not-need-anything-to-be-notified ()
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
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (real (symbol-function 'make-process)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
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

(ert-deftest straight-fetch-test-waits-when-out-of-descriptors ()
  "Running out of descriptors part way is waited out, not the end.

On Windows Emacs has sixty-four descriptors, and a fetch that has
finished still holds its two until Emacs notices.  So a fetch can fail
to start while another is running -- and starts once that one is done.
Here the second start fails the way Windows says it, once."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (other (expand-file-name "pkg2" (file-name-directory clone)))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (real (symbol-function 'make-process))
           (calls 0))
      (straight-fetch-test--git straight-fetch-test--root "clone" clone other)
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
      (puthash "pkg2" (straight-fetch-test--recipe clone :package "pkg2"
                                                  :local-repo "pkg2")
               straight--recipe-cache)
      (make-directory bin t)
      ;; Long enough that the first is still running when the second fails.
      (with-temp-file fake (insert "#!/bin/sh\nsleep 1\n"))
      (set-file-modes fake #o755)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (if (= (cl-incf calls) 2)
                       (signal 'file-error '("Creating pipe" "Too many open files"))
                     (apply real args)))))
        (let ((result (with-timeout (10 :hung) (my/straight-fetch-at-once))))
          (should-not (eq result :hung))
          (should-not (plist-get result :refused))
          (should (= calls 3)))))))

(ert-deftest straight-fetch-test-lets-go-of-finished-fetches ()
  "A finished fetch gives its pipes back before the next one needs them.

On Windows a dead process kept its descriptors long enough that a fifth
of the way through there were none left, with nothing of ours running.
Here a fetch cannot start while any earlier one has not been deleted,
one at a time, which is that limit at its tightest."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path))
           (my/straight-fetch-jobs 1)
           (my/straight-fetch-pipe-wait 1)
           (real (symbol-function 'make-process))
           (held nil))
      (dolist (name '("pkg" "pkg2" "pkg3"))
        (unless (equal name "pkg")
          (straight-fetch-test--git straight-fetch-test--root "clone" clone
                                   (expand-file-name name (file-name-directory clone))))
        (puthash name (straight-fetch-test--recipe clone :package name :local-repo name)
                 straight--recipe-cache))
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nexit 0\n"))
      (set-file-modes fake #o755)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (when (seq-some (lambda (proc) (memq proc (process-list))) held)
                     (signal 'file-error '("Creating pipe" "Too many open files")))
                   (car (push (apply real args) held)))))
        (let* ((delete-exited-processes nil)
               (result (with-timeout (10 :hung) (my/straight-fetch-at-once))))
          (should-not (eq result :hung))
          (should-not (plist-get result :refused))
          (should (= (length held) 3)))))))

(ert-deftest straight-fetch-test-out-of-descriptors-with-nothing-to-wait-for ()
  "With no fetch of ours running, waiting gives up after a while."
  (straight-fetch-test--with clone
    (let ((straight--recipe-cache (make-hash-table :test #'equal))
          (my/straight-fetch-pipe-wait 0.2))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _)
                   (signal 'file-error '("Creating pipe" "Too many open files")))))
        (should-error (with-timeout (10 :hung) (my/straight-fetch-at-once))
                      :type 'file-error)))))

(ert-deftest straight-fetch-test-notices-that-something-arrived ()
  "A fetch that says a ref moved is reported as having moved one.

git is silent onto a pipe when nothing changed and prints when
something did, which is how this is known without asking a second
time.  What it prints reaches the filter, so being told is a matter of
that output having been read before the answer is worked out."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake
        (insert "#!/bin/sh\n"
                "echo '   abc1234..def5678  main -> origin/main'\n"
                "exit 0\n"))
      (set-file-modes fake #o755)
      (let ((result (my/straight-fetch-at-once)))
        (should (equal (plist-get result :moved) '("pkg")))
        (should-not (plist-get result :refused))))))

(ert-deftest straight-fetch-test-says-nothing-moved-when-git-is-silent ()
  "And one that prints nothing is not."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\nexit 0\n"))
      (set-file-modes fake #o755)
      (let ((result (my/straight-fetch-at-once)))
        (should-not (plist-get result :moved))
        (should-not (plist-get result :refused))))))

(ert-deftest straight-fetch-test-fetches-only-the-repos-named ()
  "Given repository names, the fetch leaves every other repository alone."
  (straight-fetch-test--with clone
    (let* ((bin (expand-file-name "bin" straight-fetch-test--root))
           (fake (expand-file-name "git" bin))
           (seen (expand-file-name "seen" straight-fetch-test--root))
           (straight--recipe-cache (make-hash-table :test #'equal))
           (exec-path (cons bin exec-path)))
      (puthash "pkg" (straight-fetch-test--recipe clone) straight--recipe-cache)
      (make-directory bin t)
      (with-temp-file fake (insert "#!/bin/sh\ntouch " seen "\n"))
      (set-file-modes fake #o755)
      (my/straight-fetch-at-once '("other"))
      (should-not (file-exists-p seen))
      (my/straight-fetch-at-once '("pkg"))
      (should (file-exists-p seen)))))

(ert-deftest straight-fetch-test-apply-touches-only-what-is-elsewhere ()
  "Applying the lockfile leaves a repository at its commit alone, checks
out without fetching one whose commit is here, and fetches only for a
commit not yet here."
  (straight-fetch-test--with clone
    (let* ((at (straight-fetch-test--git clone "rev-parse" "HEAD"))
           (here (straight-fetch-test--git clone "rev-parse" "origin/main"))
           (asked :none)
           (thawed :none)
           (locked nil))
      (cl-letf (((symbol-function 'my/straight-fetch-at-once)
                 (lambda (&optional repos) (setq asked repos)))
                ((symbol-function 'straight--lockfile-read-all) (lambda () locked))
                ((symbol-function 'straight-thaw-versions)
                 (lambda () (setq thawed (straight--lockfile-read-all))))
                ((symbol-function 'straight-register-repo-modification) #'ignore)
                ((symbol-function 'message) #'ignore))
        (setq locked `(("pkg" . ,at)))
        (my/straight-apply-lockfile)
        (should (eq asked :none))
        (should (eq thawed :none))
        (setq locked `(("pkg" . ,here)))
        (my/straight-apply-lockfile)
        (should (eq asked :none))
        (should (equal thawed locked))
        (setq locked '(("pkg" . "0123456789abcdef0123456789abcdef01234567")))
        (my/straight-apply-lockfile)
        (should (equal asked '("pkg")))))))

(ert-deftest straight-fetch-test-tells-git-not-to-ask ()
  "The environment the fetches run in leaves git nobody to ask."
  (should (member "GIT_TERMINAL_PROMPT=0" my/straight-fetch-environment))
  (should (seq-find (lambda (entry)
                      (and (string-prefix-p "GIT_SSH_COMMAND=" entry)
                           (string-match-p "BatchMode=yes" entry)))
                    my/straight-fetch-environment)))

(ert-deftest straight-fetch-test-reads-a-detached-head ()
  "A detached HEAD holds its commit, and that is read too."
  (straight-fetch-test--with clone
    (let ((head (straight-fetch-test--git clone "rev-parse" "HEAD")))
      (should (equal (my/straight--head-commit clone) head))
      (straight-fetch-test--git clone "checkout" "-q" "--detach" "HEAD")
      (should (equal (my/straight--head-commit clone) head)))))

;;;; What the ref files say, with no git at all

(ert-deftest straight-fetch-test-reads-a-loose-ref ()
  "A branch that is still a file of its own."
  (straight-fetch-test--with clone
    (should (equal (my/straight--ref clone "refs/heads/main")
                   (straight-fetch-test--git clone "rev-parse" "HEAD")))))

(ert-deftest straight-fetch-test-reads-a-packed-ref ()
  "And one git has tidied away into packed-refs."
  (straight-fetch-test--with clone
    (let ((head (straight-fetch-test--git clone "rev-parse" "HEAD")))
      (straight-fetch-test--git clone "pack-refs" "--all")
      (should-not (file-exists-p (expand-file-name ".git/refs/heads/main" clone)))
      (should (equal (my/straight--ref clone "refs/heads/main") head)))))

(ert-deftest straight-fetch-test-reads-the-branch-head-is-on ()
  "HEAD names a branch, until it does not."
  (straight-fetch-test--with clone
    (should (equal (my/straight--head-branch clone) "main"))
    (straight-fetch-test--git clone "checkout" "--detach" "HEAD")
    (should-not (my/straight--head-branch clone))))

(provide 'straight-fetch-test)
;;; straight-fetch-test.el ends here
