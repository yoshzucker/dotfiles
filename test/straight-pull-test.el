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
                "my/straight--")
  "How many definitions were taken out of init.el.")

(ert-deftest straight-pull-test-definitions-are-there ()
  "The names these tests are about still exist under those names."
  (should (> straight-pull-test--loaded 0))
  (dolist (fn '(my/straight--fast-forward
                my/straight--behind-count
                my/straight--origin-default-branch
                my/straight--origin-url
                my/straight--ordinary-recipe-p
                my/straight--recipes-by-repo))
    (should (fboundp fn))))

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
