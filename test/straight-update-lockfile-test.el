;;; straight-update-lockfile-test.el --- Tests for updating the lockfile before moving  -*- lexical-binding: t; -*-

;;; Commentary:

;; `my/straight-update-lockfile' writes into the lockfile the commit each
;; repository would move to, and moves nothing.  What is worth testing is
;; the choice: the newest commit old enough, never one behind the lockfile,
;; and a working tree written here at its HEAD whatever its age.  Real git
;; repositories with commits dated in the past, because what is being
;; tested is what git answers about them.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "straight/repos/straight.el"
                                 (or (getenv "STRAIGHT_DIR")
                                     (expand-file-name "~/.emacs.d")))))
(require 'straight)

(defvar straight-update-lockfile-test--loaded
  (my-test-load (expand-file-name "emacs.d/core/my-core-straight.el"
                                  (locate-dominating-file
                                   (or load-file-name buffer-file-name default-directory)
                                   "emacs.d"))
                "my/straight")
  "How many definitions were taken out of my-core-straight.el.")

(defun straight-update-lockfile-test--git (dir &rest args)
  "Run git with ARGS in DIR and return its output, failing if git does."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun straight-update-lockfile-test--commit (dir text days-ago)
  "Commit TEXT in DIR, dated DAYS-AGO days back, and return its hash."
  (with-temp-file (expand-file-name "file.txt" dir) (insert text "\n"))
  (straight-update-lockfile-test--git dir "add" "file.txt")
  (let* ((date (format-time-string "%Y-%m-%dT%H:%M:%S"
                                   (time-subtract nil (days-to-time days-ago))))
         (process-environment (append (list (concat "GIT_COMMITTER_DATE=" date)
                                            (concat "GIT_AUTHOR_DATE=" date))
                                      process-environment)))
    (straight-update-lockfile-test--git dir "commit" "-q" "-m" text))
  (straight-update-lockfile-test--git dir "rev-parse" "HEAD"))

(defmacro straight-update-lockfile-test--with (bindings &rest body)
  "Make an origin with commits 30, 20 and 1 days old, and a clone of it.
BINDINGS names the clone, and the three commits oldest first."
  (declare (indent 1))
  (pcase-let ((`(,clone ,old ,mid ,new) bindings))
    `(let* ((root (make-temp-file "straight-update-lockfile-test" t))
            (origin (expand-file-name "origin" root))
            (,clone (expand-file-name "clone" root)))
       (unwind-protect
           (progn
             (make-directory origin)
             (straight-update-lockfile-test--git origin "init" "-q" "--initial-branch=main")
             (straight-update-lockfile-test--git origin "config" "user.email" "test@example.invalid")
             (straight-update-lockfile-test--git origin "config" "user.name" "Test")
             (let* ((,old (straight-update-lockfile-test--commit origin "old" 30))
                    (,mid (straight-update-lockfile-test--commit origin "mid" 20))
                    (,new (straight-update-lockfile-test--commit origin "new" 1)))
               (straight-update-lockfile-test--git root "clone" "-q" origin ,clone)
               ,@body))
         (delete-directory root t)))))

(ert-deftest straight-update-lockfile-test-definitions ()
  "The functions are there, and updating the lockfile is refused on a follower."
  (should (> straight-update-lockfile-test--loaded 0))
  (should (memq 'my/straight-update-lockfile my/straight-moving-commands)))

(ert-deftest straight-update-lockfile-test-newest-old-enough ()
  "The newest commit at least DAYS old is taken, and nothing newer."
  (straight-update-lockfile-test--with (clone old mid new)
    (should (equal (my/straight--candidate-commit clone old 7) (cons mid nil)))
    (should (equal (my/straight--candidate-commit clone old 0) (cons new nil)))
    (should (equal (car (my/straight--candidate-commit clone old 25)) old))
    (should (cdr (my/straight--candidate-commit clone old 60)))))

(ert-deftest straight-update-lockfile-test-never-backwards ()
  "A lockfile already past the commit old enough keeps its commit."
  (straight-update-lockfile-test--with (clone old mid new)
    (let ((pick (my/straight--candidate-commit clone new 7)))
      (should (equal (car pick) new))
      (should (cdr pick)))))

(ert-deftest straight-update-lockfile-test-own-working-tree-at-head ()
  "A repository linked in from elsewhere is taken at its HEAD, new or not."
  (straight-update-lockfile-test--with (clone old mid new)
    (let ((link (concat clone "-link")))
      (make-symbolic-link clone link)
      (should (equal (car (my/straight--candidate-commit link old 7))
                     (straight-update-lockfile-test--git clone "rev-parse" "HEAD"))))))

(ert-deftest straight-update-lockfile-test-lockfile-round-trip ()
  "The lockfile written is the text straight writes, and reads back."
  (let* ((dir (make-temp-file "straight-update-lockfile-lock" t))
         (straight-base-dir (file-name-as-directory dir))
         (straight-profiles '((nil . "default.el")))
         (versions '(("b" . "2222") ("a" . "1111"))))
    (unwind-protect
        (progn
          (make-directory (straight--versions-dir) t)
          (my/straight--write-lockfile versions)
          (should (equal (with-temp-buffer
                           (insert-file-contents (straight--versions-file "default.el"))
                           (buffer-string))
                         "((\"a\" . \"1111\")\n (\"b\" . \"2222\"))\n:epsilon\n")))
      (delete-directory dir t))))

(provide 'straight-update-lockfile-test)
;;; straight-update-lockfile-test.el ends here
