;;; straight-lockfile-test.el --- Tests for following the lockfile  -*- lexical-binding: t; -*-

;;; Commentary:

;; Except on the one machine marked as allowed to pull, packages stay
;; at the commits the lockfile names: init.el refuses to clone a repository
;; the lockfile leaves out, and my-core-straight.el puts a refusal around
;; every command that would move a repository off it.  Both are advice on
;; straight's names, so what can go wrong is quiet -- rename one upstream
;; and a pull typed on a follower goes through.  The first test is the one
;; that matters: every name guarded is still straight's, and is guarded.

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

(defconst straight-lockfile-test--root
  (locate-dominating-file (or load-file-name buffer-file-name default-directory)
                          "emacs.d")
  "The dotfiles checkout.")

(defvar straight-lockfile-test--loaded
  (+ (my-test-load (expand-file-name "emacs.d/init.el" straight-lockfile-test--root)
                   "pull-marker\\|follow-lockfile\\|only-what-is-locked")
     (my-test-load (expand-file-name "emacs.d/core/my-core-straight.el"
                                     straight-lockfile-test--root)
                   "my/straight"))
  "How many definitions were taken out of init.el and my-core-straight.el.")

(my/straight-guard-moving-commands)

(ert-deftest straight-lockfile-test-guarded-names-are-straights ()
  "Every command guarded still exists, and is guarded; so is the clone."
  (should (> straight-lockfile-test--loaded 0))
  (dolist (command my/straight-moving-commands)
    (should (fboundp command))
    (should (advice-member-p #'my/straight-refuse-on-follower command)))
  (should (fboundp 'straight-vc-clone))
  (should (advice-member-p #'my/straight-clone-only-what-is-locked
                           'straight-vc-clone))
  (should (fboundp 'straight-thaw-versions))
  (should (fboundp 'straight--lockfile-read-all))
  (should (fboundp 'straight-register-repo-modification)))

(ert-deftest straight-lockfile-test-follower-refuses ()
  "A moving command runs where the lockfile is written, and not on a follower."
  (let ((my/straight-follow-lockfile nil))
    (should (eq (my/straight-refuse-on-follower (lambda () 'ran)) 'ran)))
  (let ((my/straight-follow-lockfile t))
    (should-error (my/straight-refuse-on-follower (lambda () 'ran))
                  :type 'user-error)))

(ert-deftest straight-lockfile-test-follower-clones-only-what-is-locked ()
  "A follower clones a locked repository and refuses any other."
  (cl-letf (((symbol-function 'straight--lockfile-read-all)
             (lambda () '(("locked" . "0123abcd")))))
    (let ((my/straight-follow-lockfile t))
      (should-not (my/straight-clone-only-what-is-locked '(:local-repo "locked")))
      (should-error (my/straight-clone-only-what-is-locked '(:local-repo "other"))
                    :type 'user-error))
    (let ((my/straight-follow-lockfile nil))
      (should-not (my/straight-clone-only-what-is-locked '(:local-repo "other"))))))

(ert-deftest straight-lockfile-test-lockfile-names-straight ()
  "The lockfile dotfiles keeps reads, and names the commit straight is cloned at."
  (let ((versions (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "emacs.d/straight/versions/default.el"
                                       straight-lockfile-test--root))
                    (read (current-buffer)))))
    (should (consp versions))
    (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'"
                            (cdr (assoc "straight.el" versions))))))

(provide 'straight-lockfile-test)
;;; straight-lockfile-test.el ends here
