;;; my-editor-search-test.el --- Tests for migemo in evil's search  -*- lexical-binding: t; -*-

;;; Commentary:

;; `/' is evil's own search, and my-editor-search.el puts migemo into it by
;; advising the function evil makes every search pattern with.  That is one
;; of evil's internal names: rename it upstream and the advice waits on a
;; name nothing calls, and `/' quietly stops finding Japanese from romaji.
;; The first test is the one that matters -- the name advised is still
;; evil's.
;;
;; Against straight's build of evil, so it is skipped where straight has not
;; built it.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)

(defconst my-editor-search-test--build
  (expand-file-name "straight/build" (or (getenv "STRAIGHT_DIR")
                                         (expand-file-name "~/.emacs.d")))
  "Where straight builds packages.")

(defvar my-editor-search-test--available
  (when (file-directory-p (expand-file-name "evil" my-editor-search-test--build))
    (dolist (dir '("evil" "goto-chg"))
      (add-to-list 'load-path (expand-file-name dir my-editor-search-test--build) t))
    (require 'evil-search nil t))
  "Non-nil when evil's search could be loaded.")

(defvar my-editor-search-test--loaded
  (my-test-load (my-test-module "my-editor-search") "evil-migemo-search-pattern")
  "How many definitions were taken out of the module.")

(ert-deftest my-editor-search-test-advised-name-is-evils ()
  "The function advised still exists in evil, and is advised."
  (skip-unless my-editor-search-test--available)
  (should (>= my-editor-search-test--loaded 2))
  (should (fboundp 'evil-ex-make-search-pattern))
  (should (advice-member-p #'my/evil-migemo-search-pattern
                           'evil-ex-make-search-pattern)))

(ert-deftest my-editor-search-test-plain-word-only ()
  "A plain word goes through migemo; a regexp, or `*''s quoted word, does not."
  (skip-unless my-editor-search-test--available)
  (cl-letf (((symbol-function 'migemo-get-pattern)
             (lambda (word) (concat "MIGEMO:" word))))
    (should (equal (my/evil-migemo-search-pattern '("kensaku"))
                   '("MIGEMO:kensaku")))
    (should (equal (my/evil-migemo-search-pattern '("a.c")) '("a.c")))
    (should (equal (my/evil-migemo-search-pattern '("\\_<foo\\_>"))
                   '("\\_<foo\\_>")))))

(provide 'my-editor-search-test)
;;; my-editor-search-test.el ends here
