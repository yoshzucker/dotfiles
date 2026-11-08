;;; my-core-env-test.el --- Tests for the PATH taken from a login shell  -*- lexical-binding: t; -*-

;;; Commentary:

;; An Emacs started from the Dock has launchd's PATH, which holds none of
;; Homebrew, and my-core-env.el replaces it with the one a login shell
;; builds.  What can go wrong is quiet: a startup file that prints
;; something lands in PATH, and a shell that fails leaves Emacs on
;; launchd's list with every Homebrew tool missing.

;;; Code:

(require 'ert)
(require 'my-test)

(defvar my-core-env-test--loaded
  (my-test-load (expand-file-name "emacs.d/core/my-core-env.el"
                                  (locate-dominating-file
                                   (or load-file-name buffer-file-name default-directory)
                                   "emacs.d"))
                "path-from-login-shell")
  "How many definitions were taken out of the module.")

(ert-deftest my-core-env-test-path-is-the-shells ()
  "The login shell's PATH comes back whole, and nothing but PATH."
  (skip-unless (not (eq system-type 'windows-nt)))
  (should (= my-core-env-test--loaded 1))
  (let ((path (my/path-from-login-shell)))
    (should (stringp path))
    (should-not (string-match-p "\n" path))
    (should (member (file-name-as-directory (expand-file-name "~/.local/bin"))
                    (parse-colon-path path)))
    (when (eq system-type 'darwin)
      (should (member "/opt/homebrew/bin/" (parse-colon-path path))))))

(ert-deftest my-core-env-test-failed-shell-gives-nil ()
  "A shell that cannot run, or that fails, gives nil, so PATH is left alone."
  (let ((shell-file-name "/nonexistent/shell"))
    (should-not (my/path-from-login-shell)))
  (let ((shell-file-name "false"))
    (should-not (my/path-from-login-shell))))

(provide 'my-core-env-test)
;;; my-core-env-test.el ends here
