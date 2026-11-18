;;; my-app-org-private-test.el --- Private notes stay out of passing listings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Notes under `my/org-private-directory' are meant to be reached only
;; through the entry points that ask for them, so that a search, an agenda
;; or a heading list opened in front of others never shows one.  These tests
;; build a small notes directory with a private note beside a public one and
;; read what each listing offers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'my-test)

(defvar my-app-org-private-test--loaded
  (my-test-load (my-test-module "my-app-org")
                ;; Definitions only: the settings that use these names
                ;; want packages a bare Emacs does not have.
                (concat "\\`(def[a-z]+ \\(?:my/org-private-[a-z-]+"
                        "\\|my/org-agenda-exclude-regexps"
                        "\\|my/org-agenda-file-excluded-p\\|my/org-moor-files"
                        "\\|my/consult-org-headings-all\\) "))
  "How many of the definitions under test the module gave up.")

(defmacro my-app-org-private-test--with-notes (&rest body)
  "Run BODY with `org-directory' a fresh notes directory.
It holds `public.org' and, under the private directory, `kept.org'."
  (declare (indent 0))
  `(let* ((org-directory (file-name-as-directory (make-temp-file "memex" t)))
          (org-agenda-files nil))
     (unwind-protect
         (progn
           (make-directory (expand-file-name my/org-private-directory org-directory))
           (dolist (f (list "public.org"
                            (concat my/org-private-directory "/kept.org")))
             (with-temp-file (expand-file-name f org-directory)
               (insert "* NEXT something\n")))
           ,@body)
       (delete-directory org-directory t))))

(ert-deftest my-app-org-private-test-the-definitions-were-read ()
  "A renamed definition must not leave these tests passing on nothing."
  (should (>= my-app-org-private-test--loaded 6)))

(ert-deftest my-app-org-private-test-any-name-of-the-file-is-private ()
  "Abbreviated and expanded names of one file get the same answer."
  (let ((org-directory "~/Documents/memex/"))
    (should (my/org-private-file-p "~/Documents/memex/private/a.org"))
    (should (my/org-private-file-p
             (expand-file-name "~/Documents/memex/private/a.org")))
    ;; a directory that only starts with the same word is not it
    (should-not (my/org-private-file-p "~/Documents/memex/privateer/a.org"))
    ;; nor a private directory somewhere else, such as macOS's /private
    (should-not (my/org-private-file-p "/private/tmp/a.org"))
    (should-not (my/org-private-file-p "~/Documents/memex/a.org"))
    (should-not (my/org-private-file-p nil))))

(ert-deftest my-app-org-private-test-the-agenda-never-reads-private-notes ()
  (my-app-org-private-test--with-notes
    (should (my/org-agenda-file-excluded-p
             (expand-file-name (concat my/org-private-directory "/kept.org")
                               org-directory)))
    (should-not (my/org-agenda-file-excluded-p
                 (expand-file-name "public.org" org-directory)))))

(ert-deftest my-app-org-private-test-heading-search-leaves-them-out ()
  (my-app-org-private-test--with-notes
    (let (offered)
      (cl-letf (((symbol-function 'consult-org-heading)
                 (lambda (_match files) (setq offered files))))
        (my/consult-org-headings-all))
      (should (equal '("public.org") (mapcar #'file-name-nondirectory offered))))))

(ert-deftest my-app-org-private-test-moor-leaves-them-out ()
  (my-app-org-private-test--with-notes
    (should (equal '("public.org")
                   (mapcar #'file-name-nondirectory (my/org-moor-files))))))

(provide 'my-app-org-private-test)

;;; my-app-org-private-test.el ends here
