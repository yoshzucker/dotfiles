;;; my-app-org-attach-test.el --- Where a screenshot's link lands  -*- lexical-binding: t; -*-

;;; Commentary:

;; `C-c C-a p' captures a screenshot and writes an `attachment:' link at the
;; cursor.  The cursor is the whole difficulty: `org-attach' runs
;; `org-back-to-heading-or-point-min' before it executes the command a key
;; selects, and does so outside the `save-excursion' that wraps its prompt,
;; so by the time the command runs, point is on the headline no matter where
;; the person was reading.  Inserting there writes the link between the stars
;; and the title.
;;
;; These tests drive the real dispatcher, with the screenshot backend and the
;; key press stubbed, and read the resulting buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'my-test)

(defvar my-app-org-attach-test--loaded
  (my-test-load (my-test-module "my-app-org") "my/org-attach")
  "How many `my/org-attach' definitions the module gave up.")

(defun my-app-org-attach-test--attach (text search column)
  "Capture into TEXT with point at COLUMN of the line holding SEARCH.

Runs `org-attach' itself and answers its prompt with `p', so the path under
test is the one a keystroke takes.  Returns the buffer's text with the
generated id and file name replaced, so a test can be read."
  (let* ((dir (file-name-as-directory (make-temp-file "my-attach" t)))
         (file (expand-file-name "note.org" dir))
         (org-attach-id-dir (expand-file-name "data/" dir))
         (org-id-locations-file (expand-file-name ".org-id" dir))
         (org-id-track-globally nil)
         (org-attach-expert t)
         (inhibit-message t))
    (unwind-protect
        (progn
          (with-temp-file file (insert text))
          (with-current-buffer (find-file-noselect file)
            (org-mode)
            (goto-char (point-min))
            (search-forward search)
            (beginning-of-line)
            (forward-char column)
            (cl-letf (((symbol-function 'my/org-attach-screenshot--capture)
                       (lambda (target) (write-region "PNG" nil target)))
                      ((symbol-function 'read-char-exclusive) (lambda (&rest _) ?p)))
              (org-attach))
            (prog1 (replace-regexp-in-string
                    "attachment:[^]]+" "attachment:<png>"
                    (replace-regexp-in-string
                     ":ID: +[-0-9a-zA-Z]+" ":ID: <id>"
                     (buffer-substring-no-properties (point-min) (point-max))))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory dir t))))

(ert-deftest my-app-org-attach-test-the-definitions-are-there ()
  "A rename would otherwise leave a file of tests that pass by testing nothing."
  (should (> my-app-org-attach-test--loaded 0))
  (should (fboundp 'my/org-attach-screenshot))
  (should (fboundp 'my/org-attach--remember-origin))
  (should (assoc '(?p) org-attach-commands))
  (should (advice-member-p #'my/org-attach--remember-origin 'org-attach)))

(ert-deftest my-app-org-attach-test-link-lands-where-the-cursor-was ()
  "In the body, at the cursor, and the headline is left alone."
  (let ((out (my-app-org-attach-test--attach
              "* Task\nfirst body line\nsecond body line\n" "second body" 6)))
    (should (string-match-p "^second \\[\\[attachment:<png>\\]\\]body line$" out))
    (should-not (string-match-p "^\\*.*attachment:" out))))

(ert-deftest my-app-org-attach-test-link-lands-below-a-drawer-just-written ()
  "The cursor on the first line under a heading that has no ID yet.

Attaching gives the heading an ID, and the property drawer that carries it
is written between the headline and the line the cursor was on."
  (let ((out (my-app-org-attach-test--attach
              "* Task\nfirst body line\n" "first body" 0)))
    (should (string-match-p "^:END:\nf\\[\\[attachment:<png>\\]\\]irst body line$"
                            out))))

(ert-deftest my-app-org-attach-test-a-headline-is-not-somewhere-to-put-a-link ()
  "The cursor really on the headline means the entry, not its title."
  (let ((out (my-app-org-attach-test--attach
              "* Task\nsome body\n" "* Task" 3)))
    (should (string-match-p "^:END:\n\\[\\[attachment:<png>\\]\\]\nsome body$" out))
    (should-not (string-match-p "^\\*.*attachment:" out))))

(ert-deftest my-app-org-attach-test-an-empty-entry-gets-a-line-of-its-own ()
  (let ((out (my-app-org-attach-test--attach "* Task\n" "* Task" 3)))
    (should (string-match-p "^:END:\n\\[\\[attachment:<png>\\]\\]$" out))
    (should-not (string-match-p "^\\*.*attachment:" out))))

(provide 'my-app-org-attach-test)

;;; my-app-org-attach-test.el ends here
