;;; my-app-org-capture-test.el --- The index and the templates agree  -*- lexical-binding: t; -*-

;;; Commentary:

;; The head of `my-app-org' carries a table of what to record, where it goes
;; and how to get there.  A table like that is read far more often than the
;; `setq' three hundred lines below it, and nothing makes the two agree: keys
;; get renamed in the template list and the table keeps pointing at the old
;; letters.  It had drifted on eight of its ten rows before these tests
;; existed -- rows naming captures that were not there, and a `C-c z' prefix
;; that had never been bound.
;;
;; So the table is read as data.  Every `C-c c' row must name a template that
;; exists, every template must appear in the table, and the same for the two
;; daily-note rows against `org-roam-dailies-capture-templates'.
;;
;; The module is read rather than loaded: `org-capture-templates' is set
;; inside a `use-package :config' form that would need most of the
;; configuration around it (see the note in `my-test').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'my-test)

(defun my-app-org-capture-test--source ()
  "The text of the module under test."
  (with-temp-buffer
    (insert-file-contents (my-test-module "my-app-org"))
    (buffer-string)))

(defun my-app-org-capture-test--keys (variable)
  "The capture keys VARIABLE is set to in the module.

Reads the `setq' form and walks it for the (KEY DESCRIPTION ...) lists a
capture template is, so the answer does not depend on how the list is
indented or how many templates it holds."
  (with-temp-buffer
    (insert (my-app-org-capture-test--source))
    (goto-char (point-min))
    (re-search-forward (format "(setq %s\\_>" (regexp-quote variable)))
    (goto-char (match-beginning 0))
    (let ((form (read (current-buffer)))
          (keys '()))
      (cl-labels ((walk (x)
                    (when (consp x)
                      (when (and (stringp (car x))
                                 (= 1 (length (car x)))
                                 (stringp (car-safe (cdr x))))
                        (push (car x) keys))
                      (walk (car x))
                      (walk (cdr x)))))
        (walk form))
      (nreverse keys))))

(defun my-app-org-capture-test--index (prefix)
  "The keys the head-of-file table lists under PREFIX, e.g. \"C-c c\".

Only the table is searched -- it ends at the first blank comment line -- so a
key named in the prose below it is not mistaken for an index entry."
  (with-temp-buffer
    (insert (my-app-org-capture-test--source))
    (goto-char (point-min))
    (re-search-forward "^;; What to record, where it goes, how to get there:\n")
    (while (looking-at ";;$") (forward-line 1))
    (let ((end (save-excursion (re-search-forward "^;;$") (point)))
          (keys '()))
      (while (re-search-forward
              (format "%s \\([a-z]\\)\\b" (regexp-quote prefix)) end t)
        (push (match-string 1) keys))
      (nreverse keys))))

(ert-deftest my-app-org-capture-test-the-module-was-read ()
  "Every test below reads the module by name; a rename must not pass quietly."
  (should (my-app-org-capture-test--keys "org-capture-templates"))
  (should (my-app-org-capture-test--keys "org-roam-dailies-capture-templates"))
  (should (my-app-org-capture-test--index "C-c c")))

(ert-deftest my-app-org-capture-test-the-index-names-templates-that-exist ()
  "Nothing in the table sends anybody to a key that captures nothing."
  (let ((templates (my-app-org-capture-test--keys "org-capture-templates")))
    (dolist (key (my-app-org-capture-test--index "C-c c"))
      (should (member key templates)))))

(ert-deftest my-app-org-capture-test-every-template-is-in-the-index ()
  "And nothing captures without the table saying so."
  (let ((index (my-app-org-capture-test--index "C-c c")))
    (dolist (key (my-app-org-capture-test--keys "org-capture-templates"))
      (should (member key index)))))

(ert-deftest my-app-org-capture-test-the-daily-rows-agree-too ()
  "The two daily-note rows go through `C-c n z', whose own keys are the
dailies templates -- a separate list, listed in the same table."
  (let ((dailies (my-app-org-capture-test--keys
                  "org-roam-dailies-capture-templates"))
        (index (my-app-org-capture-test--index "C-c n z")))
    (should index)
    (dolist (key index)
      (should (member key dailies)))
    (dolist (key dailies)
      (should (member key index)))))

(ert-deftest my-app-org-capture-test-only-interrupts-need-the-clock ()
  "Moving to new work goes through the inbox and a state change; the one
template that clocks in is the one that has to give the clock back, and it
is the `:clock-resume' that makes it so."
  (with-temp-buffer
    (insert (my-app-org-capture-test--source))
    (goto-char (point-min))
    (should-not (search-forward "\"switch task\"" nil t))
    (goto-char (point-min))
    (should (search-forward "\"interrupt task\"" nil t))
    (let ((end (save-excursion (search-forward "(\"c\" ") (point))))
      (should (save-excursion (search-forward ":clock-resume t" end t))))
    ;; and no second template clocks in behind it
    (goto-char (point-min))
    (should (= 1 (cl-loop while (search-forward ":clock-in t" nil t) count t)))))

(provide 'my-app-org-capture-test)

;;; my-app-org-capture-test.el ends here
