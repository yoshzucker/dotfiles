;;; my-app-org-digest-test.el --- A day read back from its files  -*- lexical-binding: t; -*-

;;; Commentary:

;; The digest is a reading of files, so it is tested on files: a small memex
;; written out fresh for every test, with one of each kind of dated line the
;; configuration writes, a few files that must not be read, and a day either
;; side of the one being asked about.
;;
;; The journal declares its own TODO keywords.  The tests do not start the
;; configuration, so `org-todo-keywords' is Org's default here, and ONGO
;; would otherwise read as part of a heading's title.
;;
;; The day is in the past on purpose.  A clock with no end runs until now
;; when the day is today, and a test that depends on the hour it is run at
;; is a test that fails in the evening.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'my-test)
(require 'org)
(require 'org-element)

(defvar my/org-journal-file)

(defconst my-app-org-digest-test--loaded
  (+ (my-test-load (my-test-module "my-app-org")
                   "\\`(defconst my/org-log-heading ")
     (my-test-load (my-test-module "my-app-org-digest")
                   "my/org-digest\\|day-digest"))
  "How many definitions were taken from the two modules.")

(defconst my-app-org-digest-test--date "2021-06-15")

(defconst my-app-org-digest-test--long-note
  (make-string 70 ?あ)
  "A note far wider than a line of the digest.")

(defconst my-app-org-digest-test--files
  `(("journal.org" . ":PROPERTIES:
:ID:       journal-id
:END:
#+title: journal
#+todo: NEXT ONGO SDAY | DONE CANCEL

* 2021
** 2021-06 June
*** 2021-06-14 Monday
**** DONE Late task
:LOGBOOK:
CLOCK: [2021-06-14 Mon 23:30]--[2021-06-15 Tue 00:30] =>  1:00
:END:
*** 2021-06-15 火曜日
**** ONGO 設計レビュー
:PROPERTIES:
:FORESIGHT_SURGE: [2021-06-15 Tue 09:00]
:END:
:LOGBOOK:
- State \"ONGO\"       from \"NEXT\"       [2021-06-15 Tue 09:00]
- State \"NEXT\"       from \"SDAY\"       [2021-06-15 Tue 08:50]
CLOCK: [2021-06-15 Tue 14:00]--[2021-06-15 Tue 14:20] =>  0:20
CLOCK: [2021-06-15 Tue 09:00]--[2021-06-15 Tue 10:30] =>  1:30
:END:
- Note taken on [2021-06-15 Tue 10:15] \\\\
  観点を三つに絞った
  二つ目の行
**** Inbox thing
**** DONE Closed one
CLOSED: [2021-06-15 Tue 11:00]
- CLOSING NOTE [2021-06-15 Tue 11:00] \\\\
  終わった
**** DONE Abandoned note
CLOSED: [2021-06-15 Tue 12:00]
**** ONGO Open clock
:LOGBOOK:
- State \"ONGO\"       from              [2021-06-15 Tue 23:00]
CLOCK: [2021-06-15 Tue 23:00]
:END:
*** 2021-06-16 Wednesday
**** Tomorrow thing
:LOGBOOK:
CLOCK: [2021-06-16 Wed 09:00]--[2021-06-16 Wed 10:00] =>  1:00
:END:
")
    ("2020-01-01-00-00-00-a_san.org" . ":PROPERTIES:
:ID:       person-id
:END:
#+title: Aさん
#+filetags: :person:

* Log
- Note taken on [2021-06-15 Tue 13:05] \\\\
  来週の段取りを相談
- Note taken on [2021-05-01 Sat 10:00] \\\\
  古いノート
")
    ("2021-06-15-15-20-00-new_idea.org" . ":PROPERTIES:
:ID:       new-id
:END:
#+title: New idea
")
    ("horizons.org" . "#+title: horizons

* Be kind
** 苛立ち
:PROPERTIES:
:CREATED:      [2021-06-15 Tue 16:00]
:ACT_MOVE:     towards
:END:
- what pulled :: 焦り
")
    ("project.org" . ,(concat "#+title: project

* Budget
Met with B on [2021-06-15 Tue 17:00] about the budget.
- [2021-06-15 Tue 18:00] free-form item
- Note taken on [2021-06-15 Tue 19:00] \\\\
  " my-app-org-digest-test--long-note "
"))
    ("project.org_archive" . "* DONE Archived task
:LOGBOOK:
CLOCK: [2021-06-15 Tue 08:00]--[2021-06-15 Tue 08:45] =>  0:45
:END:
")
    ("demo-fixture.org" . "* Fake
:LOGBOOK:
CLOCK: [2021-06-15 Tue 10:00]--[2021-06-15 Tue 11:00] =>  1:00
:END:
")
    (".hidden/skipped.org" . "* Hidden
- Note taken on [2021-06-15 Tue 09:30] \\\\
  hidden
")
    ("daily/2021-06-15.org" . ":PROPERTIES:
:ID:       daily-id
:END:
#+title: 2021-06-15
* Log
- Note taken on [2021-06-15 Tue 20:00] \\\\
  fleeting
"))
  "The memex every test reads, as (RELATIVE-NAME . TEXT).")

(defmacro my-app-org-digest-test--with-memex (&rest body)
  "Run BODY with `memex' bound to a fresh copy of the test memex.
`org-directory' and `my/org-journal-file' point into it."
  (declare (indent 0))
  `(let* ((memex (file-name-as-directory
                  (file-truename (make-temp-file "digest-memex-" t))))
          (org-directory memex)
          (my/org-journal-file (expand-file-name "journal.org" memex)))
     (unwind-protect
         (progn
           (dolist (file my-app-org-digest-test--files)
             (let ((path (expand-file-name (car file) memex))
                   (coding-system-for-write 'utf-8))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory memex t))))

(defun my-app-org-digest-test--collect (&optional skip exclude)
  "The test day, collected as `my/org-day-digest' would collect it."
  (my/org-digest-collect my-app-org-digest-test--date skip exclude))

(defun my-app-org-digest-test--summary (record)
  "RECORD as (HH:MM KIND LABEL HEAD), the parts a reader sees."
  (list (format-time-string "%H:%M" (plist-get record :time))
        (plist-get record :kind)
        (plist-get (plist-get record :owner) :label)
        (plist-get record :head)))

(defun my-app-org-digest-test--daily (memex)
  "The test day's daily note in MEMEX."
  (expand-file-name "daily/2021-06-15.org" memex))

(defun my-app-org-digest-test--visible (line)
  "LINE as Org displays it: each link shown as its description."
  (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" line))

(ert-deftest my-app-org-digest-test-definitions-are-there ()
  "The names these tests call were found in the modules."
  (should (> my-app-org-digest-test--loaded 20))
  (should (equal my/org-log-heading "Log"))
  (dolist (fn '(my/org-digest-collect my/org-digest-render
                my/org-digest-render-report org-dblock-write:day-digest
                my/org-day-digest my/org-day-digest-report))
    (should (fboundp fn))))

(ert-deftest my-app-org-digest-test-candidates-without-rg ()
  "rg and the Emacs fallback find the same files, hidden ones skipped by both."
  (skip-unless (executable-find "rg"))
  (my-app-org-digest-test--with-memex
    (let* ((names (lambda (files)
                    (sort (mapcar (lambda (f) (file-relative-name f memex)) files)
                          #'string<)))
           (with-rg (funcall names (my/org-digest--candidate-files
                                    my-app-org-digest-test--date memex)))
           (without (cl-letf (((symbol-function 'executable-find)
                               (lambda (&rest _) nil)))
                      (funcall names (my/org-digest--candidate-files
                                      my-app-org-digest-test--date memex)))))
      (should (equal with-rg without))
      (should (equal with-rg
                     '("2020-01-01-00-00-00-a_san.org" "daily/2021-06-15.org"
                       "demo-fixture.org" "horizons.org" "journal.org"
                       "project.org" "project.org_archive"))))))

(ert-deftest my-app-org-digest-test-clocks ()
  "Time is counted inside the day only, by entry, most first."
  (my-app-org-digest-test--with-memex
    (let ((groups (my/org-digest--clock-groups
                   (plist-get (my-app-org-digest-test--collect) :clocks))))
      (should (equal (mapcar (lambda (g)
                               (list (car g)
                                     (plist-get (plist-get (cadr g) :owner) :label)))
                             groups)
                     '((110 "設計レビュー") (60 "Open clock")
                       (45 "Archived task") (30 "Late task"))))
      (should (plist-get (cadr (nth 1 groups)) :open))
      (should (equal (mapcar #'my/org-digest--span (cdr (car groups)))
                     '("09:00-10:30" "14:00-14:20"))))))

(ert-deftest my-app-org-digest-test-timeline ()
  "Every dated line is in the timeline, told apart by drawer and shape."
  (my-app-org-digest-test--with-memex
    (let ((summary (mapcar #'my-app-org-digest-test--summary
                           (plist-get (my-app-org-digest-test--collect
                                       (my-app-org-digest-test--daily memex))
                                      :timeline))))
      (dolist (expected '(("08:50" log "設計レビュー" "SDAY→NEXT")
                          ("09:00" log "設計レビュー" "NEXT→ONGO")
                          ("09:00" other "設計レビュー" "FORESIGHT_SURGE")
                          ("10:15" note "設計レビュー" "Note taken")
                          ("11:00" note "Closed one" "CLOSING NOTE")
                          ("12:00" log "Abandoned note" "CLOSED")
                          ("13:05" note "Aさん" "Note taken")
                          ("15:20" created "New idea" "new node")
                          ("16:00" created "苛立ち" "created")
                          ("17:00" other "Budget" "")
                          ("18:00" note "Budget" "free-form item")
                          ("19:00" note "Budget" "Note taken")
                          ("23:00" log "Open clock" "→ONGO")))
        (should (member expected summary)))
      ;; The CLOSED line its closing note already says, the daily note the
      ;; digest is written into, the fixture and the hidden file.
      (should (= (length summary) 13))
      (should (equal (mapcar #'car summary) (sort (mapcar #'car summary) #'string<))))))

(ert-deftest my-app-org-digest-test-caught ()
  "The journal's date tree says what came in, whatever the day is called."
  (my-app-org-digest-test--with-memex
    (should (equal (mapcar (lambda (r)
                             (let ((owner (plist-get r :owner)))
                               (list (plist-get owner :todo)
                                     (plist-get owner :label))))
                           (plist-get (my-app-org-digest-test--collect) :caught))
                   '(("ONGO" "設計レビュー") (nil "Inbox thing")
                     ("DONE" "Closed one") ("DONE" "Abandoned note")
                     ("ONGO" "Open clock"))))))

(ert-deftest my-app-org-digest-test-unsaved-buffer ()
  "A note not yet on disk is read from the buffer it was written in."
  (my-app-org-digest-test--with-memex
    (let ((buffer (find-file-noselect (expand-file-name "horizons.org" memex))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "- Note taken on [2021-06-15 Tue 21:00] \\\\\n  unsaved\n"))
            (should (seq-find (lambda (r) (equal (plist-get r :excerpt) "unsaved"))
                              (plist-get (my-app-org-digest-test--collect)
                                         :timeline))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest my-app-org-digest-test-block ()
  "The block links from the daily note, holds no heading and fits 80 columns."
  (my-app-org-digest-test--with-memex
    (let ((text (with-temp-buffer
                  (let ((buffer-file-name (my-app-org-digest-test--daily memex)))
                    (org-mode)
                    (org-dblock-write:day-digest
                     (list :date my-app-org-digest-test--date))
                    (buffer-string)))))
      (should (string-prefix-p "Clock 4:05\n" text))
      (should (string-match-p (regexp-quote "[[id:person-id][Aさん]]") text))
      (should (string-match-p
               (regexp-quote "[[file:../journal.org::*設計レビュー][設計レビュー]]")
               text))
      (should (string-match-p "^Caught\n- ONGO " text))
      (should-not (string-match-p "^\\*+ " text))
      (dolist (word '("fleeting" "hidden" "Fake" "Tomorrow"))
        (should-not (string-match-p word text)))
      (dolist (line (split-string text "\n"))
        (should (<= (string-width (my-app-org-digest-test--visible line)) 80))))))

(ert-deftest my-app-org-digest-test-empty-day ()
  "A day with nothing in it says so in one line."
  (my-app-org-digest-test--with-memex
    (should (equal (my/org-digest-render (my/org-digest-collect "1999-01-01")
                                         memex)
                   "(nothing recorded)"))))

(ert-deftest my-app-org-digest-test-report ()
  "The report carries notes in full and leaves the private files out."
  (my-app-org-digest-test--with-memex
    (let ((text (my/org-digest-render-report
                 my-app-org-digest-test--date
                 (my-app-org-digest-test--collect
                  nil my/org-digest-report-exclude-regexps))))
      (should (string-match-p "観点を三つに絞った\n  二つ目の行\n" text))
      (should (string-match-p "journal / 設計レビュー" text))
      (should (string-match-p "Aさん" text))
      (dolist (word '("苛立ち" "焦り" "fleeting"))
        (should-not (string-match-p word text)))
      (should (string-suffix-p "伝えたいこと:\n" text)))))

(ert-deftest my-app-org-digest-test-block-placement ()
  "A new block goes after the file's drawer and title, and only once."
  (with-temp-buffer
    (insert ":PROPERTIES:\n:ID:       x\n:END:\n#+title: 2021-06-15\n* Log\n")
    (org-mode)
    (my/org-digest--goto-block "2021-06-15")
    (should (looking-at-p "#\\+BEGIN: day-digest :date \"2021-06-15\""))
    (my/org-digest--goto-block "2021-06-15")
    (should (equal (buffer-string)
                   (concat ":PROPERTIES:\n:ID:       x\n:END:\n#+title: 2021-06-15\n"
                           "#+BEGIN: day-digest :date \"2021-06-15\"\n#+END:\n"
                           "* Log\n")))))

(ert-deftest my-app-org-digest-test-heads ()
  "Org's log headings, shortened to what a list line needs."
  (should (equal (my/org-digest--head
                  "State \"DONE\"       from \"ONGO\"       [2021-06-15 Tue 10:00]")
                 "ONGO→DONE"))
  (should (equal (my/org-digest--head
                  "State \"ONGO\"       from              [2021-06-15 Tue 10:00]")
                 "→ONGO"))
  (should (equal (my/org-digest--head "Note taken on [2021-06-15 Tue 10:00] \\\\")
                 "Note taken"))
  (should (equal (my/org-digest--head
                  "Rescheduled from \"<2021-06-15 Tue>\" on [2021-06-15 Tue 10:00]")
                 "Rescheduled from \"<2021-06-15 Tue>\"")))

(provide 'my-app-org-digest-test)

;;; my-app-org-digest-test.el ends here
