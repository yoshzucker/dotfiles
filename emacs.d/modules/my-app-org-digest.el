;;; my-app-org-digest.el --- The day that was, read back into its daily note -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything written during a day already carries its date: CLOCK lines,
;; the LOGBOOK, `- Note taken on [...]', `CLOSING NOTE', `:CREATED:', the name
;; of a new node, the journal's date tree.  What is missing is a place where
;; one day is read back as a whole.  The agenda's log mode cannot be it:
;; `org-agenda-files' holds only the files with an open task in them, so a
;; note added to a person's Log never reaches it.
;;
;; Two readers, and they want different things.
;;
;; I read it to look back.  It is a `day-digest' dynamic block in that day's
;; daily note -- links and one-line excerpts, so the sources stay the only
;; copy of what was written and a search does not find every note twice.
;; What the day meant is mine to say: I have the background it was written
;; against, and nothing reading the files does.
;;
;; Other people read what I tell them.  `my/org-day-digest-report' puts the
;; same day, in full, into a fresh Claude session to be shaped into something
;; that can be sent -- and stops short of sending it.  The text is there to
;; be cut, and to have my own points written under its last line, because the
;; facts come from the files and the judgement comes from me; Claude is asked
;; to invent neither.  The session starts in a temporary directory, so what it
;; sees of the notes is what was put in front of it and nothing else.
;;
;; Reading never writes.  A file is parsed in its own buffer when one is open
;; -- unsaved edits included -- and in a temporary one otherwise, and no ID is
;; created to link to.  `org-store-link' would create one, since
;; `org-id-link-to-org-use-id' is t, so a heading without an ID is linked by
;; file and title instead.
;;
;; A dated line is told apart by the rule `my/org-add-log-setup-into-drawer'
;; writes by: a record -- a state change, a reschedule -- goes in the drawer,
;; and a sentence goes outside it.  So a dated item inside a drawer is a log
;; entry and one outside is a note.  A dated line that is neither is still
;; shown, as `other': nothing with the day's date on it is left out except by
;; a rule that can be read here.
;;
;; A module rather than a package: which file is the journal, what the Log
;; heading is called and which files are private are all values of mine, and
;; this is the first time the reading side has been written.
;;
;;   the day, read back             its daily note        C-c n d
;;   another day, read back         its daily note        C-u C-c n d
;;   the day, shaped for others     a new Claude session  M-x my/org-day-digest-report

;;; Code:

;; Do NOT `require' org at top level.  Modules are loaded as *source* and
;; alphabetically, so this file is read before my-editor-evil and
;; my-app-org.el; a top-level require would pull org in ahead of evil (see
;; my-app-calendar.el for the same note).  Org, org-roam and agent-shell are
;; required inside the commands that need them.

(defvar my/org-digest-exclude-regexps '("-fixture\\.org\\'")
  "Regexps for files a day is never read from.
Matched against each candidate's absolute path.  A fixture is made-up work
with real-looking dates, and read back it would look like a day's work.")

(defvar my/org-digest-report-exclude-regexps '("/daily/" "/horizons\\.org\\'")
  "Regexps for files whose records stay out of the report for others.
Matched like `my/org-digest-exclude-regexps', on top of it.  The daily
notes are the half of the time axis that carries no weight (see the
Commentary of my-app-org.el), and horizons.org holds values and ACT choice
points.  Both are read back into the digest; neither is for anybody else.")

(defvar my/org-digest-report-prompt
  (concat
   "以下は私の %s の作業記録です。Org ファイルから機械的に集めたもので、"
   "私自身の評価は含みません。これを、他の人に共有する日報に整形してください。\n"
   "\n"
   "- 事実は記録にあるものだけを使い、推測で補わないこと\n"
   "- 評価・所感・重要度の判断は、末尾の「伝えたいこと」に私が書いたものだけを使うこと\n"
   "- 社外秘や個人的に見える箇所があれば、本文に入れる前に指摘すること\n"
   "- 箇条書き中心で、一分で読める長さにすること\n")
  "What Claude is asked to do with a day, as a `format' string.
The one `%s' is the date.  The records follow it, and the text ends with
the line under which my own points are written before it is sent.")

(defconst my/org-digest--date-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
  "A date as the digest takes it: YYYY-MM-DD.")

(defconst my/org-digest--datetree-re
  "\\`[0-9]\\{4\\}\\(?:-[0-9]\\{2\\}\\(?:-[0-9]\\{2\\}\\)?\\)?\\(?: \\|\\'\\)"
  "A date tree heading: a year, a month or a day, with whatever follows.
The day's name after the date is not matched: `format-time-string' writes
it in the system locale, which on a Japanese Windows is 火曜日.")

;;;; Finding the files

(defun my/org-digest--excluded-p (file regexps)
  "Non-nil when FILE matches any of REGEXPS."
  (seq-some (lambda (re) (string-match-p re file)) regexps))

(defun my/org-digest--candidate-files (date dir)
  "Files under DIR whose text has DATE in an inactive timestamp, as truenames.

rg narrows the search when it is installed, and Emacs reads every file
when it is not.  The same files come back either way, so what a day is
made of does not depend on what this machine has installed: both look at
`.org' and `.org_archive' -- a task archived the day it was done is still
part of that day -- and both skip hidden directories, because rg does."
  (let ((needle (concat "[" date " ")))
    (mapcar
     #'file-truename
     (if (executable-find "rg")
         (with-temp-buffer
           ;; rg writes UTF-8 whatever the locale.  Left to the default, a
           ;; Japanese file name would be decoded as cp932 on Windows.
           (let ((coding-system-for-read 'utf-8))
             (call-process "rg" nil t nil
                           "--no-config" "--files-with-matches" "--fixed-strings"
                           "--glob" "*.org" "--glob" "*.org_archive"
                           "--" needle dir))
           (split-string (buffer-string) "\n" t))
       (seq-filter
        (lambda (file)
          (and (not (string-match-p "\\(?:\\`\\|/\\)\\."
                                    (file-relative-name file dir)))
               (with-temp-buffer
                 (insert-file-contents file)
                 (search-forward needle nil t))))
        (directory-files-recursively dir "\\.org\\(?:_archive\\)?\\'"))))))

(defun my/org-digest--modified-files (dir)
  "Org files under DIR with unsaved edits in a buffer, as truenames.
rg reads the disk, and a note written a minute ago may not be there yet."
  (seq-keep (lambda (buffer)
              (let ((file (buffer-file-name buffer)))
                (and file
                     (buffer-modified-p buffer)
                     (string-match-p "\\.org\\(?:_archive\\)?\\'" file)
                     (file-in-directory-p file dir)
                     (file-truename file))))
            (buffer-list)))

(defun my/org-digest--call-in-file (file fn)
  "Call FN with point at the start of FILE's text and return its value.
An open buffer is read as it stands, widened; otherwise the file is read
into a temporary buffer whose mode hooks never run.  Neither is written."
  (let ((buffer (find-buffer-visiting file)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (funcall fn))))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (goto-char (point-min))
        (funcall fn)))))

;;;; Who a line belongs to

(defun my/org-digest--file-facts (file)
  "FILE's title and file-level ID in the current buffer, as (TITLE . ID)."
  (save-excursion
    (goto-char (point-min))
    (cons (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
              (file-name-base file))
          ;; A file that opens on a heading has no file-level drawer, and
          ;; asked here Org would answer with the heading's.
          (and (org-before-first-heading-p) (org-entry-get nil "ID")))))

(defun my/org-digest--owner (file facts)
  "The entry the line at point belongs to, as a plist.

:file, :pos (the heading's position, or 0 for the file itself), :heading
\(nil for the file), :label, :id, :todo and :path.  FACTS is FILE's
\(TITLE . ID) from `my/org-digest--file-facts'.

A line under the `my/org-log-heading' container belongs to what holds the
container -- a note on a person is about the person, not about \"Log\" --
and date tree headings are left out of :path, since the date is already
the digest's."
  (save-excursion
    (let ((at (ignore-errors (org-back-to-heading t) t)))
      (when (and at (equal (org-get-heading t t t t) my/org-log-heading))
        (setq at (and (org-up-heading-safe) t)))
      (let ((path (cons (car facts)
                        (and at
                             (seq-remove
                              (lambda (s)
                                (or (string-match-p my/org-digest--datetree-re s)
                                    (equal s my/org-log-heading)))
                              (org-get-outline-path t))))))
        (if at
            (list :file file :pos (point)
                  :heading (org-get-heading t t t t)
                  :label (org-get-heading t t t t)
                  :id (org-entry-get nil "ID")
                  :todo (org-get-todo-state)
                  :path path)
          (list :file file :pos 0 :heading nil :label (car facts)
                :id (cdr facts) :todo nil :path path))))))

(defun my/org-digest--owner-key (record)
  "What two records share when they belong to the same entry."
  (let ((owner (plist-get record :owner)))
    (cons (plist-get owner :file) (plist-get owner :pos))))

;;;; Reading the lines

(defun my/org-digest--timestamp (date)
  "A regexp for an inactive timestamp on DATE."
  (concat "\\[" (regexp-quote date) "\\(?: [^]\n]*\\)?\\]"))

(defun my/org-digest--parse (stamp)
  "STAMP as (TIME . HAS-CLOCK-TIME)."
  (cons (org-time-string-to-time stamp)
        (and (string-match-p "[0-9]:[0-9][0-9]" stamp) t)))

(defun my/org-digest--clean (text)
  "TEXT with its timestamps, its trailing `\\\\' and its extra spaces gone."
  (string-trim
   (replace-regexp-in-string
    "[ \t]+" " "
    (replace-regexp-in-string
     "[ \t]*\\\\\\\\[ \t]*\\'" ""
     (replace-regexp-in-string "\\[[0-9]\\{4\\}-[0-9][^]\n]*\\]" "" text)))))

(defun my/org-digest--head (line)
  "The log heading in LINE, short enough to read in a list.
A state change reads FROM→TO; Org's trailing \"on\" is dropped."
  (let ((head (my/org-digest--clean line)))
    (cond
     ;; The previous state is left blank when there was none -- a heading
     ;; captured straight into a state, as `my/org-state-log-line' writes.
     ((string-match "\\`State \"\\([^\"]*\\)\" from\\(?: \"\\([^\"]*\\)\"\\)?\\'" head)
      (format "%s→%s" (or (match-string 2 head) "") (match-string 1 head)))
     ((string-match "\\`\\(.*?\\) on\\'" head) (match-string 1 head))
     (t head))))

(defun my/org-digest--day-bounds (date)
  "The start of DATE and the start of the day after, as a cons of times."
  (let* ((start (org-time-string-to-time date))
         (next (decode-time start)))
    (setf (decoded-time-day next) (1+ (decoded-time-day next))
          (decoded-time-dst next) -1)
    (cons start (encode-time next))))

(defun my/org-digest--clock (date file facts)
  "The CLOCK line at point as a record for DATE, or nil if it is not one.
Only the part inside the day is counted, so a clock across midnight is
split between the two days it belongs to.  A clock with no end runs until
now, or until the end of the day for a day already over, and is :open."
  (when (looking-at (concat org-clock-line-re
                            "[ \t]*\\(\\[[^]\n]+\\]\\)"
                            "\\(?:--\\(\\[[^]\n]+\\]\\)\\)?"))
    (let* ((start-stamp (match-string 1))
           (end-stamp (match-string 2))
           (bounds (my/org-digest--day-bounds date))
           (start (org-time-string-to-time start-stamp))
           (open (not end-stamp))
           (end (if open
                    (if (time-less-p (current-time) (cdr bounds))
                        (current-time)
                      (cdr bounds))
                  (org-time-string-to-time end-stamp)))
           (from (if (time-less-p start (car bounds)) (car bounds) start))
           (to (if (time-less-p (cdr bounds) end) (cdr bounds) end))
           (minutes (round (/ (float-time (time-subtract to from)) 60))))
      (when (> minutes 0)
        (list :kind 'clock :file file :owner (my/org-digest--owner file facts)
              :time from :has-time t :start start :end (and (not open) end)
              :minutes minutes :open open)))))

(defun my/org-digest--item-record (date file facts)
  "The dated list item at point as a log or a note record.
Moves point to the end of the item, so a date in its body is not read as a
record of its own."
  (let* ((drawer (org-element-lineage (org-element-at-point) '(drawer) t))
         (first (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
         (stamp (and (string-match (my/org-digest--timestamp date) first)
                     (match-string 0 first)))
         (parsed (my/org-digest--parse stamp))
         (head-text (replace-regexp-in-string
                     "\\`[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+" "" first))
         ;; Asked of the list rather than of `org-element-at-point', which on
         ;; the first line of a list answers with the list, not the item.
         (end (if (org-at-item-p)
                  (org-list-get-item-end (line-beginning-position)
                                         (org-list-struct))
                (line-end-position)))
         (body (save-excursion
                 (forward-line 1)
                 (if (< (point) end)
                     (string-trim
                      (replace-regexp-in-string
                       "^[ \t]+" ""
                       (buffer-substring-no-properties (point) end)))
                   ""))))
    (prog1
        (list :kind (if drawer 'log 'note) :file file
              :owner (my/org-digest--owner file facts)
              :time (car parsed) :has-time (cdr parsed)
              :head (my/org-digest--head head-text)
              :excerpt (car (split-string body "\n" t "[ \t]+"))
              :body body)
      (goto-char (max end (line-end-position))))))

(defun my/org-digest--line-record (date file facts)
  "The record the dated line at point makes, or nil.
Point is at the start of the line.  It is left there, or at the end of the
list item the line begins when the item was read whole."
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position)))
         (stamp (and (string-match (my/org-digest--timestamp date) line)
                     (match-string 0 line)))
         (parsed (and stamp (my/org-digest--parse stamp)))
         (make (lambda (kind head &optional excerpt)
                 (list :kind kind :file file
                       :owner (my/org-digest--owner file facts)
                       :time (car parsed) :has-time (cdr parsed)
                       :head head :excerpt excerpt :body (or excerpt "")))))
    (cond
     ((looking-at org-clock-line-re) (my/org-digest--clock date file facts))
     ((looking-at "[ \t]*:CREATED:") (funcall make 'created "created"))
     ((looking-at "[ \t]*:\\([^: \t\n]+\\):")
      (funcall make 'other (match-string 1)))
     ((looking-at org-planning-line-re)
      (funcall make 'log (string-remove-suffix ":" (match-string 1))))
     ((looking-at "[ \t]*\\(?:[-+]\\|[0-9]+[.)]\\)[ \t]")
      (my/org-digest--item-record date file facts))
     ;; A line of my own prose, so it is kept as written, date and all;
     ;; only Org's own log headings are worth shortening.
     ((looking-at org-outline-regexp-bol)
      (funcall make 'other "" (org-get-heading t t t t)))
     (t (funcall make 'other ""
                 (replace-regexp-in-string "[ \t]+" " " (string-trim line)))))))

(defun my/org-digest--records-in-buffer (date file)
  "Every record for DATE in the current buffer, which holds FILE."
  (let ((re (my/org-digest--timestamp date))
        (facts (my/org-digest--file-facts file))
        (records '()))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (beginning-of-line)
      (let ((eol (line-end-position)))
        (when-let* ((record (my/org-digest--line-record date file facts)))
          (push record records))
        ;; On past this line, or past the item that was read whole -- which
        ;; may end at the start of the next line, so no line is skipped.
        (goto-char (max (point) eol))))
    (nreverse records)))

(defun my/org-digest--new-nodes (date dir)
  "Records for the nodes created on DATE, read from their file names.
A node's file is named for the second it was made (see
`org-roam-capture-templates'), which is the only place a node with nothing
dated in it says when it began."
  (let ((name-re (concat "\\`" (regexp-quote date)
                         "-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-[0-9]\\{2\\}-.*\\.org\\'")))
    (seq-keep
     (lambda (file)
       (let ((name (file-name-nondirectory file)))
         (when (string-match name-re name)
           (let ((time (org-time-string-to-time
                        (format "%s %s:%s" date
                                (match-string 1 name) (match-string 2 name))))
                 (file (file-truename file)))
             (my/org-digest--call-in-file
              file
              (lambda ()
                (list :kind 'created :file file
                      :owner (my/org-digest--owner
                              file (my/org-digest--file-facts file))
                      :time time :has-time t :head "new node"
                      :excerpt nil :body "")))))))
     (directory-files dir t "\\.org\\'"))))

(defun my/org-digest--caught (date file)
  "Headings filed under DATE in the date tree of FILE, in file order.
The inbox writes a heading and nothing else -- no time, no state -- so the
date tree is the only thing that says when it came in."
  (my/org-digest--call-in-file
   file
   (lambda ()
     (let ((facts (my/org-digest--file-facts file))
           (records '()))
       (when (re-search-forward (concat "^\\(\\*+\\) " (regexp-quote date)
                                        "\\(?:[ \t]\\|$\\)")
                                nil t)
         (org-back-to-heading t)
         (let ((level (org-current-level))
               (end (save-excursion (org-end-of-subtree t t))))
           (while (and (outline-next-heading) (< (point) end))
             (when (= (org-current-level) (1+ level))
               (push (list :kind 'caught :file file
                           :owner (my/org-digest--owner file facts))
                     records)))))
       (nreverse records)))))

;;;; The day

(defun my/org-digest--closed-twice-p (record timeline)
  "Non-nil when RECORD is a CLOSED line its CLOSING NOTE already says.
`org-log-done' is `note', so a task done writes both; the CLOSED line is
kept only where the note was abandoned and it is the one record there is."
  (and (equal (plist-get record :head) "CLOSED")
       (seq-some (lambda (other)
                   (and (eq (plist-get other :kind) 'note)
                        (string-prefix-p "CLOSING NOTE" (plist-get other :head))
                        (equal (my/org-digest--owner-key other)
                               (my/org-digest--owner-key record))
                        (time-equal-p (plist-get other :time)
                                      (plist-get record :time))))
                 timeline)))

(defun my/org-digest-collect (date &optional skip-file exclude)
  "Everything dated DATE (\"YYYY-MM-DD\") under `org-directory', as a plist.

:clocks, :timeline and :caught, each a list of records.  SKIP-FILE -- the
file the digest is written into -- is never read, and nor is any file
matching `my/org-digest-exclude-regexps' or the regexps in EXCLUDE."
  (require 'org)
  (require 'org-element)
  (let* ((dir (file-truename (file-name-as-directory org-directory)))
         (skip (and skip-file (file-truename skip-file)))
         (exclude (append my/org-digest-exclude-regexps exclude))
         (wanted (lambda (file)
                   (not (or (equal file skip)
                            (my/org-digest--excluded-p file exclude)))))
         (files (seq-filter wanted
                            (seq-uniq
                             (append (my/org-digest--candidate-files date dir)
                                     (my/org-digest--modified-files dir)))))
         (records (append
                   (mapcan (lambda (file)
                             (my/org-digest--call-in-file
                              file
                              (lambda ()
                                (my/org-digest--records-in-buffer date file))))
                           files)
                   (seq-filter (lambda (r) (funcall wanted (plist-get r :file)))
                               (my/org-digest--new-nodes date dir))))
         (journal (and (boundp 'my/org-journal-file)
                       (file-exists-p my/org-journal-file)
                       (file-truename my/org-journal-file)))
         (timeline (seq-remove (lambda (r) (eq (plist-get r :kind) 'clock))
                               records)))
    (list :clocks (seq-filter (lambda (r) (eq (plist-get r :kind) 'clock))
                              records)
          :timeline (sort (seq-remove (lambda (r)
                                        (my/org-digest--closed-twice-p r timeline))
                                      timeline)
                          (lambda (a b)
                            (time-less-p (plist-get a :time) (plist-get b :time))))
          :caught (and journal (funcall wanted journal)
                       (my/org-digest--caught date journal)))))

(defun my/org-digest--clock-groups (clocks)
  "CLOCKS gathered by entry, most time first, as (TOTAL FIRST . RECORDS)."
  (let ((groups '()))
    (dolist (clock clocks)
      (let* ((key (my/org-digest--owner-key clock))
             (cell (assoc key groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list clock)))
          (push (cons key (list clock)) groups))))
    (sort (mapcar (lambda (group)
                    (let ((records (sort (cdr group)
                                         (lambda (a b)
                                           (time-less-p (plist-get a :time)
                                                        (plist-get b :time))))))
                      (cons (apply #'+ (mapcar (lambda (r) (plist-get r :minutes))
                                               records))
                            records)))
                  groups)
          (lambda (a b) (> (car a) (car b))))))

;;;; Writing it out

(defun my/org-digest--duration (minutes)
  "MINUTES as H:MM."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun my/org-digest--span (clock)
  "CLOCK's interval as HH:MM-HH:MM, or HH:MM-open."
  (concat (format-time-string "%H:%M" (plist-get clock :start))
          "-"
          (if (plist-get clock :open)
              "open"
            (format-time-string "%H:%M" (plist-get clock :end)))))

(defun my/org-digest--link (owner base-dir)
  "An Org link to OWNER, with file paths relative to BASE-DIR."
  (org-link-make-string
   (cond
    ((plist-get owner :id) (concat "id:" (plist-get owner :id)))
    ((plist-get owner :heading)
     (format "file:%s::*%s"
             (file-relative-name (plist-get owner :file) base-dir)
             (plist-get owner :heading)))
    (t (concat "file:" (file-relative-name (plist-get owner :file) base-dir))))
   (plist-get owner :label)))

(defun my/org-digest--time (record)
  "RECORD's time of day as HH:MM, or blanks for a date without one."
  (if (plist-get record :has-time)
      (format-time-string "%H:%M" (plist-get record :time))
    "     "))

(defun my/org-digest--timeline-line (record base-dir)
  "RECORD as one line of the digest's timeline, kept within 80 columns.
The width is counted on what is displayed, not on the link markup."
  (let* ((owner (plist-get record :owner))
         (head (or (plist-get record :head) ""))
         (excerpt (plist-get record :excerpt))
         (lead (concat "- " (my/org-digest--time record) " "
                       (my/org-digest--link owner base-dir)
                       (if (string-empty-p head) "" (concat " " head))))
         (shown (+ 8 (string-width (plist-get owner :label))
                   (if (string-empty-p head) 0 (1+ (string-width head))))))
    (if (and excerpt (not (string-empty-p excerpt)))
        (concat lead " :: "
                (truncate-string-to-width excerpt (max 20 (- 80 shown 4))
                                          nil nil "…"))
      lead)))

(defun my/org-digest-render (digest base-dir)
  "DIGEST as the text of a `day-digest' block, linking from BASE-DIR.
No headings: the block sits in the daily note's own text, above the Log and
the entries captured into it, and a heading inside it would take them in."
  (let* ((groups (my/org-digest--clock-groups (plist-get digest :clocks)))
         (timeline (plist-get digest :timeline))
         (caught (plist-get digest :caught))
         (sections '()))
    (when groups
      (push (concat
             "Clock " (my/org-digest--duration (apply #'+ (mapcar #'car groups)))
             "\n"
             (mapconcat
              (lambda (group)
                (format "- %s %s %s"
                        (my/org-digest--duration (car group))
                        (my/org-digest--link (plist-get (cadr group) :owner)
                                             base-dir)
                        (mapconcat #'my/org-digest--span (cdr group) ", ")))
              groups "\n"))
            sections))
    (when timeline
      (push (concat "Timeline\n"
                    (mapconcat (lambda (r) (my/org-digest--timeline-line r base-dir))
                               timeline "\n"))
            sections))
    (when caught
      (push (concat "Caught\n"
                    (mapconcat
                     (lambda (r)
                       (let ((owner (plist-get r :owner)))
                         (concat "- "
                                 (if (plist-get owner :todo)
                                     (concat (plist-get owner :todo) " ")
                                   "")
                                 (my/org-digest--link owner base-dir))))
                     caught "\n"))
            sections))
    (if sections
        (string-join (nreverse sections) "\n\n")
      "(nothing recorded)")))

(defun my/org-digest-render-report (date digest)
  "DATE's DIGEST in full, as text for Claude, ending where I write my points.
Entries are named by their whole path rather than linked: the reader has
no files to follow a link into."
  (let* ((path (lambda (r)
                 (string-join (plist-get (plist-get r :owner) :path) " / ")))
         (groups (my/org-digest--clock-groups (plist-get digest :clocks)))
         (indent (lambda (text)
                   (replace-regexp-in-string "^" "  " text))))
    (concat
     (format my/org-digest-report-prompt date)
     "\n## Clock "
     (my/org-digest--duration (apply #'+ (mapcar #'car groups)))
     "\n"
     (mapconcat (lambda (group)
                  (format "- %s %s (%s)\n"
                          (my/org-digest--duration (car group))
                          (funcall path (cadr group))
                          (mapconcat #'my/org-digest--span (cdr group) ", ")))
                groups "")
     "\n## Timeline\n"
     (mapconcat (lambda (r)
                  (let ((head (plist-get r :head))
                        (body (plist-get r :body)))
                    (concat "- " (my/org-digest--time r) " "
                            (if (string-empty-p (or head "")) ""
                              (format "[%s] " head))
                            (funcall path r) "\n"
                            (if (string-empty-p (or body "")) ""
                              (concat (funcall indent body) "\n")))))
                (plist-get digest :timeline) "")
     "\n## Caught\n"
     (mapconcat (lambda (r)
                  (let ((todo (plist-get (plist-get r :owner) :todo)))
                    (concat "- " (if todo (concat todo " ") "")
                            (funcall path r) "\n")))
                (plist-get digest :caught) "")
     "\n伝えたいこと:\n")))

;;;; The block and the commands

(defun org-dblock-write:day-digest (params)
  "Dynamic block reading one day back from every Org file under `org-directory'.

Header: `#+BEGIN: day-digest :date \"YYYY-MM-DD\"'.  Refresh with C-c C-c on
the block or `org-update-all-dblocks'; `my/org-day-digest' (\\[my/org-day-digest])
puts one in the day's daily note and fills it.

Clock is where the time went, entry by entry.  Timeline is everything else
dated that day, in order: log entries from the drawers, notes from outside
them, nodes and entries created, and any other line carrying the date.
Caught is what the journal's date tree took in that day.  The file holding
the block is not read, so the daily note does not list itself."
  (let ((date (plist-get params :date))
        (file (buffer-file-name (buffer-base-buffer))))
    (unless (and (stringp date) (string-match-p my/org-digest--date-re date))
      (user-error "day-digest wants :date \"YYYY-MM-DD\", not %S" date))
    (insert (my/org-digest-render
             (my/org-digest-collect date file)
             (if file
                 (file-name-directory (file-truename file))
               (file-truename (file-name-as-directory org-directory)))))))

(defun my/org-digest--goto-block (date)
  "Move to the `day-digest' block in this buffer, adding one for DATE if none.
A new block goes after the file's own property drawer and keyword lines,
which is where the daily note's text begins."
  (goto-char (point-min))
  (if (re-search-forward "^[ \t]*#\\+BEGIN: day-digest\\_>" nil t)
      (beginning-of-line)
    (goto-char (point-min))
    (when (looking-at org-property-drawer-re)
      (goto-char (match-end 0))
      (forward-line 1))
    (while (looking-at-p "[ \t]*#\\+")
      (forward-line 1))
    (unless (bolp) (insert "\n"))
    (insert (format "#+BEGIN: day-digest :date \"%s\"\n#+END:\n" date))
    (forward-line -2)))

(defun my/org-day-digest (&optional pick-date)
  "Read the day back into its daily note: today, or a day from the calendar.
With PICK-DATE (\\[universal-argument]), the day is chosen from the calendar.
The daily note is created when the day has none, and gets a `day-digest'
block when it has none; the block is then filled.  See
`org-dblock-write:day-digest' for what it shows."
  (interactive "P")
  (require 'org-roam-dailies)
  ;; Going to a daily note runs its capture template with nothing to insert,
  ;; which creates the file and its head only.  "d" is named so that the
  ;; template menu is not offered: any of them would make the same head.
  (if pick-date
      (org-roam-dailies-goto-date nil "d")
    (org-roam-dailies-goto-today "d"))
  (let ((date (file-name-base (buffer-file-name))))
    (unless (string-match-p my/org-digest--date-re date)
      (user-error "%s is not a daily note named for its date" (buffer-name)))
    (my/org-digest--goto-block date)
    (org-update-dblock)))

(defun my/org-day-digest-report (&optional pick-date)
  "Put the day's records in front of Claude, to be shaped for others.
Today, or with PICK-DATE (\\[universal-argument]) a day read from the
calendar.  A new session is started in a temporary directory and the text
is inserted at its prompt without being sent: read it, cut what should not
go, write what matters under its last line, and send it yourself.  Files
matching `my/org-digest-report-exclude-regexps' are left out."
  (interactive "P")
  (require 'org)
  (let* ((date (if pick-date
                   (substring (org-read-date nil nil nil "Report on: ") 0 10)
                 (format-time-string "%Y-%m-%d")))
         (text (my/org-digest-render-report
                date
                (my/org-digest-collect date nil
                                       my/org-digest-report-exclude-regexps))))
    (require 'agent-shell)
    (require 'agent-shell-anthropic)
    (agent-shell-insert
     :text text
     :shell-buffer (agent-shell-new-temp-shell
                    :config (agent-shell-anthropic-make-claude-code-config)))))

(my/define-key
 (:map global-map
       :prefix "C-c n"
       :key
       "d" #'my/org-day-digest))

(provide 'my-app-org-digest)
;;; my-app-org-digest.el ends here
