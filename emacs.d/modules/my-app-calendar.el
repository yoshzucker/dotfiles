;;; my-app-calendar.el --- Calendar I/O: ics <-> org, Outlook glue -*- lexical-binding: t; -*-

;;; Commentary:
;; Ingests external calendars into a single `calendar.org' under `org-directory'
;; so events show up directly in `org-agenda' and `org-dayflow' (both read
;; `org-agenda-files').  Self-contained: this file advises
;; `my/org-agenda-files-refresh' to add `calendar.org' to `org-agenda-files', so
;; removing this file (and its symlink) fully reverts the calendar feature.
;;
;; Sources: the child's club schedule (.ics produced from the school PDF) and
;; work meetings (Outlook).  `my/org-outlook-sync' (Windows, M-x) runs
;; `local/bin/outlook-calendar-export.ps1' (COM) and imports the result in one
;; step.  All conversion is local -- the Emacs built-in `icalendar' parser -- so
;; there is no cloud dependency and no extra runtime.
;;
;; Import is idempotent: every entry carries a `:UID:' and is upserted in place,
;; so re-importing never creates duplicates (nor duplicate file headers -- the
;; header is written only into an empty file).  With SYNC (used by the Outlook
;; sync) the import is a full mirror of its CATEGORY: entries whose UID is no
;; longer in the source are pruned, so cancelled/past meetings do not linger.
;; Entries without a UID (hand-added appointments) are always preserved.
;;
;; Compliance: `~/Documents/memex' is iCloud on macOS and OneDrive on the
;; company Windows -- separate, externally-private stores that do not sync to
;; each other.  Work meeting detail therefore stays on the company side; the
;; only company->private bridge is a curated, abstracted ics export.

;;; Code:

;; Do NOT `require' org or icalendar at top level.  These modules are loaded as
;; *source* (not byte-compiled) and alphabetically, so this file loads before
;; my-editor-evil.  For source files `require' (and even `eval-when-compile',
;; which behaves like `progn' when loaded uncompiled) runs at load time -- so a
;; top-level (require 'org) would pull org in before evil and break the intended
;; evil -> org -> org-roam order (org-roam's :config uses `evil-define-key').
;; The import commands require both lazily instead.

;; Compile-time hints only (icalendar is required at runtime, not top-level).
(declare-function icalendar--read-element "icalendar")
(declare-function icalendar--all-events "icalendar")
(declare-function icalendar--get-event-property "icalendar")
(declare-function icalendar--get-event-property-attributes "icalendar")
(declare-function icalendar--decode-isodatetime "icalendar")

(defgroup my/org-calendar nil
  "Ingest external calendars (ics/Outlook) into Org."
  :group 'org)

(defcustom my/org-calendar-file-name "calendar.org"
  "Basename of the aggregated calendar file under `org-directory'."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/outlook-export-script
  (expand-file-name "outlook-calendar-export.ps1" "~/.local/bin/")
  "Path to the PowerShell Outlook->ics exporter (`my/org-outlook-sync')."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/outlook-compose-script
  (expand-file-name "outlook-compose.ps1" "~/.local/bin/")
  "Path to the PowerShell helper that opens an Outlook compose with an attachment."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/org-calendar-busy-file-name "busy.ics"
  "Basename (under `org-directory') of the abstracted busy ics for the phone."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/org-calendar-busy-label "予定あり"
  "SUMMARY used for every event in the busy ics -- the only text it carries.
No title, location, notes or attendees are exported (confidentiality)."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/org-calendar-busy-days 60
  "Forward window in days for `my/org-calendar-export-busy'."
  :type 'integer
  :group 'my/org-calendar)

(defcustom my/org-calendar-busy-exclude-categories nil
  "Categories excluded from the busy ics (e.g. events already on the phone)."
  :type '(repeat string)
  :group 'my/org-calendar)

(defun my/org-calendar-file ()
  "Absolute path to the aggregated calendar file.
Resolved lazily so it tracks `org-directory', which is bound by my-app-org.el
after this module has loaded."
  (expand-file-name my/org-calendar-file-name
                    (or (bound-and-true-p org-directory) "~/Documents/memex/")))

(defun my/org-calendar-default-dir ()
  "Starting directory for the interactive ics file picker.
`org-directory' if it exists, else the current directory."
  (if (and (bound-and-true-p org-directory) (file-directory-p org-directory))
      org-directory
    default-directory))

;; ---- agenda-files wiring -------------------------------------------------
;; Keep the feature self-contained: instead of hard-coding calendar.org into
;; `my/org-agenda-files-refresh' (defined in my-app-org.el), advise it so every
;; refresh (startup + manual) re-adds calendar.org.  Deleting this file removes
;; the advice and calendar.org drops out of `org-agenda-files' on its own.

(defun my/org-calendar--add-to-agenda-files (&rest _)
  "Append the calendar file to `org-agenda-files' when it exists."
  (let ((cal (my/org-calendar-file)))
    (when (and cal (file-exists-p cal))
      (add-to-list 'org-agenda-files cal))))

(with-eval-after-load 'org
  (advice-add 'my/org-agenda-files-refresh :after
              #'my/org-calendar--add-to-agenda-files))

;; ---- ics parsing helpers -------------------------------------------------

(defun my/org-ics--clean (s)
  "Unescape RFC5545 TEXT S and collapse it to a single heading-safe line."
  (let ((r (or s "")))
    (setq r (replace-regexp-in-string "\\\\[nN]" " " r t t))     ; \n \N -> space
    (setq r (replace-regexp-in-string "\\\\\\([,;]\\)" "\\1" r t)) ; \, \; -> , ;
    (string-trim (replace-regexp-in-string "[ \t]*[\r\n]+[ \t]*" " " r))))

(defun my/org-ics--rrule->repeater (rrule)
  "Best-effort org repeater string (with leading space) from ical RRULE.
Only simple FREQ/INTERVAL cases are mapped; anything else (BYDAY lists, COUNT,
UNTIL, ...) is approximated by FREQ alone or dropped.  Returns nil when RRULE
carries no mappable frequency."
  (when (and rrule (stringp rrule))
    (let* ((up (upcase rrule))
           (freq (when (string-match "FREQ=\\([A-Z]+\\)" up)
                   (match-string 1 up)))
           (interval (if (string-match "INTERVAL=\\([0-9]+\\)" up)
                         (string-to-number (match-string 1 up))
                       1))
           (unit (pcase freq
                   ("DAILY" "d") ("WEEKLY" "w")
                   ("MONTHLY" "m") ("YEARLY" "y") (_ nil))))
      (when unit (format " +%d%s" interval unit)))))

(defun my/org-ics--timestamp (dtstart dtend attrs rrule)
  "Build an org active-timestamp string from ical DTSTART/DTEND strings.
ATTRS is DTSTART's attribute list (for all-day detection); RRULE is optional.
All-day DTEND is exclusive per RFC5545, so the inclusive org end is DTEND-1d."
  (let* ((all-day (or (member "DATE" attrs)
                      (not (string-match-p "T" dtstart))))
         (start (encode-time (icalendar--decode-isodatetime dtstart)))
         (end (and dtend (encode-time (icalendar--decode-isodatetime dtend))))
         (rep (or (my/org-ics--rrule->repeater rrule) "")))
    (if all-day
        (let* ((incl-end (and end (time-subtract end 86400)))
               (start-day (format-time-string "%Y-%m-%d" start))
               (end-day (and incl-end (format-time-string "%Y-%m-%d" incl-end))))
          (if (and end-day (not (string= start-day end-day)))
              (format "<%s>--<%s>"
                      (format-time-string "%Y-%m-%d %a" start)
                      (format-time-string "%Y-%m-%d %a" incl-end))
            (format "<%s%s>" (format-time-string "%Y-%m-%d %a" start) rep)))
      (let ((start-day (format-time-string "%Y-%m-%d" start))
            (end-day (and end (format-time-string "%Y-%m-%d" end))))
        (cond
         ((null end)
          (format "<%s%s>" (format-time-string "%Y-%m-%d %a %H:%M" start) rep))
         ((string= start-day end-day)
          (format "<%s-%s%s>"
                  (format-time-string "%Y-%m-%d %a %H:%M" start)
                  (format-time-string "%H:%M" end)
                  rep))
         (t
          (format "<%s%s>--<%s>"
                  (format-time-string "%Y-%m-%d %a %H:%M" start)
                  rep
                  (format-time-string "%Y-%m-%d %a %H:%M" end))))))))

(defun my/org-ics--event->fields (event category)
  "Return a plist (:uid :title :category :location :timestamp) for EVENT, or nil.
DESCRIPTION is intentionally omitted to avoid dumping meeting bodies into org."
  (let* ((dtstart (icalendar--get-event-property event 'DTSTART))
         (ts (and dtstart
                  (ignore-errors
                    (my/org-ics--timestamp
                     dtstart
                     (icalendar--get-event-property event 'DTEND)
                     (icalendar--get-event-property-attributes event 'DTSTART)
                     (icalendar--get-event-property event 'RRULE))))))
    (when ts
      (let ((summary (my/org-ics--clean
                      (or (icalendar--get-event-property event 'SUMMARY)
                          "(no title)"))))
        (list :uid (or (icalendar--get-event-property event 'UID)
                       (concat "nouid-" (md5 (concat dtstart summary))))
              :title summary
              :category category
              :location (let ((l (icalendar--get-event-property event 'LOCATION)))
                          (and l (not (string-empty-p l)) (my/org-ics--clean l)))
              :timestamp ts)))))

(defun my/org-ics--fields->new-entry (f)
  "Build org text for a brand-new entry from fields plist F."
  (apply #'concat
         (delq nil
               (list
                (format "* %s\n" (plist-get f :title))
                ":PROPERTIES:\n"
                (format ":UID: %s\n" (plist-get f :uid))
                (and (plist-get f :category)
                     (format ":CATEGORY: %s\n" (plist-get f :category)))
                (and (plist-get f :location)
                     (format ":LOCATION: %s\n" (plist-get f :location)))
                ":END:\n"
                (format "%s\n" (plist-get f :timestamp))))))

;; ---- non-destructive merge (only machine-owned fields are touched) -------

(defun my/org-ics--update-timestamp (ts)
  "Replace this entry's event timestamp with TS, preserving all other content.
Point must be at the heading.  Replaces the first active timestamp/range in the
entry's own body (skipping planning/drawers, stopping before any child), or
inserts TS if none remains."
  (org-back-to-heading t)
  (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
         (body-start (save-excursion (org-back-to-heading t)
                                     (org-end-of-meta-data t) (point)))
         (limit (save-excursion
                  (goto-char body-start)
                  (if (re-search-forward org-heading-regexp subtree-end t)
                      (line-beginning-position)
                    subtree-end))))
    (goto-char body-start)
    (if (re-search-forward org-tsr-regexp limit t)
        (replace-match ts t t)
      (goto-char body-start)
      (insert ts "\n"))))

(defun my/org-ics--merge-into-entry (f)
  "Update the entry at point from fields F, preserving user-owned content.
Machine-owned only: title text, :UID:/:CATEGORY:/:LOCATION:, event timestamp.
Preserved: TODO state, tags, priority, LOGBOOK/clock, SCHEDULED/DEADLINE, body
notes and sub-headings."
  (org-back-to-heading t)
  (org-edit-headline (plist-get f :title))
  (org-back-to-heading t)
  (org-entry-put (point) "UID" (plist-get f :uid))
  (if (plist-get f :category)
      (org-entry-put (point) "CATEGORY" (plist-get f :category))
    (org-entry-delete (point) "CATEGORY"))
  (if (plist-get f :location)
      (org-entry-put (point) "LOCATION" (plist-get f :location))
    (org-entry-delete (point) "LOCATION"))
  (my/org-ics--update-timestamp (plist-get f :timestamp)))

;; ---- lookup / prune ------------------------------------------------------

(defun my/org-ics--find-uid (uid cal-buf files)
  "Return (BUFFER . POS) of the entry whose :UID: is UID.
Searches CAL-BUF first (current in-memory state), then FILES (other agenda
files) so a calendar event refiled into a task file is updated in place, not
duplicated.  Returns nil if not found."
  (or (with-current-buffer cal-buf
        (org-with-wide-buffer
         (let ((pos (org-find-property "UID" uid)))
           (and pos (cons cal-buf pos)))))
      (catch 'found
        (dolist (file files)
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (let ((pos (org-find-property "UID" uid)))
               (when pos (throw 'found (cons (current-buffer) pos)))))))
        nil)))

(defun my/org-ics--entry-pristine-p ()
  "Non-nil if the entry at point is an untouched machine mirror (safe to prune).
Pristine = no TODO keyword, no local tags, no child heading, no LOGBOOK/clock,
and the body is exactly the one event timestamp.  Any user investment makes it
non-pristine, so clocked/annotated events are never deleted."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (and (not (org-get-todo-state))
           (not (org-get-tags nil t))
           (not (save-excursion (org-goto-first-child)))
           (not (save-excursion
                  (org-back-to-heading t)
                  (re-search-forward "^[ \t]*:LOGBOOK:" end t)))
           (save-excursion
             (org-back-to-heading t)
             (org-end-of-meta-data t)
             (string-match-p
              (concat "\\`" org-tsr-regexp "\\'")
              (string-trim (buffer-substring-no-properties (point) end))))))))

(defun my/org-ics--prune-stale (category keep-uids)
  "Delete pristine CATEGORY entries in this buffer whose UID is not in KEEP-UIDS.
Only untouched machine mirrors are removed (see `my/org-ics--entry-pristine-p');
clocked/annotated entries are kept.  Returns the number removed."
  (let (stale (removed 0))
    (goto-char (point-min))
    ;; SCOPE nil = this (widened) buffer only; `file' scope would prompt about
    ;; agenda files while a brand-new calendar.org is still unsaved.
    (org-map-entries
     (lambda ()
       (let ((uid (org-entry-get nil "UID")))
         (when (and uid
                    (equal (org-entry-get nil "CATEGORY") category)
                    (not (member uid keep-uids))
                    (my/org-ics--entry-pristine-p))
           (push uid stale))))
     nil)
    (dolist (uid stale)
      (let ((pos (org-find-property "UID" uid)))
        (when pos
          (goto-char pos)
          (org-back-to-heading t)
          (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
          (cl-incf removed))))
    removed))

;; ---- import --------------------------------------------------------------

(defun my/org-import-ics (file &optional category sync)
  "Import events from ics FILE into the aggregated calendar (non-destructive).
Idempotent by `:UID:'.  CATEGORY (default: FILE's basename) tags entries so the
agenda can group/color by source (e.g. \"club\", \"outlook\").  Interactively,
a prefix arg prompts for CATEGORY.

Existing events are MERGED, not overwritten: only machine-owned fields (title,
event timestamp, :UID:/:CATEGORY:/:LOCATION:) are updated; the user's TODO
state, clock (LOGBOOK), notes, sub-headings, tags and SCHEDULED/DEADLINE are
preserved.  A UID that already lives in another agenda file (a refiled event) is
updated there, not duplicated.

When SYNC is non-nil the import is a full mirror of CATEGORY: entries of that
category whose UID is absent from FILE are pruned -- but only if they are
untouched machine mirrors (clocked/annotated events are kept).  Returns
(ADDED UPDATED SKIPPED REMOVED)."
  (interactive
   (list (read-file-name "ICS file: " (my/org-calendar-default-dir) nil t nil
                         (lambda (n) (or (file-directory-p n)
                                         (string-suffix-p ".ics" n))))
         (when current-prefix-arg (read-string "Category: "))))
  (require 'org)
  (require 'icalendar)
  (let* ((category (or category (file-name-base file)))
         (ical (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (icalendar--read-element nil nil)))
         (events (icalendar--all-events ical))
         (cal-file (my/org-calendar-file))
         (cal-buf (find-file-noselect cal-file))
         ;; Other agenda files to check for refiled events (calendar file excluded;
         ;; it is searched via CAL-BUF, whose in-memory state is authoritative).
         (other-files (seq-filter #'file-exists-p
                                  (delete cal-file
                                          (and (fboundp 'org-agenda-files)
                                               (copy-sequence (org-agenda-files))))))
         (touched (list cal-buf))
         (added 0) (updated 0) (skipped 0) (removed 0)
         (seen '()))
    ;; Header goes only into an empty calendar file (never duplicated).
    (with-current-buffer cal-buf
      (org-with-wide-buffer
       (when (= (buffer-size) 0)
         (insert "#+title: Calendar (imported events; safe to clock/annotate/refile)\n"
                 "# Managed by my/org-import-ics: sync updates only title/time/place.\n\n"))))
    (dolist (event events)
      (let ((f (my/org-ics--event->fields event category)))
        (if (null f)
            (cl-incf skipped)
          (push (plist-get f :uid) seen)
          (let ((loc (my/org-ics--find-uid (plist-get f :uid) cal-buf other-files)))
            (if loc
                (with-current-buffer (car loc)
                  (cl-pushnew (current-buffer) touched)
                  (org-with-wide-buffer
                   (goto-char (cdr loc))
                   (my/org-ics--merge-into-entry f))
                  (cl-incf updated))
              (with-current-buffer cal-buf
                (org-with-wide-buffer
                 (goto-char (point-max))
                 (unless (bolp) (insert "\n"))
                 (insert (my/org-ics--fields->new-entry f)))
                (cl-incf added)))))))
    (when sync
      (with-current-buffer cal-buf
        (org-with-wide-buffer
         (setq removed (my/org-ics--prune-stale category seen)))))
    ;; Persist only the buffers actually modified.
    (dolist (b touched)
      (when (buffer-live-p b)
        (with-current-buffer b
          (when (buffer-modified-p) (save-buffer)))))
    (when (called-interactively-p 'interactive)
      (message "ics import [%s]: +%d ~%d -%d skip %d"
               category added updated removed skipped))
    (list added updated skipped removed)))

(defun my/org-outlook-sync (&optional days)
  "Export the Outlook calendar (PowerShell/COM) and import it into org.
Windows only.  Runs `my/outlook-export-script' into a temporary .ics, imports it
as a full mirror of category \"outlook\" (cancelled/past meetings pruned), then
refreshes `org-agenda-files' -- one command instead of running PowerShell and
importing separately.  With a prefix arg, prompt for DAYS ahead."
  (interactive (list (when current-prefix-arg (read-number "Days ahead: " 30))))
  (unless (eq system-type 'windows-nt)
    (user-error "Outlook sync is Windows-only (needs Outlook + COM)"))
  (let* ((script (expand-file-name my/outlook-export-script))
         (out (make-temp-file "outlook-" nil ".ics"))
         (args (append (list "-NoProfile" "-ExecutionPolicy" "Bypass"
                             "-File" script "-OutFile" out)
                       (when days (list "-Days" (number-to-string days))))))
    (unless (file-exists-p script)
      (user-error "Export script not found: %s (run `bootstrap link'?)" script))
    (message "Exporting Outlook calendar...")
    (unwind-protect
        (let ((rc (apply #'call-process "powershell" nil "*outlook-export*" nil args)))
          (unless (and (integerp rc) (= rc 0))
            (user-error "Outlook export failed (rc=%s); see *outlook-export*" rc))
          (unless (> (file-attribute-size (file-attributes out)) 0)
            (user-error "Outlook export produced no data"))
          (pcase-let ((`(,a ,u ,s ,r) (my/org-import-ics out "outlook" t)))
            (when (fboundp 'my/org-agenda-files-refresh)
              (my/org-agenda-files-refresh))
            (message "Outlook sync: +%d ~%d -%d skip %d" a u r s)))
      (when (file-exists-p out) (delete-file out)))))

;; ---- export: abstracted "busy" ics for the private phone -----------------

(defun my/org-calendar-busy-file ()
  "Absolute path of the abstracted busy ics."
  (expand-file-name my/org-calendar-busy-file-name
                    (or (bound-and-true-p org-directory) "~/Documents/memex/")))

(defun my/org-ics--ts-rrule (ts)
  "Return an ical RRULE line for org timestamp TS's repeater, or nil."
  (let ((val (or (org-element-property :repeater-value ts) 1)))
    (pcase (org-element-property :repeater-unit ts)
      ('day   (format "RRULE:FREQ=DAILY;INTERVAL=%d" val))
      ('week  (format "RRULE:FREQ=WEEKLY;INTERVAL=%d" val))
      ('month (format "RRULE:FREQ=MONTHLY;INTERVAL=%d" val))
      ('year  (format "RRULE:FREQ=YEARLY;INTERVAL=%d" val)))))

(defun my/org-ics--ts->ics (ts)
  "Return a plist (:all-day BOOL :dtstart STR :dtend STR) for org timestamp TS.
All-day DTEND is the exclusive next day; a timed event with no end time gets a
zero-length end."
  (let ((ys (org-element-property :year-start ts))
        (ms (org-element-property :month-start ts))
        (ds (org-element-property :day-start ts))
        (hs (org-element-property :hour-start ts))
        (mns (org-element-property :minute-start ts))
        (ye (org-element-property :year-end ts))
        (me (org-element-property :month-end ts))
        (de (org-element-property :day-end ts))
        (he (org-element-property :hour-end ts))
        (mne (org-element-property :minute-end ts)))
    (if (null hs)
        (let ((end+1 (time-add (encode-time 0 0 0 de me ye) 86400)))
          (list :all-day t
                :dtstart (format "%04d%02d%02d" ys ms ds)
                :dtend (format-time-string "%Y%m%d" end+1)))
      (list :all-day nil
            :dtstart (format "%04d%02d%02dT%02d%02d00" ys ms ds hs (or mns 0))
            :dtend (if he
                       (format "%04d%02d%02dT%02d%02d00" ye me de he (or mne 0))
                     (format "%04d%02d%02dT%02d%02d00" ys ms ds hs (or mns 0)))))))

(defun my/org-ics--entry-timestamp ()
  "Parse the entry's first active timestamp into an org timestamp element, or nil.
Point must be at the heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (org-end-of-meta-data t)
      (when (re-search-forward org-tsr-regexp end t)
        (org-timestamp-from-string (match-string 0))))))

(defun my/org-ics--busy-vevent (uid ts label stamp)
  "Return an abstracted VEVENT string (only LABEL + time) for UID and TS."
  (let ((d (my/org-ics--ts->ics ts))
        (rrule (my/org-ics--ts-rrule ts)))
    (concat
     "BEGIN:VEVENT\r\n"
     (format "UID:%s\r\n" uid)
     (format "DTSTAMP:%s\r\n" stamp)
     (if (plist-get d :all-day)
         (format "DTSTART;VALUE=DATE:%s\r\nDTEND;VALUE=DATE:%s\r\n"
                 (plist-get d :dtstart) (plist-get d :dtend))
       (format "DTSTART:%s\r\nDTEND:%s\r\n"
               (plist-get d :dtstart) (plist-get d :dtend)))
     (if rrule (concat rrule "\r\n") "")
     (format "SUMMARY:%s\r\n" label)
     "END:VEVENT\r\n")))

(defun my/org-calendar--outlook-compose (attachment)
  "On Windows, open an Outlook compose window with ATTACHMENT via COM."
  (let ((script (expand-file-name my/outlook-compose-script)))
    (if (not (file-exists-p script))
        (message "Busy ics written: %s (compose script missing: %s)" attachment script)
      (call-process "powershell" nil "*outlook-compose*" nil
                    "-NoProfile" "-ExecutionPolicy" "Bypass" "-File" script
                    "-Attachment" attachment
                    "-Subject" my/org-calendar-busy-label)
      (message "Busy ics written; Outlook compose opened: %s" attachment))))

(defun my/org-calendar-export-busy ()
  "Write an abstracted \"busy\" ics of calendar events, then open the mailer.
Every event becomes a `my/org-calendar-busy-label' block carrying only its time
-- no title, location, notes or attendees -- so it is safe to send to a private
device.  Covers the next `my/org-calendar-busy-days' days plus any repeating
events; `my/org-calendar-busy-exclude-categories' are skipped.  On Windows the
default mailer (Outlook) opens with the ics attached; elsewhere the path is
reported.  Reuses each event's `:UID:' (a synthesized one for UID-less manual
entries) so re-importing on the phone updates rather than duplicates."
  (interactive)
  (require 'org)
  (require 'org-element)
  (let* ((src (my/org-calendar-file))
         (out (my/org-calendar-busy-file))
         (label my/org-calendar-busy-label)
         (now (current-time))
         (dnow (decode-time now))
         (today (encode-time 0 0 0 (nth 3 dnow) (nth 4 dnow) (nth 5 dnow)))
         (win-end (time-add now (* my/org-calendar-busy-days 24 60 60)))
         (stamp (format-time-string "%Y%m%dT%H%M%SZ" now t))
         (events '()))
    (unless (file-exists-p src)
      (user-error "No calendar file: %s" src))
    (with-current-buffer (find-file-noselect src)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (unless (member (org-entry-get nil "CATEGORY")
                          my/org-calendar-busy-exclude-categories)
            (let ((ts (my/org-ics--entry-timestamp)))
              (when ts
                (let ((start (encode-time
                              0 (or (org-element-property :minute-start ts) 0)
                              (or (org-element-property :hour-start ts) 0)
                              (org-element-property :day-start ts)
                              (org-element-property :month-start ts)
                              (org-element-property :year-start ts))))
                  (when (or (org-element-property :repeater-type ts)
                            (and (not (time-less-p start today))
                                 (time-less-p start win-end)))
                    (let ((uid (or (org-entry-get nil "UID")
                                   (concat "busy-"
                                           (md5 (concat (org-get-heading t t t t)
                                                        (or (org-element-property :raw-value ts) "")))))))
                      (push (my/org-ics--busy-vevent uid ts label stamp) events))))))))
        nil)))
    (let ((ics (concat "BEGIN:VCALENDAR\r\nVERSION:2.0\r\n"
                       "PRODID:-//dotfiles//busy-export//EN\r\n"
                       (apply #'concat (nreverse events))
                       "END:VCALENDAR\r\n"))
          (coding-system-for-write 'utf-8-unix))
      (write-region ics nil out))
    (if (eq system-type 'windows-nt)
        (my/org-calendar--outlook-compose out)
      (message "Busy ics written: %s" out))
    out))

(provide 'my-app-calendar)
;;; my-app-calendar.el ends here
