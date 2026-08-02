;;; my-app-calendar.el --- Calendar I/O: ics <-> org, Outlook glue -*- lexical-binding: t; -*-

;;; Commentary:
;; Ingests external calendars into a single `calendar.org' under `org-directory'
;; so events show up directly in `org-agenda' and `org-dayflow' (both read
;; `org-agenda-files'; see `my/org-agenda-files-refresh' in my-app-org.el, which
;; always includes `calendar.org').
;;
;; Sources: the child's club schedule (.ics produced from the school PDF) and
;; work meetings (Outlook, exported locally via `local/bin/outlook-calendar-
;; export.ps1' using COM).  All conversion is local -- the Emacs built-in
;; `icalendar' parser -- so there is no cloud dependency and no extra runtime.
;;
;; Import is idempotent: every entry carries a `:UID:' property and is upserted
;; in place, so re-importing an updated monthly .ics never creates duplicates.
;; Entries without a UID (hand-added appointments) are left untouched.
;;
;; Compliance: `~/Documents/memex' is iCloud on macOS and OneDrive on the
;; company Windows -- separate, externally-private stores that do not sync to
;; each other.  Work meeting detail therefore stays on the company side; the
;; only company->private bridge is the abstracted ics export (Phase 5).

;;; Code:

;; Do NOT `require' org or icalendar at top level.  These modules are loaded as
;; *source* (not byte-compiled) and alphabetically, so this file loads before
;; my-editor-evil.  For source files `require' (and even `eval-when-compile',
;; which behaves like `progn' when loaded uncompiled) runs at load time -- so a
;; top-level (require 'org) would pull org in before evil and break the intended
;; evil -> org -> org-roam order (org-roam's :config uses `evil-define-key').
;; The import commands require both lazily instead.

(defgroup my/org-calendar nil
  "Ingest external calendars (ics/Outlook) into Org."
  :group 'org)

(defcustom my/org-calendar-file-name "calendar.org"
  "Basename of the aggregated calendar file under `org-directory'."
  :type 'string
  :group 'my/org-calendar)

(defcustom my/org-calendar-inbox-name "inbox"
  "Subdirectory of `org-directory' watched for dropped .ics files."
  :type 'string
  :group 'my/org-calendar)

(defun my/org-calendar-file ()
  "Absolute path to the aggregated calendar file.
Resolved lazily so it tracks `org-directory', which is bound by my-app-org.el
after this module has loaded."
  (expand-file-name my/org-calendar-file-name
                    (or (bound-and-true-p org-directory) "~/Documents/memex/")))

(defun my/org-calendar-inbox ()
  "Absolute path (with trailing slash) to the watched ics inbox directory."
  (file-name-as-directory
   (expand-file-name my/org-calendar-inbox-name
                     (or (bound-and-true-p org-directory) "~/Documents/memex/"))))

(defun my/org-calendar-default-dir ()
  "Sensible starting directory for the interactive ics file picker.
Prefer the inbox, else `org-directory', else the current directory -- so the
minibuffer never opens on a non-existent path."
  (let ((inbox (my/org-calendar-inbox)))
    (cond ((file-directory-p inbox) inbox)
          ((and (bound-and-true-p org-directory)
                (file-directory-p org-directory))
           org-directory)
          (t default-directory))))

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

(defun my/org-ics--event->entry (event category)
  "Return (UID . ORG-ENTRY-STRING) for ical EVENT, or nil if it has no start.
CATEGORY, when non-nil, is written to a `:CATEGORY:' property for agenda
grouping/coloring.  DESCRIPTION is intentionally omitted to avoid dumping
meeting bodies into the file."
  (let* ((dtstart (icalendar--get-event-property event 'DTSTART))
         (ts (and dtstart
                  (ignore-errors
                    (my/org-ics--timestamp
                     dtstart
                     (icalendar--get-event-property event 'DTEND)
                     (icalendar--get-event-property-attributes event 'DTSTART)
                     (icalendar--get-event-property event 'RRULE))))))
    (when ts
      (let* ((summary (my/org-ics--clean
                       (or (icalendar--get-event-property event 'SUMMARY)
                           "(no title)")))
             (uid (or (icalendar--get-event-property event 'UID)
                      (concat "nouid-" (md5 (concat dtstart summary)))))
             (location (let ((l (icalendar--get-event-property event 'LOCATION)))
                         (and l (not (string-empty-p l)) (my/org-ics--clean l)))))
        (cons uid
              (apply #'concat
                     (delq nil
                           (list
                            (format "* %s\n" summary)
                            ":PROPERTIES:\n"
                            (format ":UID: %s\n" uid)
                            (and category (format ":CATEGORY: %s\n" category))
                            (and location (format ":LOCATION: %s\n" location))
                            ":END:\n"
                            (format "%s\n" ts)))))))))

;; ---- import --------------------------------------------------------------

(defun my/org-import-ics (file &optional category)
  "Upsert events from ics FILE into the aggregated calendar file.
Idempotent by `:UID:'.  CATEGORY (default: FILE's basename) tags entries so the
agenda can group/color by source (e.g. \"club\", \"outlook\").  Interactively,
a prefix arg prompts for CATEGORY.  Returns (ADDED UPDATED SKIPPED)."
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
         (buf (find-file-noselect (my/org-calendar-file)))
         (added 0) (updated 0) (skipped 0))
    (with-current-buffer buf
      (org-with-wide-buffer
       (when (= (buffer-size) 0)
         (insert "#+title: Calendar (imported events, upserted by :UID:)\n"
                 "# Managed by my/org-import-ics. UID-less entries are preserved.\n\n"))
       (dolist (event events)
         (let ((pair (my/org-ics--event->entry event category)))
           (if (null pair)
               (cl-incf skipped)
             (let* ((uid (car pair))
                    (entry (cdr pair))
                    (pos (org-find-property "UID" uid)))
               (if pos
                   (progn
                     (goto-char pos)
                     (org-back-to-heading t)
                     (delete-region (point)
                                    (save-excursion
                                      (org-end-of-subtree t t) (point)))
                     (insert entry)
                     (cl-incf updated))
                 (progn
                   (goto-char (point-max))
                   (unless (bolp) (insert "\n"))
                   (insert entry)
                   (cl-incf added))))))))
      (save-buffer))
    (when (called-interactively-p 'interactive)
      (message "ics import [%s]: +%d ~%d skip %d" category added updated skipped))
    (list added updated skipped)))

(defun my/org-import-ics-inbox (&optional dir archive)
  "Import every .ics file in DIR (default `my/org-calendar-inbox').
With a prefix arg (ARCHIVE non-nil), move each processed file into DIR/archive/
after import.  Category is inferred per file from its basename."
  (interactive (list nil current-prefix-arg))
  (let* ((dir (or dir (my/org-calendar-inbox)))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.ics\\'")))
         (a 0) (u 0) (s 0))
    (if (null files)
        (message "No .ics files in %s" dir)
      (dolist (f files)
        (pcase-let ((`(,fa ,fu ,fs) (my/org-import-ics f)))
          (cl-incf a fa) (cl-incf u fu) (cl-incf s fs))
        (when archive
          (let ((adir (expand-file-name "archive/" dir)))
            (make-directory adir t)
            (rename-file f (expand-file-name (file-name-nondirectory f) adir) t))))
      (message "Inbox import: +%d ~%d skip %d across %d file(s)"
               a u s (length files)))))

(my/define-key
 (:map global-map
       :prefix "C-c k"
       :key
       "i" #'my/org-import-ics
       "I" #'my/org-import-ics-inbox))

(provide 'my-app-calendar)
;;; my-app-calendar.el ends here
