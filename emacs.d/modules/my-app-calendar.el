;;; my-app-calendar.el --- Calendars, ingested into the day -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything external that has a time on it, brought into one `calendar.org'
;; so it reaches org-agenda, org-foresight and org-dayflow -- none of which
;; need to know where an event came from.
;;
;; The machinery is org-calsync.  What is here is only what is true of me:
;; which feed this machine has, which of its calendars are worth importing,
;; and what the abstracted export is called.
;;
;; One feed per machine, and they are deliberately different.  On the company
;; Windows the source is Outlook over COM -- local, so no application
;; registration, no tenant consent and no token.  On this Mac it is
;; Calendar.app, which already carries the abstracted busy blocks the Windows
;; side mails over, so a single sync brings in private life and the shape of
;; the working week together.
;;
;; The two never meet: `org-directory' is iCloud here and OneDrive there, and
;; those do not sync to each other.  Meeting detail stays on the company
;; machine; the only thing that crosses is the busy ics, which carries a label
;; and a time and nothing else.

;;; Code:

;; Do NOT `require' org at top level.  Modules are loaded as *source* and
;; alphabetically, so this file is read before my-editor-evil; a top-level
;; require would pull org in ahead of evil and break the evil -> org ->
;; org-roam order that org-roam's `:config' depends on.

(use-package org-calsync
  :straight (org-calsync :host github :repo "yoshzucker/org-calsync"
                         :files ("*.el" ("script" "script/*")))
  :after org
  :config
  (pcase system-type
    ('windows-nt
     (require 'org-calsync-outlook)
     ;; A category says what a thing is, so it is `meeting' and not the name
     ;; of the product the calendar happens to live in.
     (setq org-calsync-sources
           '((work :fetch org-calsync-outlook-fetch :category "meeting"))
           org-calsync-outlook-days 30)

     ;; The bridge out.  Japanese, because it is read on my phone.
     (require 'org-calsync-busy)
     (setq org-calsync-busy-label "予定あり"
           org-calsync-busy-days 60
           org-calsync-busy-compose-function #'org-calsync-outlook-compose))

    ('darwin
     (require 'org-calsync-macos)
     (setq org-calsync-sources
           '((private :fetch org-calsync-macos-fetch)))

     ;; Named one at a time, and each with what it is.  Taking the store
     ;; wholesale would bring in the weather feed, the holidays, the birthdays
     ;; and Siri's guesses, and an agenda that shows tomorrow's forecast beside
     ;; tomorrow's meetings is one that stops being read.
     ;;
     ;; `Work' is the working week arriving the long way round: exported from
     ;; the company machine as busy.ics -- times and the word 予定あり, nothing
     ;; else -- mailed out, and subscribed to here.  It is `work' and not
     ;; `meeting' because that is all that survives the crossing: an hour that
     ;; is taken, at work.  Calling it a meeting would be a claim about
     ;; something the file deliberately does not contain, and one this machine
     ;; would then act on -- the board would ask for preparation for a block
     ;; with no subject, which can only be prepared for where the subject is.
     ;;
     ;; The name is the only thing that crosses, so it carries the meaning: an
     ;; ics `CATEGORIES:' line does not survive Calendar.app, whose event
     ;; objects have no such field for the reader to ask for.
     (setq org-calsync-macos-calendars
           '(("Personal" . "personal")
             ("Family"   . "family")
             ("Event"    . "event")
             ("Work"     . "work"))
           org-calsync-macos-days 60)))

  ;; Keep the feature self-contained: rather than hard-coding calendar.org into
  ;; `my/org-agenda-files-refresh', advise it, so deleting this file takes the
  ;; calendar out of the agenda on its own.
  (defun my/org-calendar--add-to-agenda-files (&rest _)
    "Append the calendar file to `org-agenda-files' when it exists."
    (let ((cal (org-calsync-file)))
      (when (and cal (file-exists-p cal))
        (add-to-list 'org-agenda-files cal))))

  (advice-add 'my/org-agenda-files-refresh :after
              #'my/org-calendar--add-to-agenda-files))

(provide 'my-app-calendar)
;;; my-app-calendar.el ends here
