;;; my-app-org-agenda.el --- The day -*- lexical-binding: t; -*-

;;; Commentary:
;; The day, in every form I look at it: the agenda and the commands built on
;; it, org-foresight (which adds the parts of a day that are not entries --
;; journeys, gaps, the edges of the working span, how much may still be
;; promised), the timeline and the blocks, and ActivityWatch, which is where
;; the account of the day that was actually spent comes from.
;;
;; Split from my-app-org.el by concern rather than by size.  That file is
;; about Org -- files, capture, refile, clocking, roam, export.  This one is
;; about the one question those feed: what does today look like, and can it
;; be worked.  A thing belongs here if it would still make sense with the
;; word "today" in front of it.
;;
;; org-foresight holds only generic mechanism; everything personal is a value,
;; and values live here.  Where a setting needs explaining, the explanation
;; belongs in the package's own docstring, so `C-h v' answers it wherever the
;; package is used.  What stays here is only what is true of me and could not
;; be true of the package -- so there is no wiring below, only values: the
;; package attaches itself to the agenda when it loads.

;;; Code:

;; Do NOT `require' org or org-agenda at top level.  Modules are loaded as
;; *source*, before my-editor-evil; a top-level require would pull org in
;; ahead of evil and break the intended evil -> org -> org-roam order (see
;; my-app-calendar.el for the same note).  Everything below is deferred by
;; `use-package', and every dependency is stated as `:after' rather than left
;; to the order the files happen to be read in.

(use-package org-agenda
  :straight nil
  ;; `my-app-org' as well as evil: init.el walks `directory-files', and `-'
  ;; (0x2D) sorts before `.' (0x2E), so this file is read *before*
  ;; my-app-org.el.  Waiting on that module's feature rather than on its
  ;; filename keeps org's own `use-package' -- and the `org-directory' it
  ;; sets -- ahead of anything here that would pull org in.
  :after (evil my-app-org)
  :config
  (dolist (key '("z" "g" "/" "n" "N" ":"))
    (define-key org-agenda-mode-map (kbd key)
                (lookup-key evil-motion-state-map (kbd key))))
  
  (my/define-key
   (:map org-agenda-mode-map
         :key
         "s" #'org-agenda-schedule
         "Z" #'org-resolve-clocks
         "h" #'left-char
         "j" #'org-agenda-next-line
         "k" #'org-agenda-previous-line
         "l" #'right-char
         "@" #'org-agenda-columns
         "[" #'org-agenda-filter
         "]" #'org-agenda-filter-by-tag
         my/backslash #'evil-avy-goto-char-timer
         "C-f" #'evil-scroll-page-down
         "C-b" #'evil-scroll-page-up
         "C-w" #'evil-window-map)
   (:map org-agenda-mode-map
         :state emacs motion normal
         :key
         "gr" #'org-agenda-redo))

  ;; Ex command in agenda-mode
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (my/evil-ex-define-cmd-local "w[rite]" #'org-save-all-org-buffers)))

  ;; Agenda settings
  (setq calendar-holidays nil
        org-deadline-warning-days 4
        org-agenda-window-setup 'reorganize-frame
        org-agenda-sticky t
        org-agenda-persistent-filter t
        org-agenda-persistent-marks nil
        org-agenda-span 'day
        org-agenda-start-on-weekday 0
        org-agenda-start-with-log-mode nil
        org-agenda-start-with-clockreport-mode nil
        org-agenda-start-with-entry-text-mode nil
        org-agenda-start-with-follow-mode nil
        org-agenda-view-columns-initially nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-scheduled-delay-if-deadline nil
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-agenda-skip-timestamp-if-deadline-is-shown nil
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-additional-timestamps-same-entry nil
        org-agenda-dim-blocked-tasks t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies nil
        org-agenda-sorting-strategy '((agenda time-up category-keep)
                                      (todo habit-down category-up time-up)
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-agenda-default-appointment-duration nil
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-block-separator 62
        org-agenda-compact-blocks t
        ;; Five leading spaces on the daily agenda, not two.  One is the
        ;; margin every block's body starts at -- the frame edge belongs to
        ;; the badges -- and the listing is a body like any other.  The rest
        ;; is the gutter: two brackets, the blank between them, and the usual
        ;; gap before the text.  Every column org-foresight counts is read off
        ;; the rows themselves, so widening this moves the marks with it.
        org-agenda-prefix-format '((agenda . "     %-8.8c%?-12t% s%?-5e")
                                   (timeline . "  % s")
                                   (todo . "  %-8c %-7e")
                                   (tags . "  %i %-5c %-7e")
                                   (search . " %i %-12c"))
        org-agenda-timegrid-use-ampm nil
        ;; The agenda opens with no log at all, and `v' asks for it in three
        ;; levels:
        ;;   v l  (Log)         . closed + state       (this variable)
        ;;   v L  (Log all)     . + clock              (all item types)
        ;;   v c  (Clock check) . clock + consistency audit (gaps/overlaps/…)
        ;; `clock' is deliberately dropped from `l' so time-tracking detail lives
        ;; in the end-of-agenda viz tables and `L'.
        ;;
        ;; Note that `v l' toggles while `v L' assigns: pressing `L' twice leaves
        ;; the log on, and `l' is what turns it back off.  That is org's own
        ;; `org-agenda-log-mode', not a local choice.
        org-agenda-log-mode-items '(closed state)
        org-clock-report-include-clocking-task t
        ;; The daily agenda's own "time by area" view is now rendered by the
        ;; custom CATEGORY block (`org-foresight-report-clocked'), so the
        ;; native agenda clockreport is off by default.  This plist only shapes
        ;; interactive clocktables (`C-c C-x C-r', `v c'): tag-free, indented,
        ;; sorted by clocked time.
        org-agenda-clockreport-parameter-plist
        '(:maxlevel 5 :lang "en" :scope agenda :block today :fileskip0 t :emphasize t :link t :narrow 40! :indent t :tcolumns 1 :sort (3 . ?T))
        org-clocktable-defaults org-agenda-clockreport-parameter-plist)
  
  ;; Override indent string for clocktable
  (defun my/org-clocktable-indent-string (level)
    "Return indentation string for org clocktable at LEVEL.
Top-level (1) entries have no indent. Deeper levels are indented by spaces."
    (if (= level 1) ""
      (concat "\\_" (substring (make-string (1- level) ?\s) 1))))
  (advice-add 'org-clocktable-indent-string :override 'my/org-clocktable-indent-string)
  
  (defun my/toggle-org-agenda-breadcrumbs ()
    "Toggle breadcrumbs (%b) in the current agenda view."
    (interactive)
    (let* ((type (cl-find-if (lambda (typ) (org-agenda-check-type nil typ))
                             '(agenda timeline todo tags search)))
           (prefix (cdr (assoc type org-agenda-prefix-format)))
           (new (if (and prefix (string-match "%b" prefix))
                    (replace-regexp-in-string "%b" "" prefix)
                  (concat (or prefix "") "%b"))))
      (setq org-agenda-prefix-format
            (cons (cons type new)
                  (assq-delete-all type org-agenda-prefix-format)))
      (org-agenda-redo)
      (message "Breadcrumbs %s" (if (string-match "%b" new) "enabled" "disabled"))))

  (defun my/org-agenda-view-mode-dispatch-breadcrumbs (orig-fn)
    "Extend `org-agenda-view-mode-dispatch' with a [b]readcrumbs option.
Reads the key ourselves so the prompt can list it; any other key is pushed
back via `unread-command-events' for ORIG-FN's own dispatch to consume, so
org's existing key table stays the single source of truth."
    (org-unlogged-message
     "View: [d]ay  [w]eek  for[t]night  [m]onth  [y]ear  [SPC]reset  [q]uit/abort
       time[G]rid   [[]inactive  [f]ollow      [l]og    [L]og-all   [c]lockcheck
       [a]rch-trees [A]rch-files clock[R]eport include[D]iary       [E]ntryText
       [b]readcrumbs")
    (let ((key (read-char-exclusive)))
      (if (eq key ?b)
          (my/toggle-org-agenda-breadcrumbs)
        (setq unread-command-events (cons key unread-command-events))
        (funcall orig-fn))))

  (advice-add 'org-agenda-view-mode-dispatch :around
              #'my/org-agenda-view-mode-dispatch-breadcrumbs)

  ;; Custom agenda commands.  Two, and they ask different questions of
  ;; different periods: today, and the week just gone.  A third used to draw
  ;; today's agenda again with a different block underneath, and was never
  ;; opened -- a view whose top half is identical to another is not a place,
  ;; it is a toggle.  What it had to say now lives on the board, which is not
  ;; an agenda view at all (`C-c a b').
  (setq org-agenda-custom-commands
        '(("r" "Weekly review — past 7 days"
           ((agenda "" ((org-agenda-overriding-header "Clock check · past week")
                        (org-agenda-span 'week)
                        (org-agenda-start-day "-1w")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-show-log 'clockcheck) ; clock lines + consistency audit
                        (org-agenda-todo-ignore-scheduled t)
                        (org-habit-show-habits nil)))
            ;; Roughly the "n" command below; kept here so review is one stop.
            (todo "" ((org-agenda-overriding-header "Unscheduled / stuck TODOs")
                      (org-agenda-todo-ignore-with-date t))))
           ;; General settings apply during `org-agenda-finalize' (see
           ;; `org-agenda-run-series'), so binding the report style here switches
           ;; the finalize append from the daily tables to the week-by-area
           ;; review table.  `compact-blocks' is off here (unlike the daily `a')
           ;; so each block's orientation header is shown in the review.
           ((org-foresight-report-style 'review)
            (org-agenda-compact-blocks nil)))
          ;; `C-c a b' kept its key through the rewrite on purpose: the
          ;; question at the door has not changed, only the answer -- what only
          ;; being here can settle, and what has not been planned for at all.
          ("b" "Board — what has not been settled" org-foresight-board "")
          ("d" "People — work with someone in it"
           ;; Everything a person is part of, whichever way round: WAIT and
           ;; DELEG are with them, anything else with a `:PEOPLE:' needs them.
           ;; One property says who, and the state says which -- so this is one
           ;; list, not two that would have to be read together.
           ;;
           ;; DELEG is a done-type keyword, so the `tags' type is used (it lists
           ;; done entries, unlike `todo'); sorted by SCHEDULED (the follow-up
           ;; date) so overdue check-ins float to the top.
           ((tags "TODO=\"WAIT\"|TODO=\"DELEG\"|PEOPLE={.}"
                  ((org-agenda-overriding-header "人が絡む仕事 — 待ち・委譲・要同席 (PEOPLE)")
                   (org-agenda-sorting-strategy '(scheduled-up priority-down)))))
           ((org-foresight-report-style nil))))))

(use-package adaptive-wrap
  :after org-agenda
  :config
  (setq adaptive-wrap-extra-indent 20)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq truncate-lines t)
              (adaptive-wrap-prefix-mode t))))

(use-package org-clock-convenience
  :after org-agenda
  :config
  (my/define-key
   (:map org-agenda-mode-map
         :key
         "C-j" #'org-clock-convenience-timestamp-up
         "C-k" #'org-clock-convenience-timestamp-down
         "C-o" #'org-clock-convenience-fill-gap))

  ;; `org-clock-convenience' re-parses time out of the *rendered* agenda text
  ;; rather than any data structure, so its regexp expects a fixed 2-char hour
  ;; field -- `org-agenda-time-leading-zero' (a core agenda display setting)
  ;; lives here, not with the other prefix-format settings, specifically to
  ;; keep that assumption true.
  (setq org-agenda-time-leading-zero t
        org-clock-convenience-clocked-agenda-re
        "^ +\\([^:]+\\)[[:space:]]*\\(\\([ \t012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)"))

;;;; org-foresight -- the forward-looking half of the day

;; The capture this depends on is the "i" (interrupt) template, and it lives
;; with the other templates in my-app-org.el.  It was here once, next to the
;; settings that read what it writes, and that could not work: `use-package
;; org' runs its `:config' -- which `setq's the whole template list -- *after*
;; `use-package org-capture' runs its own, so anything appended from here was
;; silently thrown away.  Measured, not guessed: use-package's own statistics
;; timestamp org-capture's config at 35.346 and org's at 35.761.

;; ActivityWatch -- where the Observed table, the leak and the lost come from.
;; The only thing in Emacs that reads it is org-foresight, and
;; `activity-watch-org-clock-active' is the setting that makes its rows carry
;; the task being clocked: without it the day can be told what was on screen
;; but not what it was for.
(use-package activity-watch-mode
  :diminish (activity-watch-mode " aw")
  :config
  (setopt activity-watch-org-clock-active t)
  (global-activity-watch-mode))

(use-package org-foresight
  :straight (org-foresight :host github :repo "yoshzucker/org-foresight"
                           :files ("*.el"))
  :after org-agenda
  :config
  ;; The hours being defended, not the hours actually worked.  A list, so a
  ;; day that breaks can say so: add an interval and the gap stops being
  ;; capacity, stops being offered, and stops being planned through.
  (setq org-foresight-awake    '("06:50" . "22:00")
        org-foresight-work     '(("08:15" . "17:45"))
        org-foresight-workdays '(1 2 3 4 5))

  ;; Every imported meeting carries a Teams link, so a LOCATION alone cannot
  ;; say where the body has to be.  Only these say it; anything else leaves me
  ;; where I already was.
  (setq org-foresight-places '((office . "本社\\|会議室\\|オフィス")
                               (client . "様\\|訪問\\|先方"))
        org-foresight-home-place 'home
        org-foresight-travel-matrix '(((home . office) . 60)
                                      ((home . client) . 75)
                                      ((office . client) . 45))
        org-foresight-travel-default 45)

  ;; Life, in the three calendars it arrives from.  These occupy the day and
  ;; are subtracted from what may still be promised, but they are never
  ;; counted as work: an hour at the dentist inside working hours is an hour
  ;; the day cannot spend, not an hour of work that got done.  Nor are they
  ;; ever offered as the thing to move when the day is overfull.
  (setq org-foresight-private-categories '("family" "personal" "event")
        ;; The child's club calendar says when the house is empty and takes
        ;; none of my time.  A meeting I only have to hear does cost the hour,
        ;; but that is per entry (:ATTENTION: background), not per category.
        org-foresight-informational-categories '("club")
        org-foresight-background-categories nil)

  (setq org-foresight-surge-default "1:00"
        org-foresight-surge-window  20)

  ;; Which days are worked from the office.  Left empty until the pattern is
  ;; steady: an unlisted day is worked from `org-foresight-home-place', and a
  ;; day that goes differently says so on its own heading with
  ;; \[org-foresight-shape-day].  What this buys is the question at the door --
  ;; work that needs the office is not late until the next office day has gone,
  ;; and until the day has a place there is no way to ask when that is.
  (setq org-foresight-day-places nil)

  ;; The two ends of the day, booked because they happen.  Ten minutes at the
  ;; desk on arrival to see what the day is, and ten before leaving it to see
  ;; what only being here can settle -- the second is what `C-c a b' was made
  ;; for, and until it had time of its own it was taken out of whatever came
  ;; last.  The keys are resolved when the row is drawn, so they keep naming
  ;; the right ones; `C-c a a' is a custom agenda command and has no binding
  ;; to resolve, so it is written out.
  (setq org-foresight-check-in
        '(:minutes 10 :title "look at the day (C-c a a)")
        org-foresight-check-out
        '(:minutes 10 :title "before you leave (\\[org-foresight-board])"))

  ;; WAIT means somebody else has it *and it comes back to me*, so a SCHEDULED
  ;; date on one is a check-in rather than a start, and one that has gone by
  ;; is a thing to chase.  DELEG is not on the list: it means the work left my
  ;; hands for good, and chasing it would be chasing something that is not
  ;; mine (see `org-todo-keywords' in my-app-org.el for the axis).
  (setq org-foresight-followup-keywords '("WAIT"))

  ;; Only the imported work calendar implies preparation; the club one does not.
  (setq org-foresight-meeting-categories '("meeting")
        org-foresight-meeting-prep   "0:30"
        org-foresight-meeting-follow "0:15")

  ;; A task is a subject, and subjects live in the journal date tree.
  (setq org-foresight-task-file (expand-file-name "journal.org" org-directory)
        org-foresight-day-file  (expand-file-name "journal.org" org-directory)
        org-foresight-task-datetree t
        org-foresight-task-todo "NEXT")

  (setq org-foresight-wip-keywords '("ONGO")
        org-foresight-wip-limit 2)

  ;; Measured against this journal it matched 17% of all headings -- diary
  ;; entries and date-tree scaffolding, not problems -- and a board mostly
  ;; listing non-problems stops being read.
  (setq org-foresight-undecided-enabled nil)

  ;; Neither of these closes with a clock that measures the estimate it was
  ;; given.  CANCEL is the obvious one: an hour's job dropped after ten minutes
  ;; would be read as evidence that hours take minutes.  DELEG is the subtler
  ;; one -- its clock is the time *I* spent handing the work over, the mails
  ;; and the briefing, while its EFFORT was an estimate of the whole job.  The
  ;; two are about different work, so comparing them teaches nothing true.
  (setq org-foresight-bias-enabled t
        org-foresight-bias-abandoned-keywords '("CANCEL" "DELEG")
        org-foresight-report-style 'daily)

  (my/define-key
   (:map org-agenda-mode-map
         :key
         ;; The board, from inside the day.  Bound as well as reachable through
         ;; `C-c a b' so the verdict line can name a key rather than an M-x:
         ;; `substitute-command-keys' resolves what is bound, and a dispatcher
         ;; entry is not.
         "B" #'org-foresight-board
         "P" #'org-foresight-plan-fill
         ;; The shape of the day under the cursor, which is why it is bound
         ;; here rather than left to `M-x': the day that goes differently is
         ;; almost never today, and declaring it is only useful while looking
         ;; at the week it sits in.  Over `org-agenda-toggle-diary', which
         ;; nothing here uses.
         "D" #'org-foresight-shape-day
         ;; Whether a meeting needs all of the hour or will share it is
         ;; decided while looking at the day, so it is set from here.
         "A" #'org-foresight-set-attention
         ;; "That landed on me" is realised while looking at the day too --
         ;; usually about something already in the file, which the interrupt
         ;; capture never saw.
         "S" #'org-foresight-mark-surge)))

;;;; Other views of the same day
;; Neither reads the agenda, and neither is one: a timeline and a set of
;; blocks are the same hours seen a different way, and they live here because
;; what they are about is the day rather than Org.

(use-package org-dayflow
  :straight (:host github :repo "yoshzucker/org-dayflow")
  :after (org evil)
  :config
  ;; Personal category coloring for the timeline.  Category names live ONLY here
  ;; (never in the org-dayflow package); adjust to match your calendar sources.
  ;; These are the categories org-calsync writes: what a thing is, never where
  ;; it was read from.
  (setq org-dayflow-category-faces
        '(;; The two shapes work arrives in: named, from the work calendar on
          ;; the machine that has it, and nameless, from the busy export.  One
          ;; colour, because on a timeline they are the same hour.
          ("meeting"  . font-lock-keyword-face)
          ("work"     . font-lock-keyword-face)
          ("family"   . font-lock-string-face)
          ;; Household fixtures with a name of their own -- a recital, a match.
          ;; The same colour as `family', because on the timeline they are the
          ;; same thing: the house doing something rather than me.
          ("event"    . font-lock-string-face)
          ("personal" . font-lock-doc-face)))
  (dolist (key '("z" "g" "/" "n" "N" ":"))
    (define-key org-dayflow-mode-map (kbd key)
                (lookup-key evil-motion-state-map (kbd key))))

  (my/define-key
   (:map global-map
         :key
         "C-c d" #'org-dayflow)
   (:map org-dayflow-mode-map
         :key
         my/backslash #'evil-avy-goto-char-timer))

  (evil-set-initial-state 'org-dayflow-mode 'emacs)
  (add-hook 'org-dayflow-mode-hook
            (lambda ()
              (my/evil-ex-define-cmd-local "w[rite]" #'org-save-all-org-buffers))))

(use-package org-timeblock
  :straight (:host github :repo "ichernyshovvv/org-timeblock")
  :after (org evil)
  :config
  ;; `org-timeblock-files' defaults to `(org-agenda-files)', which includes
  ;; calendar.org (via the advice in my-app-calendar.el), so meetings and
  ;; scheduled tasks appear as time blocks.  SVG-rendered -- needs an Emacs built
  ;; with SVG support.
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)

  ;; Same hour range for every column so days line up (the default hides past
  ;; hours per day, giving each column a different start/end).  Integer hours
  ;; only -- org-timeblock renders on whole-hour lines.
  (setq org-timeblock-scale-options '(6 . 23))

  (my/define-key
   (:map global-map :key "C-c b" #'org-timeblock)))

(provide 'my-app-org-agenda)

;;; my-app-org-agenda.el ends here
