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
  ;;
  ;; Not sticky, because this agenda is a clock-relative page: almost
  ;; everything foresight draws is measured from the moment of the draw -- the
  ;; reserve still to be spent, what a gap can still hold, whether the day is
  ;; behind or ahead, which work would fit.  A sticky agenda hands back a page
  ;; built earlier, `org-agenda-use-sticky-p' throwing straight out of the
  ;; build, so an afternoon `a' can offer gaps that closed at lunch with only
  ;; a line in the echo area to say the page is old.
  ;;
  ;; What being sticky saved was the rebuild, and the rebuild is cheap: 14ms
  ;; for a day and 21ms for a week, under one frame either way.  What it costs
  ;; is holding two views open side by side, and `q' killing the buffer rather
  ;; than burying it.
  (setq calendar-holidays nil
        org-deadline-warning-days 4
        org-agenda-window-setup 'reorganize-frame
        org-agenda-sticky nil
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
        ;;
        ;; Effort before the leader, not after it.  An estimate is a number
        ;; read down a column and compared with the ones above it, and behind
        ;; `% s' it moves with whatever word that row needed -- `Scheduled: ',
        ;; `Deadline:  ', `Sched.3x: ', or nothing at all.  In front of it the
        ;; estimate sits at one column on every row that carries a clock, and
        ;; it is the leader that wanders, which costs nothing: nobody lines up
        ;; words.
        org-agenda-prefix-format '((agenda . "     %-8.8c%?-12t%?-5e% s")
                                   (timeline . "  % s")
                                   (todo . "  %-8c %-7e")
                                   (tags . "  %i %-5c %-7e")
                                   (search . " %i %-12c"))
        org-agenda-timegrid-use-ampm nil
        ;; The agenda opens with no log at all, and `v' asks for it in three
        ;; levels:
        ;;   v l  (Log)         . closed                (this variable)
        ;;   v L  (Log all)     . + state + clock       (all item types)
        ;;   v c  (Clock check) . clock + consistency audit (gaps/overlaps/…)
        ;; `clock' is deliberately dropped from `l' so time-tracking detail lives
        ;; in the end-of-agenda viz tables and `L'.
        ;;
        ;; And `state' with it.  The forward view now says what is *left* of the
        ;; day -- a gap that has gone offers nothing -- which leaves the morning
        ;; a blank and raises the only question worth asking about it: what came
        ;; of it.  `closed' answers that in one line per finished thing.  State
        ;; changes answer a different question, at three lines to the same fact,
        ;; and `L' still has them.
        ;;
        ;; Note that `v l' toggles while `v L' assigns: pressing `L' twice leaves
        ;; the log on, and `l' is what turns it back off.  That is org's own
        ;; `org-agenda-log-mode', not a local choice.
        org-agenda-log-mode-items '(closed)
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
                  ((org-agenda-overriding-header "Waiting on · handed over · needs someone")
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
  ;;
  ;; It is coupled to the *order* of the prefix too, which is easy to forget
  ;; and silent when broken.  Upstream joins the time range to the literal
  ;; `Clocked:' with `[[:space:]]+', which assumes nothing sits between them.
  ;; Putting the effort in front of `% s' puts `0:45' there, the join fails,
  ;; and the regexp falls into the `\|.*' branch that exists for a *running*
  ;; clock -- so the end-time groups come back nil and `C-j' / `C-k' stop
  ;; working on the clock-out while still working on the clock-in.  Worse, it
  ;; keeps working on entries that have no effort, which is what makes it look
  ;; like an intermittent fault rather than a settings clash.
  ;;
  ;; The extra group below is that hole: an optional duration between the
  ;; range and `Clocked:'.
  ;;
  ;; The category group is lazy for a related reason.  Upstream ends it at a
  ;; literal colon, which Org's own prefix supplies (`%-12:c'); this one does
  ;; not (`%-8.8c'), so a greedy `[^:]+' runs on until the colon inside the
  ;; clock-in time and only backtracks as far as it must.  It stops one
  ;; character too late, and since the field lookup counts a boundary as
  ;; belonging to the earlier field, the first digit of the clock-in -- the
  ;; leading zero, the natural place to land coming from the left -- reads as
  ;; category and nothing happens there.  Lazy, it stops at the name.
  (setq org-agenda-time-leading-zero t
        org-clock-convenience-clocked-agenda-re
        (concat "^ +\\([^:]*?\\)[[:space:]]*"
                "\\(\\([ \t012][0-9]\\):\\([0-5][0-9]\\)\\)"        ; clock-in
                "\\(?:-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)?" ; clock-out
                "\\(?:[[:space:]]+[0-9]+:[0-5][0-9]\\)?"              ; the effort field
                "[[:space:]]+Clocked:[[:space:]]+"
                "\\(([0-9]+:[0-5][0-9])\\|(-)\\)"))

  ;; Why nothing happened, in a sentence rather than in a backtrace.
  ;;
  ;; These three read the rendered line, so they act on one kind of row -- the
  ;; `Clocked:' log line -- and on the two time fields in it, and nowhere
  ;; else.  Off them they fail inside the field lookup with "No such field
  ;; name: nil", which names an internal no key on this keyboard asked for.
  ;;
  ;; The common case is worse than a bad message.  `v l' shows `closed' alone,
  ;; so on an ordinary day the page carries no eligible row at all and every
  ;; keystroke fails the same way, which reads as a broken command rather than
  ;; as a view that is not showing clocks.  `v L' and `v c' are what put them
  ;; on the page.
  (defun my/org-clock-convenience-say-why (orig &rest args)
    "Run ORIG with ARGS, or name what would have to be true first."
    (let* ((row (save-excursion
                  (beginning-of-line)
                  (looking-at org-clock-convenience-clocked-agenda-re)))
           (field (and row (ignore-errors
                             (org-clock-convenience-at-timefield-p)))))
      (cond
       (field (apply orig args))
       (row (user-error "Put point on one of the two clock times on this row"))
       ((save-excursion
          (goto-char (point-min))
          (re-search-forward org-clock-convenience-clocked-agenda-re nil t))
        (user-error
         "This row carries no clock; the rows that do read \"Clocked:\""))
       (t (user-error
           "No clocked rows on this page -- `v L' or `v c' shows them")))))

  (dolist (cmd '(org-clock-convenience-timestamp-up
                 org-clock-convenience-timestamp-down
                 org-clock-convenience-fill-gap))
    (advice-add cmd :around #'my/org-clock-convenience-say-why)))

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
  ;; Started two seconds after the keyboard goes quiet, rather than during
  ;; startup.  `:defer\=' with a number is `run-with-idle-timer\=': the package
  ;; is required once the first pause comes, so its cost lands where nobody is
  ;; waiting on it.  Watching a second of the session go unrecorded is not a
  ;; cost worth the name, and it is a second nobody was working in.
  :defer 2
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
        org-foresight-work     '(("08:15" . "12:15") ("13:30" . "18:30"))
        org-foresight-workdays '(1 2 3 4 5))

  ;; Every imported meeting carries a Teams link, so a LOCATION alone cannot
  ;; say where the body has to be.  Only these say it; anything else leaves me
  ;; where I already was.
  ;; Nothing can be worked from a pool, so the day does not wait in one: what
  ;; took me there ends and I set off, and the hours that frees land somewhere
  ;; they are worth something -- at the office rather than in a changing room.
  (setq org-foresight-unworkable-places '(gym))

  (setq org-foresight-places '((office . "本社\\|会議室\\|オフィス")
                               (client . "様\\|訪問\\|先方")
                               ;; Both scripts: the match ignores case, so
                               ;; `GYM' needs nothing extra, and カタカナ is a
                               ;; different string rather than a different
                               ;; case of the same one.
                               (gym . "gym\\|ジム"))
        org-foresight-home-place 'home
        org-foresight-travel-matrix '(((home . gym) . 15)
                                      ((home . office) . 75)
                                      ((home . client) . 75)
                                      ((gym . office) . 55)
                                      ((office . client) . 60))
        org-foresight-travel-default 60)

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

  ;; The hours that go on answering people.  Real, recurring, and the ones
  ;; still unrecorded at six o'clock, because naming each of them costs more
  ;; than the two minutes it took to reply.  `C' in the agenda now offers this
  ;; as an answer and asks only what it was about, filing the clock on a
  ;; `comms' heading under that work -- so the time keeps its subject without
  ;; anyone typing the word.
  ;;
  ;; Left unnamed these hours are indistinguishable from hours nobody can
  ;; account for, and a week of them teaches the reserve that the whole day
  ;; leaks.
  (setq org-foresight-clock-fill-kinds '("comms"))

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

  ;; Org's `now' line in the colour the bar rules itself at, which is life's
  ;; colour: what is left of the day is the part still worth defending, and
  ;; the two marks say the same thing about the same moment.  Set here rather
  ;; than in the theme because it is Org's face, not the package's.
  (set-face-attribute 'org-agenda-current-time nil
                      :inherit 'org-foresight-report-now
                      :foreground 'unspecified)

  (my/define-key
   (:map org-agenda-mode-map
         :key
         ;; The board, from inside the day.  Bound as well as reachable through
         ;; `C-c a b' so the verdict line can name a key rather than an M-x:
         ;; `substitute-command-keys' resolves what is bound, and a dispatcher
         ;; entry is not.
         "B" #'org-foresight-board
         ;; A proposal for the rest of the day: what has been accepted but not
         ;; yet placed, laid into the free hours nearest deadline first and
         ;; net of the reserve, in a buffer that writes nothing until it has
         ;; been read.  Distinct from the `↳' rows the agenda draws under a
         ;; gap -- those report what would fit in one hole and choose nothing,
         ;; so the same task appears under every hole it fits in.  Over
         ;; `org-agenda-phases-of-moon', the last of the almanac commands in
         ;; this map; the Julian and Mayan dates went the same way for `C'.
         "M" #'org-foresight-plan-fill
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
         "S" #'org-foresight-mark-surge
         ;; A derived journey answers to none of Org's commands -- there is no
         ;; entry behind it -- so making one real needs a key of its own.  Over
         ;; `org-agenda-show-tags', which says nothing this prefix does not.
         "T" #'org-foresight-book-travel
         ;; Filling in a clock is remembering, and remembering happens while
         ;; looking at the day it went missing from.  Over
         ;; `org-agenda-convert-date', which offers the Julian and Mayan
         ;; dates and has never been wanted here.
         "C" #'org-foresight-clock-fill
         ;; The other half of the same repair, and a separate command because
         ;; it asks the opposite first question: `C' offers the stretches no
         ;; clock covers, this offers the stretches a clock covers wrongly.
         ;; Over `org-agenda-holidays', which lists `calendar-holidays' -- set
         ;; to nil a hundred lines up, so the key shows an empty page today.
         "H" #'org-foresight-clock-split
         ;; Preparation is decided one invitation at a time.  The bulk
         ;; command offers every meeting that has none, which is the right
         ;; shape once a week and the wrong one for the invitation that just
         ;; arrived -- most meetings need nothing, and a prompt that asks
         ;; about all of them at once gets answered "no".  Over
         ;; `org-agenda-priority-down', which the priority axis here does not
         ;; use.
         "P" #'org-foresight-prepare-meeting)))

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
