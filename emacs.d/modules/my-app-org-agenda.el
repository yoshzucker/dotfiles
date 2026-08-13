;;; my-app-org-agenda.el --- The day: agenda, and what foresight adds to it -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything about the day view lives here: the agenda itself, the custom
;; commands built on it, the packages that exist only to serve it, and
;; org-foresight, which adds to it the parts of a day that are not entries --
;; journeys, gaps, the edges of the working span, and how much may still be
;; promised.
;;
;; Split from my-app-org.el by concern rather than by size.  That file is
;; about Org: capture, refile, clocking, export.  This one is about the one
;; question those feed -- what does today look like, and can it be worked.
;;
;; org-foresight holds only generic mechanism; everything personal is a value,
;; and values live here.  Where a setting needs explaining, the explanation
;; belongs in the package's own docstring, so `C-h v' answers it wherever the
;; package is used.  What stays here is only what is true of me and could not
;; be true of the package.

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
        org-agenda-start-with-log-mode '(state)
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
        org-agenda-prefix-format '((agenda . "  %-8.8c%?-12t% s%?-5e")
                                   (timeline . "  % s")
                                   (todo . "  %-8c %-7e")
                                   (tags . "  %i %-5c %-7e")
                                   (search . " %i %-12c"))
        org-agenda-timegrid-use-ampm nil
        ;; The agenda `v' log menu now reads as three meaningful levels:
        ;;   default (`org-agenda-start-with-log-mode' below) . state changes only
        ;;   v l  (Log)      . closed + state          (this variable)
        ;;   v L  (Log all)  . + clock                 (all item types)
        ;;   v c  (Clock check) . clock + consistency audit (gaps/overlaps/…)
        ;; `clock' is deliberately dropped from `l' so time-tracking detail lives
        ;; in the end-of-agenda viz tables and `L'.  Add it back (e.g. '(state
        ;; clock)) to taste.
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

  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("b" "Before leaving · 席を立つ前"
           ;; One-glance "is anything left undone?" before stepping away.  When
           ;; calendar/meeting events are present in `org-agenda-files', the first
           ;; block shows work + private together (both worlds).
           ((agenda "" ((org-agenda-overriding-header "Today + tomorrow · 予定/締切(両世界)")
                        (org-agenda-span 2)
                        (org-agenda-start-day "+0d")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-with-log-mode nil))) ; forward-looking, not a log
            (todo "NEXT" ((org-agenda-overriding-header "Unscheduled NEXT · 未スケジュール(取りこぼし)")
                          (org-agenda-todo-ignore-with-date t))) ; only undated, i.e. stuck
            (todo "WAIT" ((org-agenda-overriding-header "WAIT · 他者待ち(要ナッジ?)")))
            ;; DELEG is a done-type keyword ("|" ... DELEG); an explicit keyword
            ;; match still lists it, so delegated work stays visible for follow-up.
            (todo "DELEG" ((org-agenda-overriding-header "DELEG · 委譲済み(要フォロー?)"))))
           ;; A status check, not a time-tracking review: suppress the finalize
           ;; time-viz tables and keep per-block headers visible.
           ((org-foresight-report-style nil)
            (org-agenda-compact-blocks nil)))
          ("r" "Weekly review — past 7 days"
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
          ("d" "Delegation board · 委譲・待ち板"
           ;; Everything out with someone: WAIT (I'm blocked on them) + DELEG
           ;; (I handed it off).  DELEG is a done-type keyword so the `tags'
           ;; type is used (it lists done entries, unlike `todo'); sorted by
           ;; SCHEDULED (the follow-up date) so overdue check-ins float to the top.
           ((tags "TODO=\"WAIT\"|TODO=\"DELEG\""
                  ((org-agenda-overriding-header "委譲・他者待ち — 人別 (DELEGATED_TO)")
                   (org-agenda-sorting-strategy '(scheduled-up priority-down)))))
           ((org-foresight-report-style nil)))
          ("p" "Plan · 今日の設計"
           ;; An ordinary agenda block, because the day being rearranged has to
           ;; be the real day: org-foresight adds journeys, gaps and the edges
           ;; of the working span to it, and everything else the agenda knows
           ;; -- logs, habits, tags, blocked dimming -- comes along for free.
           ;; The `plan' style adds underneath the two questions the day cannot
           ;; answer from inside itself: when this could be taken on instead,
           ;; and what has not been asked about at all.
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-start-day "+0d")
                        (org-agenda-start-on-weekday nil))))
           ((org-foresight-report-style 'plan)))))

  (defun my/org-agenda-before-leaving ()
    "Open the pre-departure check (the \"b\" custom command).
Shows today+tomorrow across both worlds plus stuck NEXT / WAIT / DELEG, so
\"is anything left undone?\" is answered in one keystroke before stepping away."
    (interactive)
    (org-agenda nil "b"))

  (defun my/org-roam-person-names ()
    "Titles of all `:person:'-tagged org-roam nodes.
Used as completion candidates when delegating.  Free text is still accepted at
the prompt, so delegating to someone without a person node also works."
    (when (require 'org-roam nil t)
      (seq-uniq
       (seq-keep (lambda (n)
                   ;; File-level nodes only: the `:person:' filetag is inherited
                   ;; by sub-headings, so without this the headings inside a
                   ;; person node (Log, Delegated/Waiting, ...) leak in as names.
                   (and (= 0 (org-roam-node-level n))
                        (member "person" (org-roam-node-tags n))
                        (org-roam-node-title n)))
                 (org-roam-node-list)))))

  (defun my/org-delegate-on-state-change ()
    "When a task enters the DELEG state, capture the delegation metadata.
Runs from `org-after-todo-state-change-hook': prompts for who the task went to
and a follow-up (check-in) date, then records the `:DELEGATED_TO:' property and
SCHEDULEs the follow-up -- so the entry shows up, grouped by person, in the
\"d\" Delegation board.  The existing DELEG(e@) note still records why.

The \"Delegate to\" prompt completes over `:person:' node titles (so names stay
consistent with those nodes, which keeps the per-person `delegated' block
matching) but accepts free text too -- delegation is not limited to reports.
`:DELEGATED_TO:' defaults to any current value, so re-entering DELEG never
loses it.  The follow-up SCHEDULE is set with logging suppressed so it does not
interleave with the pending state-change note.  No-op when non-interactive, so
scripted state changes (and capture, which inserts DELEG text without a state
change) are unaffected."
    (when (and (equal org-state "DELEG") (not noninteractive))
      (let ((who (completing-read "Delegate to: " (my/org-roam-person-names)
                                  nil nil nil nil
                                  (org-entry-get nil "DELEGATED_TO")))
            (followup (org-read-date nil nil nil "Follow-up date")))
        (unless (string-empty-p who)
          (org-set-property "DELEGATED_TO" who))
        (when (and followup (not (string-empty-p followup)))
          (let ((org-log-reschedule nil))
            (org-schedule nil followup))))))

  (add-hook 'org-after-todo-state-change-hook #'my/org-delegate-on-state-change))

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

(use-package org-foresight
  :straight (org-foresight :host github :repo "yoshzucker/org-foresight"
                           :files ("*.el"))
  :after org-agenda
  :config
  (setq org-foresight-awake         '("06:30" . "23:00")
        org-foresight-workday-start "09:00"
        org-foresight-workday-end   "17:30"
        org-foresight-workdays      '(1 2 3 4 5))

  ;; This machine's desktop, in the names macOS actually reports for a
  ;; Japanese locale.  Shared across the dotfiles, so it describes the personal
  ;; machine; the work machine's own list belongs in its `custom-file', which
  ;; is not committed.  What matters beyond the Observed table is that surge
  ;; learning counts only work and comms as displaced work.
  (setq org-foresight-app-categories
        '(("work" . ("Emacs" "Ghostty" "Terminal" "iTerm2" "Code" "Xcode"
                     "プレビュー" "Preview" "Claude" "Grok" "ActivityWatch"))
          ("comms" . ("メール" "Mail" "カレンダー" "Calendar" "Slack"
                      "メッセージ" "Messages" "Zoom"))
          ("distraction" . ("Safari" "Chrome" "Firefox" "YouTube" "X"
                            "Twitter" "Discord"))))

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

  (setq org-foresight-private-categories '("family" "personal")
        ;; The child's club calendar says when the house is empty and takes
        ;; none of my time.  A meeting I only have to hear does cost the hour,
        ;; but that is per entry (:ATTENTION: background), not per category.
        org-foresight-informational-categories '("club")
        org-foresight-background-categories nil)

  (setq org-foresight-surge-default "1:00"
        org-foresight-surge-window  20)

  ;; Both mean "out with someone else", so a SCHEDULED date on them is a
  ;; check-in rather than a start.  DELEG is a done-type keyword here, which
  ;; keeps it off the daily agenda -- exactly why it needs a signal of its own.
  (setq org-foresight-followup-keywords '("WAIT" "DELEG"))

  ;; Only the imported work calendar implies preparation; the club one does not.
  (setq org-foresight-meeting-categories '("outlook")
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

  (setq org-foresight-bias-enabled t
        org-foresight-report-style 'daily)

  (my/define-key
   (:map org-agenda-mode-map
         :key
         "P" #'org-foresight-plan-fill
         ;; Whether a meeting needs all of the hour or will share it is
         ;; decided while looking at the day, so it is set from here.
         "A" #'org-foresight-set-attention))

  (add-hook 'org-agenda-finalize-hook #'org-foresight-report-render t))

(provide 'my-app-org-agenda)

;;; my-app-org-agenda.el ends here
