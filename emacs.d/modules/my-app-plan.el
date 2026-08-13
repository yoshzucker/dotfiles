;;; my-app-plan.el --- Forward-looking planning: org-foresight -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for `org-foresight', which supplies the forward-looking half of
;; the agenda: how much of today is still uncommitted, when the day will
;; actually end, and which work exists but has not been planned yet.
;;
;; The package holds only generic mechanism.  Everything personal -- which
;; applications count as work, when the working day starts, which meetings imply
;; preparation -- is a value, and values live here.  Keeping that line means the
;; package stays publishable and this file stays the single place to look when
;; the answer is "because that is how I work".
;;
;; Self-contained: this module owns the finalize hook and the "p" agenda
;; command, so removing it (and its symlink) reverts planning entirely and
;; leaves the rest of the org setup untouched.

;;; Code:

;; Do NOT `require' org or org-foresight at top level.  Modules are loaded as
;; *source* and alphabetically, so this file loads before my-editor-evil; a
;; top-level require would pull org in ahead of evil and break the intended
;; evil -> org -> org-roam order (see my-app-calendar.el for the same note).
;; `use-package' defers everything below to `org-agenda' instead.

(use-package org-foresight
  :straight (org-foresight :host github :repo "yoshzucker/org-foresight"
                           :files ("*.el"))
  :after org-agenda
  :config
  ;; Which applications count as what.  The package ships a generic default;
  ;; this is the local reality, and it matters twice: the Observed table groups
  ;; by it, and surge learning counts only work/comms leak as displaced work
  ;; (time lost to `distraction' is not capacity that a plan can reclaim).
  (setq org-foresight-app-categories
        '(("work" . ("Emacs" "Ghostty" "Terminal" "iTerm2" "Code" "Xcode"
                     "プレビュー" "Preview" "Claude" "Grok" "ActivityWatch"))
          ("comms" . ("メール" "Mail" "カレンダー" "Calendar" "Slack"
                      "メッセージ" "Messages" "Zoom"))
          ("distraction" . ("Safari" "Chrome" "Firefox" "YouTube" "X"
                            "Twitter" "Discord"))))

  ;; The shape of an ordinary day.  `-end' is the hour I intend to leave, not
  ;; the hour I sometimes leave: the number exists to be defended, so setting
  ;; it to the observed average would defeat the whole point of having it.
  ;; An unusual day is declared on its own heading with M-x org-foresight-shape-day.
  (setq org-foresight-awake          '("06:30" . "23:00")
        org-foresight-workday-start  "09:00"
        org-foresight-workday-end    "17:30"
        org-foresight-workdays       '(1 2 3 4 5)
        org-foresight-day-file (expand-file-name "journal.org" org-directory))

  ;; Places, and what it costs to be at them.  Every imported meeting carries a
  ;; Teams link, so LOCATION alone cannot say where the body has to be -- only
  ;; a place written down deliberately counts, and anything else leaves me
  ;; where I already was.
  (setq org-foresight-places '((office . "本社\\|会議室\\|オフィス")
                               (client . "様\\|訪問\\|先方"))
        org-foresight-home-place 'home
        org-foresight-travel-matrix '(((home . office) . 60)
                                      ((home . client) . 75)
                                      ((office . client) . 45))
        org-foresight-travel-default 45)

  ;; The commute is work.  Whether the company counts it as paid time is beside
  ;; the point: an hour spent getting to a meeting is an hour of my life spent
  ;; on that meeting, so it comes out of the working day rather than out of
  ;; nowhere.  This is what makes an office meeting cost what it really costs.
  (setq org-foresight-private-categories '("family" "personal"))

  ;; Occupying time and demanding all of it are different things.  The child's
  ;; club calendar is a fact about the household, not an hour of mine: it says
  ;; when the house is empty, and takes nothing.  A meeting I only have to
  ;; hear still costs the hour but will share it with a commute, so that is
  ;; marked per entry with :ATTENTION: background rather than by category.
  (setq org-foresight-informational-categories '("club")
        org-foresight-background-categories nil)

  ;; Held back for work that has not arrived yet.  `M-x org-foresight-learn-surge'
  ;; replaces this with the measured median once there is ActivityWatch history;
  ;; until then, and on any machine without it, this is the reserve.
  (setq org-foresight-surge-default "1:00"
        org-foresight-surge-window  20)

  ;; The plain `a' agenda gets the daily tables; the "b" and "d" boards bind
  ;; this to nil and "r" binds it to `review' in their general settings.
  (setq org-foresight-report-style 'daily)

  ;; Both keywords mean "out with someone else", so a SCHEDULED date on them is
  ;; a check-in rather than a start date.  DELEG is a done-type keyword here,
  ;; which keeps it off the daily agenda -- exactly why it needs a signal of
  ;; its own, or handed-off work goes quiet unnoticed.
  (setq org-foresight-followup-keywords '("WAIT" "DELEG"))

  ;; Meetings imported from Outlook carry work either side of themselves that
  ;; nothing else budgets for.  The child's club calendar does not, so only
  ;; this category is treated as implying preparation.
  (setq org-foresight-meeting-categories '("outlook")
        org-foresight-meeting-prep   "0:30"
        org-foresight-meeting-follow "0:15")

  ;; Generated tasks follow the same rule as captured ones: a task is a subject
  ;; and subjects live in the journal date tree.
  (setq org-foresight-task-file (expand-file-name "journal.org" org-directory)
        org-foresight-task-datetree t
        org-foresight-task-todo "NEXT")

  ;; ONGO means started but unfinished.  Several at once is the state worth
  ;; catching: each addition costs the switch back into the others, so the day
  ;; slows down while every individual item still looks reasonable.
  (setq org-foresight-wip-keywords '("ONGO")
        org-foresight-wip-limit 2)

  ;; `Undecided' stays off.  Measured against this journal it matched 17% of
  ;; all headings -- diary entries and date-tree scaffolding, not problems --
  ;; and a board mostly listing non-problems stops being read.
  (setq org-foresight-undecided-enabled nil)

  ;; Estimates are corrected by what past work of the same category actually
  ;; took (M-x org-foresight-learn-bias).  The multiplier is always shown, so
  ;; a shrinking day reads as "my estimates are optimistic" rather than as the
  ;; tool being pessimistic.
  (setq org-foresight-bias-enabled t)

  (my/plan-apply-face-palette)

  ;; Placement belongs where the day is being looked at, so it lives on the
  ;; agenda map rather than taking a global key of its own.
  (my/define-key
   (:map org-agenda-mode-map
         :key
         "P" #'org-foresight-plan-fill))

  (add-hook 'org-agenda-finalize-hook #'org-foresight-report-render t))

;; ---- the bar's colours ---------------------------------------------------
;; The package tells the bar's segments apart by inheriting from font-lock
;; faces, which sets four hues side by side and makes a forty-column band
;; shout louder than the number it illustrates.  Two rules replace that.
;;
;; Grey says "work already claimed", and only three steps of it -- booked,
;; travel, promised -- two apart each time so the eye can actually resolve
;; them.  Everything else on the board earns a shape or a hue instead, which
;; is what frees the ramp up to be that coarse.
;;
;; Booked tops out at mono6, the badge's own grey: nothing inside a section
;; should be louder than the badge announcing it.
;;
;; Colour is kept for what is not claimed work, because that is where the
;; decisions are.  Blue is room -- spare, and the free gaps it is made of --
;; the number the whole board exists to grow.  Green is life, neither work nor
;; room, and must not be mistaken for either.  Surge takes no colour here at
;; all: the package draws it as an outline in the overrun's own yellow, which
;; is the truth about it -- the reserve is the last thing between the day and
;; an overrun, so spending it is being over without having said so.

(defvar my/plan-face-palette
  '((org-foresight-report-booked   . mono6)
    (org-foresight-report-travel   . mono4)
    (org-foresight-report-promised . mono2)
    (org-foresight-report-grey     . mono2)
    (org-foresight-report-spare    . blue)
    (org-foresight-report-private  . green))
  "Foresight faces, and the gensho palette key each takes its colour from.
The greys are listed in the order the bar draws them, so that stretch of it
stays monotonic; the hues stand outside the ramp deliberately.

`grey' shares promised's step rather than taking a fourth.  It draws the
alongside rows and the rare unclaimed band, which sit outside the accounting
and never beside a bar segment, so a step of their own would buy nothing.
Saying it is still necessary: the default is `font-lock-comment-face', which
gensho puts at mono5, and leaving it there would make somebody else's fixture
the same grey as a journey.  It is not green -- unclaimed waking hours are
not a private commitment, and the whole model turns on keeping those apart.")

(defun my/plan-apply-face-palette (&rest _)
  "Colour the foresight bar and grid gutter from the gensho palette.
Does nothing where gensho is absent, leaving the package's own faces alone.
Runs again on every theme change, since the palette differs per variant."
  (when (and (fboundp 'gensho-palette)
             (facep 'org-foresight-report-booked))
    (let ((palette (gensho-palette)))
      (pcase-dolist (`(,face . ,key) my/plan-face-palette)
        (set-face-attribute face nil :foreground (alist-get key palette))))))

(add-hook 'enable-theme-functions #'my/plan-apply-face-palette)

;; ---- the "p" board -------------------------------------------------------
;; The morning counterpart to the "r" review: what is arriving, rather than
;; what has been done.
;;
;; Registered by advice rather than by `add-to-list' in the `:config' above,
;; because `use-package org-agenda' *sets* `org-agenda-custom-commands' in its
;; own `:config', and that config is registered on `org-agenda' only once evil
;; loads -- i.e. after this file's, so it runs later and would wipe the entry
;; out again.  Attaching to the dispatcher instead makes the registration
;; independent of load order, and keeps the feature removable with this file
;; (cf. the same approach in my-app-calendar.el).

(defvar my/plan-agenda-command
  '("p" "Plan · 今日の設計"
    ((agenda "" ((org-agenda-span 1)
                 (org-agenda-start-day "+0d")
                 (org-agenda-start-on-weekday nil))))
    ((org-foresight-report-style 'plan)))
  "The plan view's entry for `org-agenda-custom-commands'.

An ordinary agenda block, because the day being rearranged has to be the real
day: org-foresight adds journeys, gaps and the edges of the working span to
it, and everything else the agenda knows -- logs, habits, tags, blocked
dimming -- comes along for free.  What the `plan' style adds underneath is
the two questions the day cannot answer from inside itself: when this could
be taken on instead, and what has not been asked about at all.")

(defun my/plan-register-agenda-command (&rest _)
  "Ensure the plan board is present in `org-agenda-custom-commands'."
  (when (boundp 'org-agenda-custom-commands)
    (unless (assoc "p" org-agenda-custom-commands)
      (setq org-agenda-custom-commands
            (append org-agenda-custom-commands
                    (list my/plan-agenda-command))))))

(with-eval-after-load 'org-agenda
  (advice-add 'org-agenda :before #'my/plan-register-agenda-command))

(provide 'my-app-plan)

;;; my-app-plan.el ends here
