;;; my-app-org.el --- Org itself -*- lexical-binding: t; -*- ;;; Commentary:
;; Org as a way of recording: files, capture, refile, clocking, roam, export.
;;
;; What today looks like is the other question and lives in
;; my-app-org-agenda.el -- the agenda, org-foresight, the timeline, the blocks,
;; ActivityWatch.  The line between the two files is that one: this file is
;; about how a thing gets written down and found again, that one is about a
;; day.
;;
;; Recording model.  Two structures carry everything:
;;
;;   headline -- a *subject*.  Addressable: it takes an ID, a TODO state, tags,
;;               properties, SCHEDULED/DEADLINE, a LOGBOOK; it can be refiled,
;;               archived, linked to, surfaced in the agenda, indexed as a node.
;;   note     -- *what happened to* a subject: a plain list item carrying an
;;               inactive timestamp (`org-add-note', C-c C-z).  Not addressable,
;;               no state, never in the agenda.
;;
;; The test is one question: will this be looked up, moved, or given a state on
;; its own later?  Yes -> headline.  "More about something that already exists"
;; -> note.  Notes are cheap and headlines are expensive, so high-frequency
;; recording goes through notes; only things needing an identity become
;; headlines.
;;
;; Time is recorded at two levels of commitment.  `journal.org' feeds
;; `org-agenda-files' (see `my/find-todo-files'), clock reports and the weekly
;; review, so writing there carries weight -- that is by design.  The daily
;; notes under `org-roam-dailies-directory' carry none: fleeting thoughts,
;; feelings, questions, fragments.  Whatever grows there gets promoted, either
;; to a node (`org-roam-refile') or to a task in journal.org (`org-refile').
;;
;; What to record, where it goes, how to get there:
;;
;;   a task (has a state, a date)       journal.org datetree   C-c c a / i / s / e
;;   an entry under today               journal.org datetree   C-c c j
;;   a thought, feeling, fragment       today's daily note     C-c z n
;;   a subject inside today             today's daily note     C-c z d
;;   more about the clocked task        that task              C-c c n
;;   more about the entry at point      that entry             C-c C-z
;;   more about a distant node          that node's Log        C-c c g
;;   more about a person                that person's Log      C-c c o
;;   a new subject                      a new node             C-c f / C-c n c
;;   a choice point (ACT)               the value it is about  C-c c v
;;
;; Records with a shape.  Some of what is recorded here is not prose but a
;; *kind of thing* -- a person, a mood, a choice point -- and those are written
;; to a form rather than freely.  A shaped record is three decisions, and they
;; are worth making together:
;;
;;   where it goes    a named place, so it is never "somewhere in the notes"
;;   what it holds    fixed fields, asked in order by the capture
;;   how it is read   a dynamic block, a column view, or the eye once a month
;;
;; Free text still belongs in them -- inside the shape, in the fields meant for
;; it.  The shape is what makes a hundred of them comparable later; the prose is
;; what makes any one of them worth keeping.
;;
;; They live here, in this file, and none of it is a package.  Org already has
;; the parts (capture prompts, property drawers, `#+COLUMNS:', date trees), and
;; there is a structural reason too: `org-capture-templates' is one `setq', so
;; a template kept in another file is a template that gets thrown away when
;; this one runs.  That is not hypothetical -- it happened to the "i" template
;; and went unnoticed for a while.  When the *reading* side is written a third
;; time, that is the thing worth extracting.
;;
;;; Code:

;; Shared by the capture templates below, the org-roam person template and
;; `my/org-roam-node-log-target'.  Defined at top level so every use-package
;; block can see them regardless of load order.

;; Every node accumulates its notes under one heading of this name -- daily
;; notes, person nodes and subject nodes alike -- so there is a single word to
;; remember.  It is also a structural requirement: an `item' capture whose
;; target is not a heading searches the *whole buffer* for a list to join (see
;; `org-capture-place-item'), which would let a note land under an unrelated
;; heading.  A container heading makes the destination deterministic.
(defconst my/org-log-heading "Log")

;; Same shape `org-store-log-note' produces, so capture-written notes and
;; C-c C-z notes are indistinguishable in the file.  The bare prefix is for
;; templates that place the cursor elsewhere and leave the note to be typed.
(defconst my/org-note-prefix "- Note taken on %U \\\\\n  ")
(defconst my/org-note-template (concat my/org-note-prefix "%?"))

;; Notes filed onto a node from somewhere else: keep the link back to wherever
;; the thought occurred, and pull in the region if one is active.  `%i' repeats
;; the characters leading up to it on every line (see `org-capture-fill-template'),
;; so a multi-line region stays indented as item continuation.
(defconst my/org-node-note-template (concat my/org-note-prefix "%a\n  %i%?"))

;; The daily file is already the date, by name and by title; a date heading
;; inside it would only repeat that.
(defconst my/org-daily-head "#+title: %<%Y-%m-%d>\n")

(use-package org
  :straight org-contrib
  ;; Loaded at startup, deliberately.  Org is the first thing reached for here,
  ;; and a pause on reaching for it is worse than a slower start.  It can afford
  ;; to be: what made Org expensive was never Org but the ecosystem that used to
  ;; load beside it, and that now waits to be asked for.
  :after evil
  :init
  (setq system-time-locale "C")
  ;; Set `org-directory' here in `:init' (not `:config') so it is bound before
  ;; org.el loads rather than after.  The agenda-file discovery below depends on
  ;; it and on nothing else in Org, and runs from `emacs-startup-hook'.
  ;; Presetting this defcustom is safe -- org.el will not clobber an
  ;; already-bound value.
  (setq org-directory (file-name-as-directory "~/Documents/memex/"))

  ;; Agenda-file discovery also lives in `:init' so it is available at startup.
  ;; These helpers depend only on `org-directory', not on org itself.
  (defvar my/org-agenda-exclude-regexps
    '("/archive/" "-fixture\\.org\\'")
    "Regexps that disqualify a file from `org-agenda-files'.
Matched against each candidate's absolute path; any match drops the file.

Archived trees and test fixtures carry real TODO keywords, so without this
they are discovered by `my/find-todo-files' and then silently skew every
agenda, clock report and scheduling decision built on `org-agenda-files'.
The surrounding slashes in \"/archive/\" anchor it to a whole directory
component, so a file merely named `archive.org' is still included.")

  (defun my/org-agenda-file-excluded-p (file)
    "Non-nil when FILE matches any regexp in `my/org-agenda-exclude-regexps'."
    (let ((path (expand-file-name file)))
      (seq-some (lambda (re) (string-match-p re path))
                my/org-agenda-exclude-regexps)))

  (defun my/find-org-recursive (&rest dirs)
    "Recursively find all .org files in DIRS."
    (seq-mapcat (lambda (dir)
                  (directory-files-recursively dir "\\.org\\'"))
                dirs))

  (defun my/find-todo-files (dir)
    "List .org files under DIR with an open (NEXT/ONGO/WAIT) heading via rg.
`.org_archive' files are excluded by rg explicitly (the `*.org' rg type
otherwise matches them).  Falls back to listing every .org file recursively
when rg is unavailable.

Both paths end in the same `my/org-agenda-file-excluded-p' filter, so the
agenda file set does not depend on whether rg is installed -- rg's own
`--glob' stays a cheap pre-filter, not a second source of truth."
    (let* ((abs (expand-file-name dir))
           (files
            (if (executable-find "rg")
                (with-temp-buffer
                  (when (zerop (apply #'call-process
                                      "rg" nil t nil
                                      (append
                                       '("--type-add" "org:*.org"
                                         "-torg"
                                         "--glob" "!*.org_archive"
                                         "-l"
                                         "--no-heading" "--no-config"
                                         "^\\*+ (NEXT|ONGO|WAIT)\\b")
                                       (list abs))))
                    (split-string (buffer-string) "\n" t)))
              ;; fallback if rg is not available
              (my/find-org-recursive abs))))
      (seq-remove #'my/org-agenda-file-excluded-p files)))

  (defun my/org-agenda-files-refresh ()
    "Rebuild `org-agenda-files' from open-task .org files under the main dir."
    (interactive)
    (setq org-agenda-files
          (and (file-directory-p org-directory)
               (my/find-todo-files org-directory))))

  ;; Populate at startup after init finishes (PATH is set by then, org may
  ;; still be unloaded).  Presetting `org-agenda-files' is safe: org.el's
  ;; defcustom will not clobber an already-bound value.
  (add-hook 'emacs-startup-hook #'my/org-agenda-files-refresh)
  :config
  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "t" #'toggle-truncate-lines
         "l" #'org-store-link
         "c" #'org-capture
         "a" #'org-agenda
         "p" #'org-cliplink
         "]" #'my/consult-org-headings-all)
   (:map global-map
         :prefix "C-c C-x"
         :key
         "C-i" #'org-clock-in
         "C-o" #'org-clock-out
         "C-j" #'org-clock-goto
         "C-|" #'org-clock-goto
         ;; Clocking into something already clocked once is Org's own
         ;; `C-u C-c C-x C-i', which offers the recent ones to choose from.
         "C-z" #'org-resolve-clocks
         "C-e" #'org-clock-modify-effort-estimate)
   (:map org-mode-map
         :prefix "C-c"
         :key
         "w"       #'org-refile-goto-last-stored
         "C-v C-b" #'org-dblocks-babel-execute-buffer
         "C-."     #'org-todo)
   (:map org-mode-map
         :key
         "C-RET" #'org-insert-heading-respect-content)
   (:map org-mode-map
         :state normal
         :key
         "RET" #'org-return
         "C-i" #'org-cycle
         "zs"  #'org-narrow-to-subtree)
   (:map org-mode-map
         :state insert
         :key
         "RET" (lambda () (interactive) (org-return t)))
   (:map org-mode-map
         :state motion normal visual insert
         :key
         "M-h" #'org-metaleft
         "M-j" #'org-metadown
         "M-k" #'org-metaup
         "M-l" #'org-metaright
         "M-H" #'org-shiftmetaleft
         "M-L" #'org-shiftmetaright))

  (setq org-startup-folded nil
        org-startup-indented t
        org-startup-truncated nil
        org-startup-with-inline-images t
        org-image-actual-width 800
        org-odd-levels-only nil
        org-indent-indentation-per-level 1
        org-hide-leading-stars nil
        org-hide-block-startup t
        ;; Disable disk PERSISTENCE of the org-element cache.  Our org files live
        ;; under ~/Documents/memex, a junction to ~/some-cloud.../memex, so the
        ;; same file is reachable via two paths; org-persist then stores/reloads
        ;; TWO element-cache entries for it (one per path) and a stale one can
        ;; survive restarts -- making `org-agenda-redo' show old TODO state or
        ;; miss newly added tasks until `org-element-cache-reset'.  Keep the
        ;; in-memory cache (`org-element-use-cache' t) for speed; only stop
        ;; persisting it, so every session starts from a clean, correctly
        ;; invalidated cache.
        org-element-cache-persistent nil
        org-id-link-to-org-use-id t
        org-attach-store-link-p nil
        org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto))
        org-indirect-buffer-display 'current-window
        org-cycle-include-plain-lists 'integrate
        org-M-RET-may-split-line '((default . nil))
        org-todo-keyword-faces '(("ONGO" . my/org-ongo)
                                 ("WAIT" . my/org-wait)
                                 ("DONE" . org-done)
                                 ("DELEG" . org-done)
                                 ("CANCEL" . org-done)))

  (defun my/org-return-in-evil-normal (orig-fn &rest args)
    "Prevent newline when pressing RET in evil normal state."
    (if (not (eq evil-state 'insert))
        (cl-letf (((symbol-function 'newline-and-indent) #'evil-ret-and-indent)
                  ((symbol-function 'newline) #'evil-ret))
          (apply orig-fn args))
      (apply orig-fn args)))
  
  (advice-add 'org-return :around #'my/org-return-in-evil-normal)

  ;; Fallback target for capture templates whose file part is the empty string
  ;; (see `org-capture-expand-file').  Every template here names its own file,
  ;; so this is only a sane default; project.org is a flat (non-datetree) file.
  (setq org-default-notes-file (concat org-directory "project.org"))

  (defvar my/org-journal-file (concat org-directory "journal.org"))

  (setq org-return-follows-link t)
  
  (defvar my/org-file-app-rules
    `(("\\.pdf\\'"  . 'default)
      ("\\.docx\\'" . 'default)
      ("\\.xlsx\\'" . 'default)
      ("\\.pptx\\'" . 'default)
      ("\\.url\\'"  . 'default)
      ("\\.lnk\\'"  . 'default)
      ("\\.png\\'"  . ,(if (display-graphic-p) "sxiv %s" nil))
      (directory    . ,(when my/wsl-p "wsl-open %s"))
      (default      . ,(when my/wsl-p "wsl-open %s"))))

  (dolist (entry my/org-file-app-rules)
    (when (cdr entry)
      (add-to-list 'org-file-apps
                   (cons (car entry)
                         (if (symbolp (cdr entry))
                             (symbol-value (cdr entry))
                           (cdr entry))))))

  ;; Refile Settings.  A *static* target list (built from these files), so
  ;; standard `org-refile' narrowing works with orderless+migemo -- unlike
  ;; `org-ql-refile', which pre-filters via org-ql (no migemo) and defeats it.
  ;; maxlevel 6 covers the deepest current heading (project.org/memex.org at 5)
  ;; plus one level of headroom for sub-entries under journal datetree tasks
  ;; (year/month/day = 1-3, captured entry = 4).
  (setq org-refile-targets
        `((nil :maxlevel . 6)
          (org-agenda-files :maxlevel . 6)))
  
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes nil)

  ;; Archive behavior
  (setq org-archive-location "%s_archive::"
        org-cycle-open-archived-trees nil
        org-sparse-tree-open-archived-trees nil
        org-columns-skip-archived-trees t)
  
  ;; Timestamp
  (defun my/org-time-stamp-in-evil-insert (orig-fn &rest args)
    "Insert org timestamp with proper evil state handling."
    (if (memq evil-state '(motion normal visual))
        (let ((orig-state evil-state))
          (evil-open-below 1)
          (apply orig-fn args)
          (evil-change-state orig-state))
      (apply orig-fn args)))
  
  (advice-add 'org-time-stamp :around #'my/org-time-stamp-in-evil-insert)
  
  ;; Log
  ;;
  ;; One axis decides the state: *whose move is it, and does it come back to
  ;; me?*  Not "how far along is it" -- progress is what the clock and the
  ;; effort are for.
  ;;
  ;;   NEXT / ONGO  mine                                   todo
  ;;   WAIT         theirs, and it comes back to me        todo
  ;;   DELEG        theirs, and it does not -- I am out    done
  ;;   DONE/CANCEL  over                                   done
  ;;
  ;; So DELEG does not mean "delegated", it means *handed over for good*: the
  ;; case where the work left my accountability with it.  Work given to a
  ;; report is WAIT, because the result returns and I am still answerable for
  ;; it -- and it stays under its own name, with `:PEOPLE:' saying who has it
  ;; and SCHEDULED saying when to ask.  A separate "receive X's report" task is
  ;; the same fact written twice.
  ;;
  ;; The sequence below already reads that way: a WAIT that turns out never to
  ;; come back is closed with DELEG; one that does is closed with DONE.
  ;;
  ;; DELEG stays a done-type keyword for a reason beyond bookkeeping: the clock
  ;; on such an entry measures the *handover* -- the mails, the briefing --
  ;; while its EFFORT was an estimate of the whole job.  Comparing the two
  ;; would teach org-foresight that work takes a fraction of its estimate, so
  ;; `org-foresight-bias-abandoned-keywords' excludes it.
  (setq org-todo-keywords
        ;; `SDAY' is someday: put down on purpose, not forgotten.  It logs
        ;; its time like the other live states, because how long something has
        ;; been put down is the question the weekly review asks of it.
        ;; `org-foresight-parked-keywords' is what makes it cost nothing --
        ;; without that it would be work like any other, taking its estimate
        ;; out of every day it is not being done on.
        '((sequence "NEXT(n!)" "ONGO(o!)" "SDAY(s!)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "WAIT(w@)" "|" "DELEG(e@)")))
  
  (setq org-log-done 'note
        org-treat-insert-todo-heading-as-state-change t
        org-log-state-notes-insert-after-drawers t)

  ;; Read an entry top to bottom in the order things happened: notes appended
  ;; by C-c C-z land below the previous ones rather than above, matching how
  ;; the `item' capture templates append.  Applies to LOGBOOK state and clock
  ;; lines too.
  (setq org-log-states-order-reversed nil)
  
  (setq org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-refile     'time
        org-closed-keep-when-no-todo nil)
  
  (defun my/org-add-log-setup-into-drawer (&optional purpose _state _prev-state _how _extra)
    (setq org-log-into-drawer
          (memq purpose '(state deldeadline delschedule redeadline reschedule refile))))
  
  (advice-add 'org-add-log-setup :before #'my/org-add-log-setup-into-drawer)

  ;; Cookie
  (setq org-provide-todo-statistics t
        org-hierarchical-todo-statistics t
        org-track-ordered-property-with-tag t)

  ;; Tag 
  (setq org-tag-persistent-alist
        '((:startgroup . nil) ("self" . ?s) ("delegate" . ?d) (:endgroup . nil)
          (:startgroup . nil) ("mark" . ?m) (:endgroup . nil)))
  
  (setq org-tags-column -76
        org-agenda-tags-column -79
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("mark")
        org-agenda-use-tag-inheritance '(todo search timeline agenda)
        org-agenda-show-inherited-tags t
        org-tags-sort-function #'string<)
  
  (defun my/org-inherited-no-file-tags ()
    "Remove file-level tags from inherited tag list."
    (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
          (ltags (org-entry-get nil "TAGS")))
      (mapc (lambda (tag)
              (setq tags
                    (replace-regexp-in-string (concat tag ":") "" tags)))
            (append org-file-tags (when ltags (split-string ltags ":" t))))
      (unless (string= ":" tags) tags)))
  
  (defun my/org-archive-subtree-low-level (f &rest args)
    "Archive to outline node instead of flat list."
    (let ((tags (my/org-inherited-no-file-tags))
          (org-archive-location
           (if (> (org-outline-level) 1)
               (concat (car (split-string org-archive-location "::"))
                       "::* " (car (org-get-outline-path)))
             org-archive-location)))
      (apply f args)
      (with-current-buffer (find-file-noselect (org-extract-archive-file))
        (save-excursion
          (while (org-up-heading-safe))
          (org-set-tags-to tags)))))
  
  (advice-add 'org-archive-subtree :around #'my/org-archive-subtree-low-level)

  ;; Property
  ;;
  ;; `PLACE' is read from a day's own heading and, rarely, from a single entry
  ;; whose LOCATION does not say where the body has to be.  Both are typed
  ;; with `C-c C-x p', and a place is a closed set of names rather than free
  ;; text -- a misspelt one is not an error anywhere, it just quietly stops
  ;; producing a commute.  Global rather than a `#+PROPERTY:' line, so it also
  ;; holds in calendar.org, which org-calsync rewrites.  The names themselves
  ;; are `org-foresight-places' in my-app-org-agenda.el; kept here because
  ;; this is the one setq that owns allowed values, and splitting it is how
  ;; the later half gets silently dropped.
  (setq org-global-properties
        '(("EFFORT_ALL"       . "0:00 0:02 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00")
          ("PLACE_ALL"        . "home gym office client")
          ("STYLE_ALL"        . "habit personal")
          ("COOKIE_DATA_ALL"  . "recursive")))
  
  (setq org-use-property-inheritance nil)

  ;; Column
  (setq org-columns-default-format
        " %1TODO %35ITEM %5CATEGORY %8ALLTAGS %1PRIORITY %5EFFORT{:} %5CLOCKSUM_T{:}")

  ;; Clock 
  (setq org-clock-out-when-done t
        org-clock-in-switch-to-state (lambda (state) (if state "ONGO"))
        org-clock-out-switch-to-state nil
        org-clock-history-length 10
        org-clock-goto-may-find-recent-task t
        org-clock-in-resume nil
        ;; The running clock, and not the history.  `t' saves both, and on
        ;; the next start Org visits the file of the running clock *and every
        ;; file the history mentions* -- measured here at 0.87s and three
        ;; buffers opened, on a local disk; the same over a synced drive is
        ;; the several seconds of `org-clock-persist' in the echo area at
        ;; startup.  `clock' keeps what the setting is for -- carrying on with
        ;; what you were doing -- and opens one file to do it.  The history
        ;; still builds up within a session; it is only not carried across.
        org-clock-persist 'clock
        org-clock-persist-query-save nil
        org-clock-idle-time 60
        ;; `org-clock-auto-clock-resolution' is not set here: it belongs to
        ;; `my/org-clock-obeys-the-row' below, which is where the reason for
        ;; its value is written down and where anything setting it back is
        ;; noticed.
        org-clock-continuously nil
        org-clock-clocked-in-display 'both
        org-clock-string-limit 0)
  
  (unless (eq org-clock-persist nil)
    (org-clock-persistence-insinuate))
  
  (setq org-clock-clocktable-default-properties
        '(:scope agenda :maxlevel 5 :lang "en" :block thisweek :step day)
        org-duration-format '((special . h:mm))) ;; Avoid incorrect sort: 1d 0:10 < 0:20
  
  ;; Clock display in tab-bar (coloring + left-edge slant live in my-ui-face).
  (with-eval-after-load 'tab-bar
    (unless (memq 'tab-bar-format-global tab-bar-format)
      (setq tab-bar-format
            (append tab-bar-format
                    '(tab-bar-format-align-right
                      tab-bar-format-global)))))

  ;; Clocking in acts on what was aimed at
  ;;
  ;; A defcustom rather than a comment beside a value, because the value looks
  ;; harmless and the reason it is not took three days to find.  A name can be
  ;; searched for, a docstring is read by `C-h v', and the watcher below says
  ;; something if the value it protects is ever set back.

  (defcustom my/org-clock-obeys-the-row t
    "When non-nil, clocking in acts on what was aimed at and nothing else.

Org offers to tidy up an unclosed clock at the moment you clock in, which is
`org-clock-auto-clock-resolution\=' -- and the tidying is not passive.
`org-clock-in\=' calls `org-resolve-clocks\=' before it looks at the entry
at all (org-clock.el:1410), and `org-clock-resolve\=' *jumps to* the entry
holding the unclosed clock (1163) and reads one character.  Its own prompt
says what that costs: \"using uppercase makes your final state to be CLOCKED
OUT\" -- so every lowercase answer leaves the clock running on the old entry.

The result is a keystroke that starts the clock on something you were not
looking at, from an agenda row that is perfectly correct.  It cannot be seen
by comparing the row with its marker, because nothing about the row is wrong;
and visiting the same row goes to the right place, because visiting never
passes through `org-clock-in\='.  That asymmetry is the whole tell.

Unclosed clocks do not go away, they are dealt with when they are the
subject: `Z\=' in the agenda is `org-resolve-clocks\=', `v c\=' audits the
day, and the idle check is a separate mechanism driven by
`org-clock-idle-time\=' (`org-resolve-clocks-if-idle\=', org-clock.el:1351)
which this does not touch."
    :type 'boolean
    :group 'org-clock
    :set (lambda (symbol value)
           (set-default symbol value)
           (when value (setq org-clock-auto-clock-resolution nil))))

  (defun my/org-clock--refuse-resolution (_symbol newval operation _where)
    "Refuse to let `org-clock-auto-clock-resolution\=' be turned back on.

A watcher that signals prevents the assignment, and preventing it is the
point.  A warning about this one arrives after the trap is armed again, in a
buffer nobody is looking at; the refusal arrives instead of the trap, and
names the way past itself.  Turning `my/org-clock-obeys-the-row\=' off is
that way, and reading why is what its docstring is for.

Only a plain set is refused.  Org binds the variable to nil around its own
resume (org-clock.el:3345), and a binding is not somebody changing their
mind."
    (when (and my/org-clock-obeys-the-row newval (eq operation 'set))
      (user-error
       (concat "Refusing to set `org-clock-auto-clock-resolution' to %S: it "
               "lets clocking in start the clock on an entry you were not "
               "looking at.  Set `my/org-clock-obeys-the-row' to nil first, "
               "and read its docstring for what that gives up")
       newval)))

  (add-variable-watcher 'org-clock-auto-clock-resolution
                        #'my/org-clock--refuse-resolution)

  ;; State and Clock
  (defvar my/org-inhibit-auto-clock-in nil
    "When non-nil, `my/org-clock-in-if-ongo' does not auto-clock-in.
Bound around `org-clock-in' (whose switch-to-state already clocks in) and
around the recursive parent-state promotion, so a single clock-in cannot
cascade into spurious clock-ins on ancestor tasks.")

  (defvar my/org-todo-cycle-commands
    '(org-todo org-agenda-todo org-shiftright org-shiftleft
               org-agenda-todo-nextset org-agenda-todo-previousset)
    "Commands that count as the user manually changing a TODO state.
Only these let `my/org-clock-in-if-ongo' auto-clock-in; extend as needed.")

  (defun my/org-parent-ongo-if-needed ()
    "If the current task is ONGO/WAIT/DONE/DELEG, update parent TODO to ONGO if it's NEXT."
    (when (member org-state '("ONGO" "WAIT" "DONE" "DELEG"))
      (save-excursion
        (when (org-up-heading-safe)
          (when (member (org-entry-get nil "TODO") '("NEXT"))
            ;; Promotion is a side effect, not a user clock request: never let
            ;; it auto-clock the ancestor.  The state change still re-fires this
            ;; hook, so promotion keeps cascading up the NEXT chain.
            (let ((my/org-inhibit-auto-clock-in t))
              (org-todo "ONGO")))))))

  (defun my/org-clock-in-if-ongo ()
    "Clock in when the user manually switches a task to ONGO.
Skipped during `org-clock-in' and parent promotion (guarded by
`my/org-inhibit-auto-clock-in'), and only for genuine user state-cycling
commands (`my/org-todo-cycle-commands'), so cascaded/automated ONGO
transitions never spawn a clock-in on a different task."
    (when (and (not my/org-inhibit-auto-clock-in)
               (memq this-command my/org-todo-cycle-commands)
               (equal org-state "ONGO")
               (not (equal org-clock-current-task (org-entry-get (point) "ITEM"))))
      (org-clock-in)))
  
  (my/add-hook
   (:hook org-after-todo-state-change-hook
          :func (lambda ()
                  (my/org-parent-ongo-if-needed)
                  (my/org-clock-in-if-ongo)))
   (:hook org-clock-in-prepare-hook
          :func (lambda ()
                  (unless (org-entry-get (point) "EFFORT")
                    (org-set-effort)))))
  
  (defun my/org-clock-in-continuously-reverse-by-prefix (f &optional select start-time)
    "Around advice on `org-clock-in': inhibit `my/org-clock-in-if-ongo' for the
whole call (`org-clock-in' already inserts the clock, and its
`org-clock-in-switch-to-state' fires `org-after-todo-state-change-hook',
which must not re-enter clock-in), and toggle `org-clock-continuously' when
called with C-u (prefix 64)."
    (let ((my/org-inhibit-auto-clock-in t))
      (if (equal select '(64))
          (let ((org-clock-continuously (not org-clock-continuously)))
            (apply f nil start-time))
        (apply f select start-time))))
  (advice-add 'org-clock-in :around #'my/org-clock-in-continuously-reverse-by-prefix)
  
  ;; Clock heading
  (defun my/org-clock-heading-get ()
    (org-entry-get org-clock-marker "ITEM"))
  
  (defun my/org-clock-heading-update ()
    (setq org-clock-heading
          (or (and org-clock-heading-function
                   (funcall org-clock-heading-function))
              (let ((text (nth 4 (org-heading-components))))
                (if text
                    (replace-regexp-in-string
                     "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" text)
                  "???")))))
  
  ;; Notification
  (defun my/org-show-notification-message (msg)
    (message "%s" msg))

  (setq org-show-notification-handler #'my/org-show-notification-message)
  
  ;; Capture support with Evil
  (defun my/org-capture-evil-setup ()
    (evil-append 1)
    (my/evil-ex-define-cmd-local "q[uit]" #'org-capture-kill)
    (my/evil-ex-define-cmd-local "wq" #'org-capture-finalize))
  
  (defun my/org-capture-update-clock-heading ()
    (when (marker-position org-clock-marker)
      (let ((org-clock-heading-function #'my/org-clock-heading-get))
        (my/org-clock-heading-update)
        (org-clock-update-mode-line))))

  (my/add-hook
   (:hook org-capture-mode-hook
          :func #'my/org-capture-evil-setup)
   (:hook org-capture-before-finalize-hook
          :func #'my/org-capture-update-clock-heading))

  ;; Two kinds of template live here, distinguished by capture type rather than
  ;; by key: `entry' ones create a subject (a task, a dated journal entry, a
  ;; clock repair), while `item' ones append a note to a subject that already
  ;; exists.  The `item' ones are the cheap, high-frequency half -- use them
  ;; freely.  C-c C-z (`org-add-note') is the same act performed in place; the
  ;; templates here exist for adding to a subject you are *not* looking at.
  (setq org-capture-templates
        `(("a" "add task" entry (file+datetree my/org-journal-file)
           "* NEXT %?\nSCHEDULED: %^t\n:LOGBOOK:\n- State \"NEXT\"       from              %U\n:END:")
          ;; The one capture org-foresight depends on.  `:SURGE:' marks work as
          ;; having arrived rather than been planned, and its value is when it
          ;; arrived -- which is what decides when it stops counting as
          ;; unplanned.  A date of its own on any later day means the work has
          ;; been taken in hand, and from then it is ordinary promised work.
          ;;
          ;; `:clock-resume' is the whole difference from "s": an interruption
          ;; is recorded and then you go back to what you were doing, where a
          ;; switch leaves you on the new thing.
          ("i" "interrupt task" entry (file+datetree my/org-journal-file)
           ,(concat "* ONGO %?\n"
                    ":PROPERTIES:\n:SURGE: %U\n:END:\n"
                    ":LOGBOOK:\n- State \"ONGO\"       from              %U\n:END:")
           :clock-in t :clock-resume t)
          ("s" "switch task" entry (file+datetree my/org-journal-file)
           ;; Capture writes the heading rather than calling `org-todo', so
           ;; no state change fires and nothing logs when the task began.
           ;; Written by hand, as the "a" and "e" templates do.
           "* ONGO %?\n:LOGBOOK:\n- State \"ONGO\"       from              %U\n:END:"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("j" "journal" entry (file+datetree my/org-journal-file)
           ,(concat "* %?\n" my/org-note-prefix)
           :jump-to-captured t)
          ;; Work handed to someone, captured as it is created.  WAIT rather
          ;; than DELEG: it is coming back, and I am still answerable for it
          ;; (see `org-todo-keywords').  SCHEDULED is when to ask, `:PEOPLE:'
          ;; is who has it, and the "d" custom command (the people board)
          ;; groups by that.  Work that is leaving for good has no template --
          ;; it is a state change on something that already exists, and the
          ;; hook `my/org-handover-on-state-change' asks what it needs to.
          ("e" "hand over (comes back)" entry (file+datetree my/org-journal-file)
           "* WAIT %?\nSCHEDULED: %^t\n:PROPERTIES:\n:PEOPLE: %^{Who has it}\n:END:\n:LOGBOOK:\n- State \"WAIT\"       from              %U\n:END:")
          ;; Note onto the task being clocked.  The `clock' target leaves
          ;; `:target-entry-p' at its default t, so the item joins the note list
          ;; in that entry's own body and never reaches into its children.
          ("n" "note on clocked task" item (clock)
           ,(concat my/org-note-prefix "Annotation %a\n  %?")
           :jump-to-captured t)
          ;; Note onto a node chosen by name.  "o" is the same template with the
          ;; candidate list narrowed to people -- the only difference is the
          ;; filter, so a note reads the same wherever it lands.  Anything that
          ;; later deserves to be looked up on its own (a 1on1 worth scheduling
          ;; follow-ups against) gets promoted to a headline then, not now.
          ("g" "note on a node" item (function my/org-roam-node-log-target)
           ,my/org-node-note-template)
          ("o" "note on a person" item (function my/org-roam-person-log-target)
           ,my/org-node-note-template)
          ;; ACT.  A choice point: what showed up, what it pulled me into, and
          ;; what the value asked for instead.  Only what can be enumerated is
          ;; prompted for -- the three sentences are labelled and left to be
          ;; typed, because a form that asks six questions in a row is a form
          ;; nobody fills in at the moment it is needed.
          ;;
          ;; It is filed as a child of the rung it is about, chosen by
          ;; completion over the ladder, so no property has to name the value:
          ;; the parent says it.  What used to be a fixed list of life domains
          ;; is now whatever has actually been declared, which means a value
          ;; cannot be practised until it has been written down.
          ;;
          ;; The two numbers are the ones ACT actually works on.  Struggle is
          ;; how hard I fought the feeling, not how strong it was: intensity is
          ;; deliberately not recorded, because tracking it invites wanting it
          ;; lower, which is the trap the whole practice is about.  And what
          ;; came of it is `towards' or `away' -- workability, the only test
          ;; ACT applies to an action.
          ("v" "ACT: choice point" entry (function org-convect-act-target)
           ,(concat "* %^{状況}\n"
                    ":PROPERTIES:\n"
                    ":CREATED:      %U\n"
                    ":ACT_STRUGGLE: %^{もがき度|0|1|2|3|4|5|6|7|8|9|10}\n"
                    ":ACT_MOVE:     %^{向かえたか|towards|partly|away}\n"
                    ":END:\n"
                    "- 釣られた思考・感情 :: %?\n"
                    "- 逸れた行動 :: \n"
                    "- 向かう行動 :: \n"))))

  ;; Handing work over, the other half of the "e" template above.  That one
  ;; captures work handed over as it is created; this one catches work you are
  ;; already looking at changing hands, and asks the same questions.  It
  ;; belongs to Org rather than to the agenda -- `org-after-todo-state-change-hook'
  ;; fires wherever the state is cycled -- and living beside the agenda meant it
  ;; did not exist until the agenda had been opened at least once.
  (defun my/org-roam-person-names ()
    "Titles of all `:person:'-tagged org-roam nodes.
Used as completion candidates when handing work over.  Free text is still
accepted at the prompt, so handing something to someone without a person node
also works."
    (when (require 'org-roam nil t)
      (seq-uniq
       (seq-keep (lambda (n)
                   ;; File-level nodes only: the `:person:' filetag is inherited
                   ;; by sub-headings, so without this the headings inside a
                   ;; person node (Log, Together, ...) leak in as names.
                   (and (= 0 (org-roam-node-level n))
                        (member "person" (org-roam-node-tags n))
                        (org-roam-node-title n)))
                 (org-roam-node-list)))))

  (defun my/org-handover-on-state-change ()
    "Record who has the work when it stops being mine, and when to ask.

Runs from `org-after-todo-state-change-hook\='.  Which questions get asked
follows from what the two states mean (see `org-todo-keywords\='):

  WAIT   who has it, *and* when to ask -- it is coming back to me
  DELEG  who has it.  Nothing else: it is not coming back, and a follow-up
         date on work I am no longer answerable for is a reminder to chase
         something that is not mine

`:PEOPLE:\=' rather than a property named for delegation: the same word names
whoever a piece of work involves, and the state says which way round it is --
WAIT and DELEG are with them, a NEXT with `:PEOPLE:\=' needs them.  One word
means the person\='s own note can show both in one block.  Written as a
multi-valued property, so a name with a space in it survives.

The prompt completes over `:person:\=' node titles, so names stay consistent
with those nodes and the per-person block keeps matching, but accepts free
text too -- work is not only handed to reports.  The follow-up SCHEDULE is set
with logging suppressed so it does not interleave with the pending
state-change note.  No-op when non-interactive, so scripted state changes (and
capture, which writes the state as text without a state change) are
unaffected."
    (when (and (member org-state '("WAIT" "DELEG")) (not noninteractive))
      (let ((who (completing-read
                  (if (equal org-state "WAIT") "Who has it: " "Handed to: ")
                  (my/org-roam-person-names)
                  nil nil nil nil
                  (car (org-entry-get-multivalued-property nil "PEOPLE"))))
            (followup (and (equal org-state "WAIT")
                           (org-read-date nil nil nil "When to ask"))))
        (unless (string-empty-p who)
          (org-entry-put-multivalued-property nil "PEOPLE" who))
        (when (and followup (not (string-empty-p followup)))
          (let ((org-log-reschedule nil))
            (org-schedule nil followup))))))

  (add-hook 'org-after-todo-state-change-hook #'my/org-handover-on-state-change)
  
  ;; Babel
  ;; Load R here.  agent-shell is registered later by `ob-agent-shell' in
  ;; my-app-agent.el (which appends rather than clobbering this list).
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)))
  
  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window)
  
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)
  
  (with-eval-after-load 'ob-R
    (setq org-babel-default-header-args:R
          (cons '(:session . "*R*")
                (assq-delete-all :session org-babel-default-header-args:R))))
  
  ;; Export
  (setq org-export-preserve-breaks nil)

  ;; Corpus heading nav: jump to any heading under `org-directory'.  The
  ;; heading layer, vs `consult-org-roam-search' which searches body text.
  (defun my/consult-org-headings-all (&optional archivep)
    "Consult all headings under `org-directory` (archives directory excluded).
With-current-buffer prefix argument INCLUDE-ARCHIVE (C-u), also include .org_archive files."
    (interactive "P")
    (unless (and org-directory (file-directory-p org-directory))
      (user-error "Please set a valid `org-directory`"))

    (let* ((ext (if archivep "\\.org\\(_archive\\)?$" "\\.org$"))
           (files (directory-files-recursively
                   org-directory
                   ext
                   nil
                   (lambda (d)
                     (not (string-match-p "/archive/" d))))))
      (consult-org-heading nil files)))

  ;; Shorten
  (defun my/org-shorten-string (s maxlength)
    "Shorten S to MAXLENGTH using string-width (multi-byte safe)."
    (if (<= (string-width s) maxlength)
        s
      (concat (truncate-string-to-width s (max (- maxlength 1) 0) 0) ".")))
  
  (advice-add 'org-shorten-string :override #'my/org-shorten-string))

(use-package org-colview
  :straight nil
  :after (evil org org-agenda)
  :config
  (my/define-key
   (:map org-columns-map :key "g" (lookup-key evil-motion-state-map "g"))))

(use-package org-habit
  ;; Waits on the agenda rather than on Org, because the agenda is the only
  ;; place a habit is drawn -- and because org-habit.el requires org-agenda at
  ;; its top level, so waiting on Org instead pulled the whole agenda into
  ;; every startup.
  :straight nil
  :after org-agenda
  :config
  (setq org-habit-graph-column 50
        org-habit-following-days 7
        org-habit-preceding-days 21
        org-habit-show-habits-only-for-today nil
        org-habit-show-all-today nil))

(use-package org-indent
  :straight nil
  :after org
  :diminish org-indent-mode)

;; Structure templates via C-c C-, (no "<" → no electric-pair "<>").
;; Keys: as = agent-shell, sr = R, srg = R graph.
(use-package org-tempo
  :straight nil
  :after org
  :config
  (setq tempo-interactive nil)
  (dolist (entry '(("as"  . "src agent-shell :results output drawer")
                   ("sr"  . "src R :results output")
                   ("srg" . "src R :results output graphics :file plot.png")))
    (add-to-list 'org-structure-template-alist entry)))

(use-package org-cliplink
  ;; Reached by its key, which is enough to load it.
  :defer t
  :init
  (my/define-key (:map org-mode-map :after org :key "C-c p" #'org-cliplink)))

(use-package org-attach
  :straight nil
  :after org
  :config
  ;; Absolute central ID-keyed store.  With the default relative "data/", each
  ;; note directory gets its own store, so moving a subtree between the root and
  ;; daily/ breaks `attachment:' links.  A single absolute store makes
  ;; attachments resolve by ID regardless of note location.
  (setq org-attach-id-dir
        (expand-file-name "data/" org-directory))

  ;; Expose screenshot capture as an entry in the `org-attach' dispatcher
  ;; (C-c C-a) rather than a standalone key.  `my/org-attach-screenshot' is a
  ;; forward reference here; the dispatcher only calls it when invoked.
  (add-to-list 'org-attach-commands
               '((?p) my/org-attach-screenshot
                 "Capture and attach a screenshot"))

  (defconst my/org-attach-screenshot-timestamp-format "%Y-%m-%dT%H-%M-%S-"
    "`format-time-string' spec used as the attached PNG filename prefix.")

  (defun my/org-attach-screenshot--capture (target)
    "Invoke the platform screenshot backend and write the image to TARGET."
    (pcase system-type
      ('windows-nt
       (let ((script (locate-file "save-clipboard-image" exec-path '(".ps1"))))
         (unless script
           (user-error "save-clipboard-image.ps1 not found on exec-path"))
         (let ((code (call-process "powershell" nil nil nil
                                   "-NoProfile" "-ExecutionPolicy" "Bypass"
                                   "-File" script "-OutputPath" target)))
           (unless (and (integerp code) (zerop code))
             (user-error "powershell exited with %s" code)))))
      ('darwin
       (unless (zerop (call-process "screencapture" nil nil nil "-i" target))
         (user-error "screencapture failed")))
      ('gnu/linux
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (unless (zerop (call-process "flameshot" nil t nil "gui" "--raw"))
           (user-error "flameshot failed"))
         (let ((coding-system-for-write 'no-conversion))
           (write-region (point-min) (point-max) target))))
      (_
       (user-error "No screenshot backend for system-type %s" system-type))))

  (defun my/org-attach-screenshot ()
    "Capture a screenshot and attach it to the current Org node.

Writes directly into the attach directory instead of routing through
`org-download-screenshot'. That path calls `org-attach-attach' with
method \\='none on a file already at its destination, which fires a
spurious overwrite prompt whose \"yes\" branch deletes the file
without replacing it."
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in an Org buffer"))
    ;; Remember the caller's position before `org-id-get-create' and
    ;; `org-attach-tag' move point onto the headline.  A marker follows any
    ;; ID drawer inserted below the heading, so the link still lands at the
    ;; character the cursor was on.
    (let ((origin (copy-marker (point))))
      (org-id-get-create)
      (let* ((attach-dir (org-attach-dir 'get-create))
             (basename (concat (format-time-string
                                my/org-attach-screenshot-timestamp-format)
                               (make-temp-name "")
                               ".png"))
             (target (expand-file-name basename attach-dir)))
        (my/org-attach-screenshot--capture target)
        (unless (file-exists-p target)
          (user-error "No image was saved (empty clipboard?)"))
        (org-attach-tag)
        (run-hook-with-args 'org-attach-after-change-hook attach-dir)
        (goto-char origin)
        (set-marker origin nil)
        ;; Insert to the right of the character under the (block) cursor, so
        ;; placing the cursor on the last glyph appends at end of line.
        (unless (eolp) (forward-char 1))
        (insert (format "[[attachment:%s]]" (org-link-escape basename)))
        (org-display-inline-images)))))

(use-package org-download
  :after org
  :init
  (setq org-download-timestamp "%Y-%m-%dT%H-%M-%S-")
  :config
  (setq org-download-method 'attach))

(use-package org-pomodoro
  ;; Deferred to the first timer.  The key that starts one is bound when Org
  ;; loads, so nothing about reaching it changes; what stops loading at startup
  ;; is `alert' and the notification backends behind it.
  :defer t
  :commands (org-pomodoro)
  :init
  (my/define-key
   (:map global-map org-mode-map
         :after org
         :key
         "C-c C-x C-p" #'org-pomodoro))
  :config
  (setq org-pomodoro-format "%s")

  (defun my/org-pomodoro-update-mode-line ()
    "Update mode-line with bracket styling for Pomodoro."
    (let ((s (cl-case org-pomodoro-state
               (:pomodoro org-pomodoro-format)
               (:overtime org-pomodoro-overtime-format)
               (:short-break org-pomodoro-short-break-format)
               (:long-break org-pomodoro-long-break-format))))
      (setq org-pomodoro-mode-line
            (when (and (org-pomodoro-active-p) (> (length s) 0))
              (list
               (propertize "[" 'face 'org-pomodoro-mode-line)
               (format s (org-pomodoro-format-seconds))
               (propertize "]" 'face 'org-pomodoro-mode-line))))
      (force-mode-line-update t)))
  
  (advice-add 'org-pomodoro-update-mode-line :override #'my/org-pomodoro-update-mode-line))

(use-package calendar
  :config
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (setq calendar-intermonth-text
        '(propertize
          (format "%02d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian
                     (list month (- day (1- calendar-week-start-day)) year)))))
          'font-lock-face 'my/calendar-iso-week-header)))

(use-package org-clock-split
  :after org)

(use-package ox-pandoc
  ;; An export backend is reachable from one place, the export dispatcher, and
  ;; that dispatcher lives in `ox' -- so `ox' loading is exactly the moment this
  ;; has to exist, and no earlier.  Under `:after org' it loaded with Org and
  ;; brought the whole export tree with it (ox, ox-org, ox-html, ox-latex,
  ;; ox-odt, ox-ascii), three seconds of every startup for a converter reached
  ;; a few times a year.
  :defer t
  :init (with-eval-after-load 'ox (require 'ox-pandoc)))

(use-package org-ql
  ;; A query language, entered through one of two commands.  Binding them is
  ;; all that has to happen at startup; straight's autoloads take it from
  ;; there, and they point at org-ql-search.el and org-ql-view.el, which is
  ;; where those commands actually live.
  :defer t
  :init
  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "q" #'org-ql-search
         "v" #'org-ql-view)
   (:map global-map
         :prefix "C-c n"
         :key
         "q" #'org-ql-search
         "v" #'org-ql-view))
  :config
  (with-eval-after-load 'org-ql-view
    (dolist (key '("g"))
      (define-key org-ql-view-map (kbd key)
                  (lookup-key evil-motion-state-map (kbd key))))))

(use-package org-roam
  ;; Reached by command, so loaded by one.  The keys are bound at startup and
  ;; straight's autoloads carry them into the right file -- which for the
  ;; dailies is org-roam-dailies.el, not org-roam.el, so `:commands' is
  ;; deliberately absent: use-package would generate autoloads naming the
  ;; wrong file and overwrite the correct ones.
  ;;
  ;; What waits with it is `org-roam-db-autosync-mode', in `:config'.  Until
  ;; the first org-roam command of a session, saving a note does not update the
  ;; database; `M-x org-roam-db-sync' repairs that, and every command that
  ;; reads the database loads org-roam first and so turns the mode on.
  :defer t
  :init
  (setq org-roam-v2-ack t)
  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "f" #'org-roam-node-find
         "j" #'org-roam-dailies-goto-today
         "z" #'org-roam-dailies-capture-today)
   (:map global-map
         :prefix "C-c n"
         :key
         "l" #'org-roam-buffer-toggle
         "f" #'org-roam-node-find
         "g" #'org-id-get-create
         "c" #'org-roam-capture
         "j" #'org-roam-dailies-goto-today
         "z" #'org-roam-dailies-capture-today
         "i" #'org-roam-node-insert
         "t" #'org-roam-tag-add
         "A" #'org-roam-alias-add
         "R" #'org-roam-ref-add)
   (:map org-mode-map
         :after org
         :prefix "C-c n"
         :key
         "r" #'org-roam-refile)
   ;; Page through *existing* dailies (gaps skipped); `goto-yesterday'/`-tomorrow'
   ;; instead step a fixed calendar day and create the note if missing.
   (:map org-mode-map
         :after org
         :state normal motion
         :key
         "]d" #'org-roam-dailies-goto-next-note
         "[d" #'org-roam-dailies-goto-previous-note))
  :config
  ;; `org-roam-db-location' is left to no-littering (var/org/org-roam.db).
  ;; The db is a regenerable cache of the .org files, so it stays machine-local
  ;; and out of the synced notes dir; only the .org files (and data/) sync.
  (setq org-roam-directory (file-truename org-directory)
        org-roam-file-exclude-regexp "/[Aa]rchive/"
        org-roam-completion-everywhere t)

  ;; Show tags alongside the title in `org-roam-node-find', so nodes are
  ;; discoverable and filterable by tag instead of by filename alone.
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))

  (org-roam-db-autosync-mode)

  ;; People/delegation management via generic Org mechanisms (a custom dynamic
  ;; block + capture templates + tags), not bespoke commands.
  (defun org-dblock-write:people (params)
    "Dynamic block listing everything :who is part of, whichever way round.

`:PEOPLE:' names who a piece of work involves and the TODO state says the
relation: WAIT and DELEG are with them, anything else needs them.  So one
block answers both halves of a person -- what they owe and what they are
holding up -- where two blocks would have had to be read together.

Unlike the built-in `org-ql' dynamic block (current-buffer only), this
searches `org-agenda-files', so a `person' node shows everything anywhere.
Header: `#+BEGIN: people :who \"名前\"'.  Refresh with C-c C-c on the block or
`org-update-all-dblocks'."
    (require 'org-ql)
    (let* ((who (or (plist-get params :who) ""))
           (render
            (lambda (query)
              (ignore-errors
                (org-ql-select (org-agenda-files) query
                  :sort 'scheduled
                  :action (lambda ()
                            (format "- %s %s%s"
                                    (org-get-todo-state)
                                    (org-get-heading t t t t)
                                    (let ((s (org-entry-get nil "SCHEDULED")))
                                      (if s (format "  %s" s) ""))))))))
           (with-them (funcall render
                               `(and (todo "WAIT" "DELEG")
                                     (property "PEOPLE" ,who))))
           (needs-them (funcall render
                                `(and (not (todo "WAIT" "DELEG"))
                                      (not (done))
                                      (property "PEOPLE" ,who)))))
      (insert "With them:\n"
              (if with-them (string-join with-them "\n") "- (none)")
              "\n\nNeeds them:\n"
              (if needs-them (string-join needs-them "\n") "- (none)"))))

  (defun my/org-roam-node-log-target (&optional filter prompt)
    "Capture target: the `my/org-log-heading' subtree of a node read from PROMPT.
FILTER narrows the candidates the way `org-roam-node-read' expects; without one,
every node is offered.  Creates the heading when the node does not have it yet,
so any node can receive notes without being prepared for them in advance."
    (require 'org-roam)
    (let* ((node (org-roam-node-read nil filter nil t (or prompt "Log on: ")))
           (file (org-roam-node-file node)))
      (unless (and file (file-exists-p file))
        (user-error "No node selected"))
      (set-buffer (org-capture-target-buffer file))
      (widen)
      (goto-char (point-min))
      ;; Leave point ON the heading rather than inside it.  `org-capture' reads
      ;; `org-at-heading-p' here to set `:target-entry-p', which is what confines
      ;; an `item' to this entry's own body -- and what lets an `entry' template
      ;; file itself as a child, should one ever target this.
      (unless (re-search-forward
               (format "^\\* %s[ \t]*$" (regexp-quote my/org-log-heading)) nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " my/org-log-heading))
      (goto-char (line-beginning-position))))

  (defun my/org-roam-person-log-target ()
    "Capture target: the log heading of a node tagged `person'.
`my/org-roam-node-log-target' with the candidates narrowed to direct reports --
a 1on1, a hallway chat, an observation, feedback, at any granularity, all
consolidated in that person's node.  Tasks are NOT refiled here; the `delegated'
block already shows what is out with them, as a live query."
    (my/org-roam-node-log-target
     (lambda (n) (member "person" (org-roam-node-tags n)))
     "Log on person: "))

  ;; These templates create *nodes*; they are what `org-roam-capture' and
  ;; `org-roam-node-find' offer when a title does not resolve to an existing
  ;; node.  Appending to a node that already exists is a separate act, handled
  ;; by the org-capture "g"/"o" templates -- org-roam's `:target' has no
  ;; `function' type, and choosing an existing entry to add to is org-capture's
  ;; job.  The two axes pair up: [d]efault/[p]erson create, "g"/"o" append.
  ;;
  ;; No tag prompt on the default template: forcing a tag at creation time is
  ;; friction paid on every note, and an empty answer leaves a bare
  ;; `#+filetags:' line behind.  Tags are added later, once a note has enough
  ;; shape to deserve one, with `org-roam-tag-add' (C-c n t).
  ;;
  ;; Of what the person template lays out, only the `people' block is
  ;; person-specific -- it queries :PEOPLE:, and only an agent can be one.  Role/Since, Objectives and the log heading
  ;; are generic, and `:person:' is just the label the candidate filter reads.
  ;; Another category of node (a place, a system) needs no new machinery: create
  ;; it with [d] and append to it with "g".
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "person" plain "%?"
           :target (file+head
                    "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                    ,(concat "#+title: ${title}\n#+filetags: :person:\n\n- Role ::\n- Since ::\n\n* Objectives / 期待\n\n* Together\n#+BEGIN: people :who \"${title}\"\n#+END:\n\n* " my/org-log-heading "\n"))
           :unnarrowed t)))

  ;; Daily notes live under daily/ but share the single central attachment store
  ;; (see `org-attach-id-dir'), so promoting a daily entry to a standalone note
  ;; keeps its `attachment:' links valid.
  (setq org-roam-dailies-directory "daily/")

  ;; The pressure-free half of the time axis.  journal.org feeds the agenda and
  ;; the clock reports, so an entry there is a commitment; here nothing is, which
  ;; is the point -- fragments, questions and moods go in without deserving to.
  ;; What grows gets promoted afterwards, with `org-roam-refile' or `org-refile'.
  ;;
  ;; `file+head+olp' creates the file, its ID, its title and the log heading on
  ;; its own (see `org-roam-capture-find-or-create-olp'), so notes land in the
  ;; same place C-c c g puts them with no target function of our own.
  (setq org-roam-dailies-capture-templates
        `(("d" "subject in today" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" ,my/org-daily-head))
          ("n" "note" item ,my/org-note-template
           :target (file+head+olp "%<%Y-%m-%d>.org" ,my/org-daily-head
                                  (,my/org-log-heading)))))
  ;; There was a third template here that recorded a mood on a five-point
  ;; scale.  It went unused for six years, and by the time ACT arrived it was
  ;; also pointing the wrong way: a scale becomes a series, and a series
  ;; invites wanting the number to improve -- which is the struggle ACT is
  ;; about, not a record of it.  Naming what showed up still happens, in the
  ;; `釣られた思考・感情' line of a choice point, where it sits beside what was
  ;; actually done about it.  A plain note ("n") takes anything else.

  ;; Display behavior
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-protocol
  :straight nil
  :after org-roam
  :config
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :target (file+head "%<%Y-%m-%dT%H-%M-%S>-${slug}.org"
                              "#+title: ${title}")
           :unnarrowed t))))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :diminish (org-roam-ui-mode org-roam-ui-follow-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  (unless org-roam-ui-mode
    (org-roam-ui-mode 1)))

;; Stable, refreshable buffer of nodes matching a tag/link/backlink/date
;; query.  Complements Deft (filename/full-text) rather than replacing it.
(use-package org-roam-ql :after org-roam)

;; Minibuffer-driven full-text search and backlink navigation, on top of the
;; existing consult/vertico stack.  Coexists with Deft and deadgrep.
(use-package consult-org-roam
  :after org-roam
  :diminish consult-org-roam-mode
  :config
  (consult-org-roam-mode 1)
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "s" #'consult-org-roam-search)
   (:map global-map
         :prefix "C-c n"
         :key
         "s" #'consult-org-roam-search
         "B" #'consult-org-roam-backlinks
         "F" #'consult-org-roam-forward-links)))

(use-package pdf-tools
  :if (display-graphic-p)
  ;; Not loaded until a PDF is opened.  `pdf-tools-install' checks that
  ;; `epdfinfo' is built and current, which means a subprocess and a walk of
  ;; the build directory -- nine seconds of every Windows startup, measured,
  ;; against under two on macOS, for a program most sessions never open a PDF
  ;; in.  `pdf-loader-install' registers the file associations and defers that
  ;; check to the first PDF, which is the only moment its answer is wanted.
  :defer t
  :init (pdf-loader-install)
  :config
  ;; MSYS2 + pdf-tools setup (for Windows)
  ;; 1. Install MSYS2 (e.g., scoop install msys2)
  ;; 2. Install required packages via pacman:
  ;;    - base-devel
  ;;    - mingw-w64-x86_64-toolchain
  ;;    - mingw-w64-x86_64-{zlib, libpng, poppler, imagemagick}
  ;;    - autotools
  ;; 3. Build via Emacs:
  ;;    M-x pdf-tools-install
  ;;    M-x pdf-info-check-epdfinfo

  (blink-cursor-mode 0))    ;; Better UX for PDF buffers

(use-package org-pdftools
  :after (org pdf-tools org-noter-pdftools)
  :config
  (add-hook 'org-mode-hook #'org-pdftools-setup-link))

(use-package org-noter
  :after (org pdf-tools)
  :config
  (my/define-key
   (:map pdf-view-mode-map
         :state normal
         :key
         "i" #'org-noter-insert-note
         "q" #'org-noter-kill-session
         "&" #'open-externally)
   (:map pdf-history-minor-mode-map
         :state normal
         :key
         "TAB" #'org-noter-insert-note-toggle-no-questions))

  (setq org-noter-always-create-frame nil
        org-noter-notes-search-path (list org-directory)
        org-noter-doc-property-in-notes t))

(use-package deft
  :after evil
  :config
  (my/define-key
   (:map global-map
         :key
         "C-c n d" #'deft)
   (:map deft-mode-map
         :key
         "C-RET" #'deft-complete
         "C-c C-c" #'deft-new-file
         "C-j" (lambda ()
                 (interactive)
                 (evil-next-line)
                 (my/deft-open-close-file))
         "C-k" (lambda ()
                 (interactive)
                 (evil-previous-line)
                 (my/deft-open-close-file)))
   (:map deft-mode-map
         :state normal
         :key
         "TAB" #'my/deft-open-close-file
         "d" #'deft-filter-clear
         "p" #'deft-filter-yank
         "o" #'my/deft-switch-file-other-window
         "gr" #'deft-refresh
         "q" #'quit-window))

  (defun my/clear-button-key ()
    (define-key button-map (kbd "TAB") nil)
    (define-key button-map (kbd "BACKTAB") nil)
    (define-key button-map (kbd "C-RET") nil))

  (add-hook 'deft-mode-hook #'my/clear-button-key)

  (setq deft-directory org-directory
        deft-archive-directory "archive/"
        deft-default-extension "org"
        deft-ignore-file-regexp (concat "\\(?:" "^$" "\\)" "\\|.#")
        deft-recursive nil
        deft-recursive-ignore-dir-regexp (concat "\\(?:" "\\."
                                                 "\\|\\.\\." "\\)$"
                                                 "\\|\\bdata$"
                                                 "\\|.org-attaches$"
                                                 "\\|\\b[Aa]rchive$")
        deft-new-file-format "%Y-%m-%dT%H-%M-%S"
        deft-use-filter-string-for-filename t
        deft-use-filename-as-title nil
        deft-file-naming-rules '((noslash . "-") (nospace . "-"))
        deft-org-mode-title-prefix t
        deft-markdown-mode-title-level 1
        deft-auto-save-interval 0)

  (defun my/deft-parse-title (file contents)
    (if deft-use-filename-as-title
        (deft-base-filename file)
      (let ((begin (string-match "^#\\+title.+$" contents)))
        (if begin
            (funcall deft-parse-title-function
                     (substring contents begin (match-end 0)))))))
  (advice-add 'deft-parse-title :override #'my/deft-parse-title)

  (setq deft-strip-summary-regexp
        (concat
         deft-strip-summary-regexp
         "\\|^#.*$"
         "\\|^:PROPERTIES:.*$"
         "\\|^:ID:.*$"
         "\\|^:ROAM_REFS:.*$"
         "\\|^:END:.*$"
         "\\|- tags ::.*$"
         "\\|- source ::.*$"
         "\\|^;; -\\*-.*-\\*-$"
         "\\|\\(?:\\[\\[.*\\]\\(?:\\[.*\\]\\)?\\]\\)"))

  ;; Evil state helpers
  (defun my/deft-evil-normal-state (&rest args)
    (interactive)
    (set-face-attribute 'deft-header-face nil :inverse-video nil)
    (evil-normal-state args))

  ;; File opening/closing
  (defun my/deft-switch-file-other-window ()
    (interactive)
    (deft-open-file-other-window '(4)))
  (advice-add 'deft-complete :before #'my/deft-evil-normal-state)
  (advice-add 'deft-switch-file-other-window :before #'my/deft-evil-normal-state)

  (add-hook 'deft-mode-hook (lambda () (setq-local truncate-lines t)))

  (defun my/deft-open-close-file ()
    (interactive)
    (let ((filename (deft-filename-at-point)))
      (when filename
        (let* ((buffer (get-file-buffer filename))
               (window (get-buffer-window buffer)))
          (if (not buffer)
              (deft-open-file-other-window)
            (cond ((not window)
                   (deft-open-file-other-window))
                  (t
                   (delete-window window)
                   (kill-buffer buffer))))))))

  (defun my/deft-close-file ()
    (interactive)
    (let ((filename (deft-filename-at-point)))
      (when filename
        (let* ((buffer (get-file-buffer filename))
               (window (get-buffer-window buffer)))
          (when buffer
            (when window (delete-window window))
            (kill-buffer buffer))))))

  (when (fboundp 'migemo-forward)
    (defun my/deft-search-forward-migemo (str)
      (if deft-incremental-search
          (migemo-forward str nil t)
        (re-search-forward str nil t)))
    (advice-add 'deft-search-forward :override #'my/deft-search-forward-migemo)))

;;;; Finding the line that takes a second to step onto

;; Moving the cursor down a folded outline is not one thing.  Emacs runs the
;; motion, then every function on `post-command-hook', then a redisplay, and
;; Org may parse a subtree along the way -- and a profile that says
;; `command-execute' 91% has named all four at once.  These two measure the
;; same keystroke with those parts taken away one at a time, which is what
;; turns a percentage into a cause.
;;
;; Nothing here reads a line out.  What is reported is where the line is, how
;; long it is, how deep the heading is, whether it is folded and how much is
;; folded under it -- shapes, on a machine whose text cannot leave it.

(defun my/org-motion--shape ()
  "Describe the line at point without quoting any of it."
  (let ((heading (org-at-heading-p)))
    (list :line (line-number-at-pos)
          :chars (- (line-end-position) (line-beginning-position))
          :level (and heading (org-current-level))
          :folded (and heading (fboundp 'org-fold-folded-p)
                       (org-fold-folded-p (line-end-position)) t)
          :hides (when heading
                   (save-excursion
                     (let ((from (point)))
                       (org-end-of-subtree t t)
                       (count-lines from (point)))))
          :inside (cond ((org-at-table-p) 'table)
                        ((org-at-drawer-p) 'drawer)
                        ((org-at-property-p) 'property)
                        ((org-in-block-p '("src" "example" "quote" "export"))
                         'block)))))

(defun my/org-motion--step (hooks redisplay &optional visual)
  "Move down one line the way the command loop would, and return the seconds.
HOOKS runs `post-command-hook' as a real keystroke does; REDISPLAY forces the
screen to be brought up to date.  Between them they separate the motion from
what the motion sets off.

VISUAL is `logical' to move with `line-move-visual' off, or `raw' to use
`forward-line' and skip the line-move machinery altogether.  One line down a
folded outline means crossing everything the fold hides, and what that costs
depends on which of the two does the crossing."
  (let ((start (float-time))
        (this-command 'next-line)
        (last-command 'next-line))
    (pcase visual
      ('raw (ignore-errors (forward-line 1)))
      ('logical (let ((line-move-visual nil))
                  (ignore-errors (call-interactively #'next-line))))
      (_ (ignore-errors (call-interactively #'next-line))))
    (when hooks (run-hooks 'post-command-hook))
    (when redisplay (redisplay t))
    (- (float-time) start)))

;;;###autoload
(defun my/org-motion-sweep (&optional threshold)
  "Step down every line of this buffer and name the ones that cost.
THRESHOLD is in seconds, 0.05 by default.  The outline is left folded as it
is: a line that is expensive to arrive at while folded may be free when the
subtree under it is open, and it is the folded case being looked for."
  (interactive (list (read-number "Report lines slower than (seconds): " 0.05)))
  (unless (derived-mode-p 'org-mode) (user-error "This is for an Org file"))
  (let ((threshold (or threshold 0.05))
        (slow nil) (total 0.0) (steps 0) (worst 0.0) (worst-line nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((was (point))
               (cost (my/org-motion--step t t))
               (shape (my/org-motion--shape)))
          (when (= was (point)) (forward-line 1))
          (setq total (+ total cost) steps (1+ steps))
          (when (> cost worst)
            (setq worst cost worst-line (plist-get shape :line)))
          (when (> cost threshold)
            (push (cons cost shape) slow)))))
    (let ((buf (get-buffer-create "*org motion cost*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%d steps, %.1f s in all, %.0f ms each on average\n"
                          steps total (* 1000 (/ total (max 1 steps)))))
          (insert (format "slowest: line %s at %.0f ms\n\n" worst-line
                          (* 1000 worst)))
          (if (not slow)
              (insert (format "Nothing over %.0f ms.\n" (* 1000 threshold)))
            (insert "     ms  line   chars  level  folded  hides  inside\n")
            (dolist (row (sort slow (lambda (a b) (> (car a) (car b)))))
              (let ((s (cdr row)))
                (insert (format "%7.0f  %5s  %5s  %5s  %6s  %5s  %s\n"
                                (* 1000 (car row))
                                (plist-get s :line) (plist-get s :chars)
                                (or (plist-get s :level) "-")
                                (if (plist-get s :folded) "yes" "-")
                                (or (plist-get s :hides) "-")
                                (or (plist-get s :inside) "-"))))))
          (insert "\nGo to one of those lines and run M-x my/org-motion-here\n"
                  "to see which part of the keystroke is paying for it.\n")
          (goto-char (point-min))
          (special-mode)))
      (pop-to-buffer buf))))

;;;###autoload
(defun my/org-motion-here (&optional repeats)
  "Take the keystroke onto the next line apart, here.

Four measurements, each the same motion with one more part left out, so the
difference between two lines is what that part costs.  The element cache is
the fifth: `org-element-use-cache' off means Org parses what it needs instead
of keeping it, which is slower in general and faster when keeping it is what
went wrong."
  (interactive "p")
  (unless (derived-mode-p 'org-mode) (user-error "This is for an Org file"))
  (let* ((repeats (max 1 (or repeats 1)))
         (home (point))
         (best (lambda (hooks redisplay cache &optional visual)
                 (let ((low 1.0e9))
                   (dotimes (_ repeats low)
                     (goto-char home)
                     (let ((org-element-use-cache cache))
                       (setq low (min low (my/org-motion--step
                                           hooks redisplay visual))))))))
         (all (funcall best t t org-element-use-cache))
         (no-redisplay (funcall best t nil org-element-use-cache))
         (bare (funcall best nil nil org-element-use-cache))
         (no-cache (funcall best nil nil nil))
         (logical (funcall best nil nil org-element-use-cache 'logical))
         (raw (funcall best nil nil org-element-use-cache 'raw)))
    (goto-char home)
    (message
     (concat "keystroke %.0f ms · redisplay %.0f · post-command-hook %.0f "
             "· the motion %.0f  ||  the motion: no element cache %.0f "
             "· logical lines %.0f · forward-line %.0f")
     (* 1000 all) (* 1000 (- all no-redisplay))
     (* 1000 (- no-redisplay bare)) (* 1000 bare)
     (* 1000 no-cache) (* 1000 logical) (* 1000 raw))))

;; What a session accumulates, counted so two of them can be compared.
;;
;; The same file is quick in a fresh Emacs and slow in one that has been
;; running, so what is slow is not the file.  Something grows: overlays,
;; markers a package forgot to free, entries in the invisibility spec, hooks
;; added twice, timers nobody cancelled.  Any of them makes moving over
;; folded text dearer, because moving over folded text is a walk through
;; everything the fold hides and everything laid on top of it.
;;
;; Run it once while Emacs is quick and again when it has gone slow: the
;; second call prints what changed.  Counts only -- no name of anything.

(defvar my/org-motion-state--last nil
  "The last counts taken by `my/org-motion-state\\=', for comparison.")

(defun my/org-motion-state--counts ()
  "Return an alist of the things that grow in a long-running session.
Counted with `safe-length\=': some of these are rings rather than lists --
`org-mark-ring\=' is joined end to end -- and `length\=' does not come back
from one."
  (let ((local (lambda (hook)
                 (safe-length (remq t (if (local-variable-p hook)
                                     (buffer-local-value hook (current-buffer))
                                   nil))))))
    (list
     (cons "buffer characters" (buffer-size))
     (cons "overlays here" (safe-length (overlays-in (point-min) (point-max))))
     (cons "overlays everywhere"
           (apply #'+ (mapcar (lambda (b)
                                (with-current-buffer b
                                  (safe-length (overlays-in (point-min) (point-max)))))
                              (buffer-list))))
     (cons "invisibility spec"
           (if (listp buffer-invisibility-spec)
               (safe-length buffer-invisibility-spec) 1))
     (cons "pre-command-hook here" (funcall local 'pre-command-hook))
     (cons "post-command-hook here" (funcall local 'post-command-hook))
     (cons "post-command-hook global" (safe-length (default-value 'post-command-hook)))
     (cons "before-change here" (funcall local 'before-change-functions))
     (cons "after-change here" (funcall local 'after-change-functions))
     (cons "timers" (safe-length timer-list))
     (cons "idle timers" (safe-length timer-idle-list))
     (cons "buffers" (safe-length (buffer-list)))
     (cons "org buffers"
           (safe-length (seq-filter (lambda (b)
                                 (with-current-buffer b
                                   (derived-mode-p 'org-mode)))
                               (buffer-list))))
     (cons "agenda markers org keeps"
           (if (boundp 'org-agenda-markers) (safe-length org-agenda-markers) -1))
     (cons "org mark ring" (safe-length org-mark-ring))
     (cons "global mark ring" (safe-length global-mark-ring))
     (cons "clock history"
           (if (boundp 'org-clock-history) (safe-length org-clock-history) -1))
     (cons "kill ring" (safe-length kill-ring))
     (cons "font-lock keywords"
           (safe-length (ignore-errors (cadr font-lock-keywords))))
     (cons "garbage collections" gcs-done)
     (cons "seconds in gc" (round gc-elapsed)))))

;;;###autoload
(defun my/org-motion-state ()
  "Count what has piled up, and against the last count if there is one.
Take one in a fresh Emacs and another when it has gone slow; what grew is
what to look at."
  (interactive)
  (let* ((now (my/org-motion-state--counts))
         (before my/org-motion-state--last)
         (buf (get-buffer-create "*org session state*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if before
                    "count now, and how it moved since the last one\n\n"
                  "the first count -- run this again when Emacs has gone slow\n\n"))
        (dolist (row now)
          (let* ((was (cdr (assoc (car row) before)))
                 (delta (and was (- (cdr row) was))))
            (insert (format "%28s  %9s%s\n" (car row) (cdr row)
                            (cond ((null delta) "")
                                  ((zerop delta) "")
                                  (t (format "   %+d" delta)))))))
        (goto-char (point-min))
        (special-mode)))
    (setq my/org-motion-state--last now)
    (pop-to-buffer buf)))

;;;; The horizons above the project
;; org-foresight holds the two lowest altitudes -- projects and next actions --
;; and answers everything with the clock.  What is above them is a different
;; kind of question: not "does it fit" but "what is this in service of", which
;; no amount of time arithmetic reaches.  org-convect holds those rungs.
;;
;; Nothing here points downward.  A task carries a CATEGORY, CATEGORY is
;; inherited, and that is the whole of a task's relationship to the ladder --
;; so opening a five-minute job never costs a thought about purpose.  The climb
;; happens in the review and nowhere else.
(use-package org-convect
  :straight (org-convect :host github :repo "yoshzucker/org-convect"
                         :files ("*.el"))
  :after org
  :config
  (setq org-convect-files (list (concat org-directory "horizons.org")))

  ;; ACT's life domains: not a rung and not a hierarchy, but the check that
  ;; keeps the areas from turning out to be entirely about work.  Few is better
  ;; than many -- a domain nothing is ever written about becomes a blank that
  ;; gets skipped, and skipping is a habit.
  (setq org-convect-act-domains '("仕事" "家族" "健康" "学び" "つながり" "余暇"))

  ;; Sections are scaffolding: an entry belongs to a rung because it says so,
  ;; not because of where it sits.  They are ordered lowest first because that
  ;; is the order they get filled -- GTD's own advice is to clear the runway
  ;; before reaching for purpose, and the file should not argue with that.
  ;;
  ;; The `#+COLUMNS' line is what makes the property drawers worth writing:
  ;; `C-c C-x C-c' on a value is the table of its choice points, so nothing has
  ;; to be stored twice.
  (setq org-convect-skeleton
        (concat "#+title: Horizons\n"
                "#+COLUMNS: %40ITEM(項目) %CONVECT_HORIZON(高さ)"
                " %CONVECT_SERVES(仕える先) %ACT_STRUGGLE(もがき)"
                " %ACT_MOVE(向かう)\n\n"
                "* 関心と責任の領域\n"
                "* 目標\n"
                "* ビジョン\n"
                "* 目的と原則\n")))

(provide 'my-app-org)
;;; my-app-org.el ends here
