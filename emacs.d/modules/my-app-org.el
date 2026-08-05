;;; my-app-org.el --- Org-mode configuration -*- lexical-binding: t; -*- ;;; Commentary:
;; Configuration for org-mode including keybindings, clocking, and general behaviors.
;;; Code:

(use-package org
  :straight org-contrib
  :after evil
  :init
  (setq system-time-locale "C")
  ;; Set `org-directory' here in `:init' (not `:config') so it is bound at
  ;; startup: org is deferred via `:after evil', and the startup agenda refresh
  ;; below reads it before org loads.  Presetting this defcustom is safe --
  ;; org.el will not clobber an already-bound value.
  (setq org-directory (file-name-as-directory "~/Documents/memex/"))

  ;; Agenda-file discovery also lives in `:init' so it is available at startup.
  ;; These helpers depend only on `org-directory', not on org itself.
  (defun my/find-org-recursive (&rest dirs)
    "Recursively find all .org files in DIRS."
    (seq-mapcat (lambda (dir)
                  (directory-files-recursively dir "\\.org\\'"))
                dirs))

  (defun my/find-todo-files (dir)
    "List .org files under DIR with an open (NEXT/ONGO/WAIT) heading via rg.
`.org_archive' files are excluded explicitly (the `*.org' rg type otherwise
matches them).  Falls back to listing every .org file recursively when rg
is unavailable."
    (let ((abs (expand-file-name dir)))
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
         "i"   #'org-mru-clock-in
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
  (setq org-todo-keywords
        '((sequence "NEXT(n!)" "ONGO(o!)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "WAIT(w@)" "|" "DELEG(e@)")))
  
  (setq org-log-done 'note
        org-treat-insert-todo-heading-as-state-change t
        org-log-state-notes-insert-after-drawers t)
  
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
  (setq org-global-properties
        '(("EFFORT_ALL"       . "0:00 0:02 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00")
          ("STYLE_ALL"        . "habit")
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
        org-clock-persist t
        org-clock-persist-query-save nil
        org-clock-idle-time 60
        org-clock-auto-clock-resolution 'when-no-clock-is-running
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

  (setq org-capture-templates
        '(("a" "add task" entry (file+datetree my/org-journal-file)
           "* NEXT %?\nSCHEDULED: %^t\n:LOGBOOK:\n- State \"NEXT\"       from              %U\n:END:")
          ("i" "interrupt task" entry (file+datetree my/org-journal-file)
           "* ONGO %?\n"
           :clock-in t :clock-resume t)
          ("s" "switch task" entry (file+datetree my/org-journal-file)
           "* ONGO %?\n"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("p" "appointment" entry (file+datetree my/org-journal-file)
           "* %? %^T\n"
           :jump-to-captured t)
          ("j" "journal" entry (file+datetree my/org-journal-file)
           "* %?\n- Note taken on %U \\\\\n"
           :jump-to-captured t)
          ("c" "clocking journal" entry (file+datetree my/org-journal-file)
           "* %?\n- Note taken on %U \\\\\n"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("n" "clocking note" plain (clock)
           "- Note taken on %U \\\\\n  Annotation %a\n  %?"
           :jump-to-captured t)
          ("l" "insert clock" entry (file+datetree my/org-journal-file)
           "* %?\n:LOGBOOK:\nCLOCK: %U--%U =>  0:00\n:END:")
          ("d" "insert done" entry (file+datetree my/org-journal-file)
           "* DONE %?\nCLOSED: %U\n:LOGBOOK:\nCLOCK: %U--%U =>  0:00\n:END:")
          ("w" "weekly review" entry (file+datetree my/org-journal-file)
           "* ONGO %?\n Note taken on %U \\\\\ng>"
           :clock-in t :clock-resume t)
          ;; Delegation: capture a brand-new task already handed off.  DELEG is a
          ;; done-type keyword, so it stays off the daily agenda; the "d" custom
          ;; command ("Delegation board") surfaces it, grouped by :DELEGATED_TO:.
          ;; SCHEDULED is the follow-up/check-in date.  For a task you are already
          ;; looking at, just change its state to DELEG -- the hook
          ;; `my/org-delegate-on-state-change' prompts for the same metadata.
          ("e" "delegate task" entry (file+datetree my/org-journal-file)
           "* DELEG %?\nSCHEDULED: %^t\n:PROPERTIES:\n:DELEGATED_TO: %^{Delegate to}\n:END:\n:LOGBOOK:\n- State \"DELEG\"      from              %U\n:END:")
          ;; Person log: append a dated entry under a chosen `person' node's "Log"
          ;; heading (1on1, chat, observation, feedback -- any granularity).
          ;; `my/org-roam-person-log-target' (in the org-roam block) resolves the
          ;; report via a person-filtered node prompt, keeping the log
          ;; consolidated in that person's node.
          ("o" "person log / やりとり" entry (function my/org-roam-person-log-target)
           "* %<%Y-%m-%d %a> %?")))
  
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
  :straight nil
  :after org
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
  :after org
  :config
  (my/define-key (:map org-mode-map :key "C-c p" #'org-cliplink)))

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
  :after (org org-clock)
  :config
  (my/define-key
   (:map global-map org-mode-map
         :key
         "C-c C-x C-p" #'org-pomodoro))
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

(use-package org-agenda
  :straight nil
  :after evil
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
        ;; custom CATEGORY block (`my/org-agenda-clocked-by-category'), so the
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
                        (org-agenda-start-with-log-mode nil) ; forward-looking, not a log
                        (org-super-agenda-groups nil)))
            (todo "NEXT" ((org-agenda-overriding-header "Unscheduled NEXT · 未スケジュール(取りこぼし)")
                          (org-agenda-todo-ignore-with-date t) ; only undated, i.e. stuck
                          (org-super-agenda-groups nil)))
            (todo "WAIT" ((org-agenda-overriding-header "WAIT · 他者待ち(要ナッジ?)")
                          (org-super-agenda-groups nil)))
            ;; DELEG is a done-type keyword ("|" ... DELEG); an explicit keyword
            ;; match still lists it, so delegated work stays visible for follow-up.
            (todo "DELEG" ((org-agenda-overriding-header "DELEG · 委譲済み(要フォロー?)")
                           (org-super-agenda-groups nil))))
           ;; A status check, not a time-tracking review: suppress the finalize
           ;; time-viz tables (viz-mode nil) and keep per-block headers visible.
           ((my/org-agenda-viz-mode nil)
            (org-agenda-compact-blocks nil)))
          ("r" "Weekly review — past 7 days"
           ((agenda "" ((org-agenda-overriding-header "Clock check · past week")
                        (org-agenda-span 'week)
                        (org-agenda-start-day "-1w")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-show-log 'clockcheck) ; clock lines + consistency audit
                        (org-agenda-todo-ignore-scheduled t)
                        (org-habit-show-habits nil)
                        (org-super-agenda-groups nil)))
            ;; Roughly the "n" command below; kept here so review is one stop.
            (todo "" ((org-agenda-overriding-header "Unscheduled / stuck TODOs")
                      (org-agenda-todo-ignore-with-date t)
                      (org-super-agenda-groups nil))))
           ;; General settings apply during `org-agenda-finalize' (see
           ;; `org-agenda-run-series'), so binding the viz mode here switches the
           ;; finalize append (`my/org-agenda-append-time-viz') from the daily
           ;; three tables to the week-by-area review table.
           ;; `compact-blocks' is off here (unlike the daily `a') so each block's
           ;; orientation header is shown in the review.
           ((my/org-agenda-viz-mode 'review)
            (org-agenda-compact-blocks nil)))
          ("d" "Delegation board · 委譲・待ち板"
           ;; Everything out with someone, grouped by person: WAIT (I'm blocked
           ;; on them) + DELEG (I handed it off).  DELEG is a done-type keyword
           ;; so the `tags' type is used (it lists done entries, unlike `todo');
           ;; sorted by SCHEDULED (the follow-up date) so overdue check-ins float
           ;; to the top of each person's group.
           ((tags "TODO=\"WAIT\"|TODO=\"DELEG\""
                  ((org-agenda-overriding-header "委譲・他者待ち — 人別 (DELEGATED_TO)")
                   (org-super-agenda-groups '((:auto-property "DELEGATED_TO")))
                   (org-agenda-sorting-strategy '(scheduled-up priority-down)))))
           ((my/org-agenda-viz-mode nil)))))

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

  (add-hook 'org-after-todo-state-change-hook #'my/org-delegate-on-state-change)

  ;; ---- Daily time visualization appended to the agenda -------------------
  ;; Two <=80-wide blocks after the existing clockreport (which is left
  ;; untouched): planned-vs-actual (EFFORT vs today's clock, estimated tasks
  ;; only) and ActivityWatch observed app/AFK time.  Field widths are budgeted
  ;; with `truncate-string-to-width' so multibyte titles stay within 80 columns.

  (defvar my/org-ascii-bar-chars " ▏▎▍▌▋▊▉█"
    "Shades (empty..full) for `orgtbl-ascii-draw' bars; unicode block elements.
Shared by the three custom agenda time-viz tables.")

  (defvar my/org-agenda-viz-mode 'daily
    "Which time-viz block `my/org-agenda-append-time-viz' appends on finalize:
  `daily'  the three today tables (Clocked / Estimate / Observed)
  `review' the week-by-area review table (`my/org-agenda-week-review-clocked')
  nil      nothing.
Bound to `review' in the \"r\" command's general settings; defaults to `daily'
for the plain `a' agenda.")

  ;; agent-shell-style two-segment badge (cf. `agent-shell--make-button'):
  ;; a filled title chip followed by an outlined meaning chip, boxed.
  (defface my/org-agenda-viz-title
    '((t :inherit org-agenda-structure))
    "Left badge (filled): muted agenda fg on a subtle bg.")
  (defface my/org-agenda-viz-meaning
    '((t :inherit (org-agenda-structure highlight)))
    "Right badge (solid): `shadow' fg becomes the fill; text knocked out to
the page background via inverse-video.")
  ;; Right badge is a solid fill (inverse-video: `shadow' fg becomes the bg,
  ;; text drops to the page bg).  Both badges share one box color (= `shadow'
  ;; fg) so the left outline meets the right fill seamlessly (agent-shell
  ;; style).  Set via `set-face-attribute' so a plain reload re-applies these
  ;; (a bare `defface' would not touch an already-defined face).
  (let ((frame (face-foreground 'shadow nil t)))
    (set-face-attribute 'my/org-agenda-viz-title nil
                        :inverse-video t
                        :box (list :line-width -1 :color frame))
    (set-face-attribute 'my/org-agenda-viz-meaning nil
                        :box (list :line-width -1 :color frame)))

  (defun my/org-agenda-viz-title-string (title meaning)
    "Return a styled \"TITLE MEANING\" header as an agent-shell-like badge.
GUI: adjacent filled + outlined boxed chips.  TUI: bracketed fallback."
    (if (display-graphic-p)
        (concat (propertize (concat " " title " ") 'face 'my/org-agenda-viz-title)
                (propertize (concat " " meaning " ") 'face 'my/org-agenda-viz-meaning))
      (concat "[" title "] " meaning)))

  (defvar my/aw-base-url "http://127.0.0.1:5600/api/0"
    "Base URL of the local ActivityWatch REST API.
Use the IPv4 literal, not `localhost': on Windows `localhost' resolves to IPv6
`::1' first while ActivityWatch binds 127.0.0.1 only, so each request pays a
~2s connect-fallback before reaching the server.")
  (defvar my/aw-cache nil
    "Cons (FETCH-TIME . STRING) caching `my/aw-today-summary'.")
  (defvar my/aw-cache-ttl 120
    "Seconds to reuse `my/aw-cache' before refetching.")
  (defvar my/aw-clamp-afk-to-activity t
    "When non-nil, exclude overnight idle from today's afk total.
The day is treated as starting at the first not-afk activity, so `away (afk)'
counts only breaks within the active window, not pre-dawn idle time.")

  (defun my/aw--get-json (path)
    "GET PATH under `my/aw-base-url' and return parsed JSON, or nil on failure."
    (condition-case nil
        (let ((buf (url-retrieve-synchronously (concat my/aw-base-url path) t t 5)))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (if (bound-and-true-p url-http-end-of-headers)
                                 url-http-end-of-headers (point-min)))
                  (json-parse-buffer :object-type 'alist :array-type 'list))
              (kill-buffer buf))))
      (error nil)))

  (defun my/aw--find-bucket (buckets prefix)
    "Return the id (string) of the first bucket in BUCKETS whose id starts
with PREFIX.  BUCKETS is the parsed `/buckets/' alist."
    (let ((b (seq-find (lambda (kv) (string-prefix-p prefix (symbol-name (car kv))))
                       buckets)))
      (and b (symbol-name (car b)))))

  (defun my/aw--today-range ()
    "Return (START . END) ISO8601 strings for local midnight..now."
    (let* ((now (current-time))
           (d (decode-time now))
           (mid (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d))))
      (cons (format-time-string "%Y-%m-%dT%H:%M:%S%:z" mid)
            (format-time-string "%Y-%m-%dT%H:%M:%S%:z" now))))

  (defun my/aw--events (bucket start end)
    "Fetch BUCKET events between START and END (ISO8601 strings)."
    (my/aw--get-json
     (format "/buckets/%s/events?start=%s&end=%s&limit=20000"
             bucket (url-hexify-string start) (url-hexify-string end))))

  (defvar my/aw-app-categories
    '(("work" . ("Emacs" "Ghostty" "Terminal" "iTerm2" "Code" "Xcode"
                 "プレビュー" "Preview" "Claude" "Grok" "ActivityWatch"))
      ("comms" . ("メール" "Mail" "カレンダー" "Calendar" "Slack"
                  "メッセージ" "Messages" "Zoom"))
      ("distraction" . ("Safari" "Chrome" "Firefox" "YouTube" "X"
                        "Twitter" "Discord")))
    "Alist (CATEGORY . (APP-NAME...)) for the Observed table's Cat column.
Matched case-insensitively against the AW window `app' name; unmatched apps
fall into `other'.  Tune the app lists to taste.")

  (defun my/aw--category (app)
    "Return the category name for APP per `my/aw-app-categories', or \"other\"."
    (let ((a (downcase (or app ""))))
      (or (cl-loop for (cat . apps) in my/aw-app-categories
                   when (member a (mapcar #'downcase apps)) return cat)
          "other")))

  (defun my/aw--cat-abbrev (cat)
    "Return a 4-column display code for category CAT."
    (cond ((equal cat "comms") "comm")
          ((equal cat "distraction") "dist")
          ((equal cat "other") "misc")
          (t (truncate-string-to-width cat 4 0 ?\s))))

  (defun my/aw--parse-ts (s)
    "Parse an ActivityWatch ISO8601 timestamp S into an Emacs time value."
    (parse-iso8601-time-string s))

  (defun my/aw--afk-split (events)
    "Return (ACTIVE-SECONDS . AFK-SECONDS) from afk EVENTS."
    (let ((active 0) (afk 0))
      (dolist (e events)
        (let ((status (alist-get 'status (alist-get 'data e)))
              (dur (or (alist-get 'duration e) 0)))
          (cond ((equal status "not-afk") (setq active (+ active dur)))
                ((equal status "afk") (setq afk (+ afk dur))))))
      (cons active afk)))

  (defun my/aw--afk-after (afk-events day-start)
    "Sum afk-status seconds in AFK-EVENTS occurring at/after DAY-START.
The event straddling DAY-START is counted only for its post-DAY-START part."
    (let ((sum 0.0))
      (dolist (e afk-events sum)
        (when (equal (alist-get 'status (alist-get 'data e)) "afk")
          (let* ((s (my/aw--parse-ts (alist-get 'timestamp e)))
                 (dur (or (alist-get 'duration e) 0))
                 (end (time-add s (seconds-to-time dur)))
                 (cs (if (time-less-p s day-start) day-start s)))
            (when (time-less-p cs end)
              (setq sum (+ sum (float-time (time-subtract end cs))))))))))

  (defun my/aw--status-intervals (events status)
    "Return sorted (START . END) Emacs-time conses for STATUS periods in EVENTS.
STATUS is \"afk\" or \"not-afk\"."
    (let (ivs)
      (dolist (e events)
        (when (equal (alist-get 'status (alist-get 'data e)) status)
          (let ((s (my/aw--parse-ts (alist-get 'timestamp e)))
                (dur (or (alist-get 'duration e) 0)))
            (push (cons s (time-add s (seconds-to-time dur))) ivs))))
      (sort ivs (lambda (a b) (time-less-p (car a) (car b))))))

  (defun my/aw--overlap-seconds (s e intervals)
    "Seconds of [S,E] covered by sorted disjoint INTERVALS (list of (A . B))."
    (let ((sum 0.0))
      (dolist (iv intervals sum)
        (let ((a (car iv)) (b (cdr iv)))
          (when (and (time-less-p s b) (time-less-p a e))
            (let ((os (if (time-less-p s a) a s))
                  (oe (if (time-less-p e b) e b)))
              (setq sum (+ sum (float-time (time-subtract oe os))))))))))

  ;; --- Interval set algebra (return intervals, not just seconds) ----------
  ;; `my/aw--overlap-seconds' above only totals coverage; the coverage/leak
  ;; view needs the resulting intervals themselves (to bin per half-hour), so
  ;; these produce (START . END) time-cons lists.  Inputs need not be sorted or
  ;; disjoint -- each op normalizes first.

  (defun my/aw--intervals-normalize (ivs)
    "Sort IVS (list of (START . END) time conses) and merge overlaps/adjacencies.
Returns a fresh, sorted, disjoint list; never mutates IVS."
    (let ((sorted (sort (mapcar (lambda (iv) (cons (car iv) (cdr iv))) ivs)
                        (lambda (a b) (time-less-p (car a) (car b)))))
          out)
      (dolist (iv sorted (nreverse out))
        (if (and out (not (time-less-p (cdar out) (car iv))))
            ;; prev-end >= cur-start: overlap/touch -> extend prev end if longer
            (when (time-less-p (cdar out) (cdr iv))
              (setcdr (car out) (cdr iv)))
          (push iv out)))))

  (defun my/aw--intervals-intersect (a b)
    "Return intervals covered by BOTH A and B (normalized internally)."
    (let ((a (my/aw--intervals-normalize a))
          (b (my/aw--intervals-normalize b))
          out)
      (while (and a b)
        (let* ((ae (cdar a)) (be (cdar b))
               (lo (if (time-less-p (caar a) (caar b)) (caar b) (caar a)))
               (hi (if (time-less-p ae be) ae be)))
          (when (time-less-p lo hi) (push (cons lo hi) out))
          (if (time-less-p ae be) (setq a (cdr a)) (setq b (cdr b)))))
      (nreverse out)))

  (defun my/aw--intervals-subtract (a b)
    "Return the parts of A not covered by B (normalized internally)."
    (let ((a (my/aw--intervals-normalize a))
          (b (my/aw--intervals-normalize b))
          out)
      (dolist (iv a (nreverse out))
        (let ((cur (car iv)) (end (cdr iv)) (bs b))
          ;; drop b-intervals ending at/before cur
          (while (and bs (not (time-less-p cur (cdar bs)))) (setq bs (cdr bs)))
          (while (and bs (time-less-p (caar bs) end))
            (let ((os (caar bs)) (oe (cdar bs)))
              (when (time-less-p cur os)
                (push (cons cur (if (time-less-p os end) os end)) out))
              (setq cur (if (time-less-p oe cur) cur oe))
              (setq bs (cdr bs))))
          (when (time-less-p cur end) (push (cons cur end) out))))))

  (defun my/aw--intervals-seconds (ivs)
    "Total seconds covered by interval list IVS."
    (let ((sum 0.0))
      (dolist (iv ivs sum)
        (setq sum (+ sum (float-time (time-subtract (cdr iv) (car iv))))))))

  (defun my/aw--sum-active-by (events intervals key)
    "Return alist (VALUE . SECONDS) desc: each EVENTS duration clipped to
INTERVALS (not-afk), grouped by KEY (\\='app or \\='project) of its data."
    (let ((h (make-hash-table :test 'equal)) out)
      (dolist (e events)
        (let* ((s (my/aw--parse-ts (alist-get 'timestamp e)))
               (dur (or (alist-get 'duration e) 0))
               (ov (my/aw--overlap-seconds
                    s (time-add s (seconds-to-time dur)) intervals)))
          (when (> ov 0)
            (let ((v (or (alist-get key (alist-get 'data e)) "?")))
              (puthash v (+ ov (gethash v h 0)) h)))))
      (maphash (lambda (k v) (push (cons k v) out)) h)
      (seq-sort-by #'cdr #'> out)))

  (defun my/aw--binned-active (intervals)
    "Return a 48-element vector of active seconds per local half-hour bin."
    (let ((v (make-vector 48 0.0)))
      (dolist (iv intervals v)
        (let ((a (float-time (car iv)))
              (b (float-time (cdr iv))))
          (while (< a b)
            (let* ((dt (decode-time (seconds-to-time a)))
                   (min (nth 1 dt))
                   (bin (+ (* 2 (nth 2 dt)) (if (>= min 30) 1 0)))
                   ;; seconds elapsed into the current half-hour bin
                   (into (+ (* 60 (mod min 30)) (nth 0 dt)))
                   (bin-end (+ a (- 1800 into)))
                   (seg-end (min b bin-end)))
              (aset v bin (+ (aref v bin) (- seg-end a)))
              ;; guard against a zero-length step at an exact boundary
              (setq a (if (> seg-end a) seg-end (+ a 1800)))))))))

  (defvar my/aw--spark-chars "▁▂▃▄▅▆▇█"
    "Vertical bar glyphs (U+2581..U+2588) for `my/aw--sparkline'.
Unlike `my/org-ascii-bar-chars' (horizontal fill, for table bars), these stack
from the baseline so per-cell height encodes intensity — a real sparkline.")

  (defun my/aw--spark-char (frac)
    "Return the `my/aw--spark-chars' glyph for FRAC (0.0-1.0), or ?· if <=0."
    (if (<= frac 0.0) ?·
      (let ((n (1- (length my/aw--spark-chars))))
        (aref my/aw--spark-chars (max 0 (min n (round (* frac n))))))))

  (defun my/aw--bin-frac (binned i)
    "Active fraction (0.0-1.0) of half-hour bin I in BINNED (sec per bin)."
    (min 1.0 (max 0.0 (/ (aref binned i) 1800.0))))

  (defun my/aw--sparkline (binned)
    "Return a 48-char active-intensity sparkline for BINNED (active sec per
half-hour).  Each cell is normalized by the fixed 30-minute (1800s) bin length."
    (let ((out (make-string 48 ?\s t)))
      (dotimes (i 48 out)
        (aset out i (my/aw--spark-char (my/aw--bin-frac binned i))))))

  (defun my/aw--dominant-face (ca cf ua uf i)
    "Face for half-hour bin I: whichever of CA/CF/UA/UF (clocked×active/afk
48-vectors from `my/aw-coverage-data') has the most seconds there."
    (let ((best-v (aref ca i)) (best-f 'my/aw-tl-clocked-active))
      (when (> (aref cf i) best-v) (setq best-v (aref cf i) best-f 'my/aw-tl-clocked-afk))
      (when (> (aref ua i) best-v) (setq best-v (aref ua i) best-f 'my/aw-tl-unclocked-active))
      (when (> (aref uf i) best-v) (setq best-f 'my/aw-tl-unclocked-afk))
      best-f))

  (defun my/aw--sparkline-colored (binned ca cf ua uf)
    "Like `my/aw--sparkline' but each cell's face shows whichever of
CA/CF/UA/UF (clocked×active/afk 48-vectors from `my/aw-coverage-data')
dominates that half-hour, so one row shows both activity density and
clocked-vs-unclocked status."
    (mapconcat
     (lambda (i)
       (propertize (char-to-string
                    (my/aw--spark-char (my/aw--bin-frac binned i)))
                   'face (my/aw--dominant-face ca cf ua uf i)))
     (number-sequence 0 47) ""))

  (defun my/aw--hour-axis ()
    "Return a 48-char axis line (30-min cells) with hour ticks at 0/6/12/18/23."
    (let ((s (make-string 48 ?\s)))
      (dolist (h '(0 6 12 18 23) s)
        (let* ((lbl (number-to-string h))
               (start (min (* 2 h) (- 48 (length lbl)))))
          (dotimes (i (length lbl)) (aset s (+ start i) (aref lbl i)))))))

  (defun my/aw--switch-count (window-events)
    "Count app transitions across WINDOW-EVENTS (order-independent)."
    (let ((prev nil) (n 0))
      (dolist (e window-events n)
        (let ((app (alist-get 'app (alist-get 'data e))))
          (when (and prev (not (equal app prev))) (setq n (1+ n)))
          (setq prev app)))))

  (defun my/aw-today-data ()
    "Return today's parsed ActivityWatch data as a plist, or nil on failure.
Keys: :active :afk SECONDS ; :active-apps :emacs-projects ALIST (NAME . SEC)
of window/emacs time intersected with not-afk ; :binned 48-vector of active
sec per half-hour ; :first :last not-afk boundary times ; :switches count ;
:window-events :afk-events the raw AW events (reused by the coverage view).
Cached for `my/aw-cache-ttl' seconds; shared by the ① coverage metric
\(uses :active) and the ③ Observed table."
    (if (and my/aw-cache
             (< (float-time (time-subtract (current-time) (car my/aw-cache)))
                my/aw-cache-ttl))
        (cdr my/aw-cache)
      (let ((data
             (condition-case nil
                 (let* ((buckets (my/aw--get-json "/buckets/"))
                        (wb (my/aw--find-bucket buckets "aw-watcher-window")))
                   (when wb
                     (let* ((ab (my/aw--find-bucket buckets "aw-watcher-afk"))
                            (eb (my/aw--find-bucket buckets "aw-watcher-emacs"))
                            (rng (my/aw--today-range))
                            (win (my/aw--events wb (car rng) (cdr rng)))
                            (afk-ev (and ab (my/aw--events ab (car rng) (cdr rng))))
                            (em (and eb (my/aw--events eb (car rng) (cdr rng))))
                            (split (my/aw--afk-split afk-ev))
                            (ivs (my/aw--status-intervals afk-ev "not-afk"))
                            (day-start (and ivs (car (car ivs))))
                            (afk-sec (if (and my/aw-clamp-afk-to-activity day-start)
                                         (my/aw--afk-after afk-ev day-start)
                                       (cdr split))))
                       (list :active (car split) :afk afk-sec
                             :active-apps (my/aw--sum-active-by win ivs 'app)
                             :emacs-projects (and em (my/aw--sum-active-by
                                                      em ivs 'project))
                             :binned (my/aw--binned-active ivs)
                             :first (and ivs (car (car ivs)))
                             :last (and ivs (cdr (car (last ivs))))
                             :switches (my/aw--switch-count win)
                             ;; Raw events retained so the coverage/leak view
                             ;; reuses this single cached fetch (no extra HTTP).
                             :window-events win :afk-events afk-ev))))
               (error nil))))
        (setq my/aw-cache (cons (current-time) data))
        data)))

  ;; --- Coverage (clocked vs leak) -----------------------------------------
  ;; Partition the active window into 4 disjoint classes by `clocked?' (inside
  ;; an org CLOCK segment) x `active/afk' (ActivityWatch).  Answers "when I was
  ;; NOT clocked in but active, which apps ate the time?" -- the leak that is
  ;; either forgotten-clock work (work-category apps) or distraction.  Folded
  ;; into `my/aw-today-summary' below: one sparkline colored by clocked-status
  ;; instead of a second 4-row timeline, and a "Leak" column on the same
  ;; per-app table instead of a second leak-only table.

  (defface my/aw-tl-clocked-active '((t :inherit success))
    "Sparkline fill for clocked & active (declared focus).")
  (defface my/aw-tl-clocked-afk '((t :inherit shadow))
    "Sparkline fill for clocked & afk (clock running while away).")
  (defface my/aw-tl-unclocked-active '((t :inherit warning))
    "Sparkline fill for unclocked & active (on-PC leak -- the actionable one).")
  (defface my/aw-tl-unclocked-afk '((t :inherit font-lock-comment-face))
    "Sparkline fill for unclocked & afk (off-PC leak).")

  (defun my/aw-coverage-data (clock)
    "Return today's clocked/leak coverage as a plist, or nil when AW is down.
Reuses `my/aw-today-data' (single cached fetch) and CLOCK's :today-intervals
(the plist from `my/org-clock-scan').
Keys:
  :active-sec :clocked-sec :leak-sec  totals (seconds)
  :ca :cf :ua :uf  48-vectors (active sec per half-hour) for
     clocked-active / clocked-afk / unclocked-active / unclocked-afk
  :leak-apps  ALIST (APP . SEC) desc over the unclocked-active window."
    (let ((data (my/aw-today-data)))
      (when data
        (let ((first (plist-get data :first))
              (last (plist-get data :last)))
          (when (and first last)
            (let* ((win (plist-get data :window-events))
                   (afk-ev (plist-get data :afk-events))
                   (window (list (cons first last)))
                   (active (my/aw--intervals-intersect
                            (my/aw--status-intervals afk-ev "not-afk") window))
                   (afk (my/aw--intervals-intersect
                         (my/aw--status-intervals afk-ev "afk") window))
                   (clocked (my/aw--intervals-intersect
                             (plist-get clock :today-intervals) window))
                   (ca (my/aw--intervals-intersect clocked active))
                   (cf (my/aw--intervals-intersect clocked afk))
                   (ua (my/aw--intervals-subtract active clocked))
                   (uf (my/aw--intervals-subtract afk clocked))
                   (leak-apps (my/aw--sum-active-by win ua 'app))
                   (off-pc (my/aw--intervals-seconds uf)))
              (list :active-sec (my/aw--intervals-seconds active)
                    :clocked-sec (my/aw--intervals-seconds clocked)
                    :leak-sec (+ (my/aw--intervals-seconds ua) off-pc)
                    :ca (my/aw--binned-active ca)
                    :cf (my/aw--binned-active cf)
                    :ua (my/aw--binned-active ua)
                    :uf (my/aw--binned-active uf)
                    :leak-apps leak-apps)))))))

  (defun my/aw-today-summary (clock)
    "Return today's ActivityWatch \"reality & rhythm\" block (lines <=80 cols).
Header (boundaries/active/afk/switches/clocked/leak) + hourly sparkline
(colored by clocked-status when CLOCK's coverage data is available) + a table
partitioning the observed day into active apps (with a category tag and, when
available, the unclocked/leak portion of that app's time) plus an away(afk)
row, then an emacs-project detail line.  CLOCK is the plist from
`my/org-clock-scan', passed through to `my/aw-coverage-data'."
    (let ((data (my/aw-today-data)))
      (if (not data)
          (propertize "(ActivityWatch unavailable)" 'face 'org-table)
        (let* ((active (plist-get data :active))
               (afk (plist-get data :afk))
               (full (max 1.0 (+ active afk)))
               (cov (my/aw-coverage-data clock))
               (leak-by-app (and cov
                                  (let ((h (make-hash-table :test 'equal)))
                                    (dolist (kv (plist-get cov :leak-apps))
                                      (puthash (car kv) (cdr kv) h))
                                    h)))
               (apps (plist-get data :active-apps))
               (top (seq-take apps 8))
               (rest-sum (apply #'+ (mapcar #'cdr (seq-drop apps 8))))
               (rows (append
                      (mapcar (lambda (kv)
                                (list (my/aw--category (car kv)) (car kv) (cdr kv)
                                      (and leak-by-app (gethash (car kv) leak-by-app))))
                              top)
                      (when (> rest-sum 60)
                        (list (list "other" "other apps" rest-sum nil)))
                      (list (list "idle" "away (afk)" (float afk) nil))))
               (rows (seq-filter (lambda (r) (> (nth 2 r) 0)) rows))
               (rows (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b)))))
               (maxrow (if rows (apply #'max (mapcar (lambda (r) (nth 2 r)) rows)) 1))
               (first (plist-get data :first))
               (last (plist-get data :last)))
          (propertize
           (concat
            (format "Screen %s–%s · active %s · afk %s · %d switches%s"
                    (if first (format-time-string "%H:%M" first) "—")
                    (if last (format-time-string "%H:%M" last) "—")
                    (org-duration-from-minutes (/ active 60.0))
                    (org-duration-from-minutes (/ afk 60.0))
                    (plist-get data :switches)
                    (if cov
                        (format " · clocked %s · leak %s"
                                (org-duration-from-minutes (/ (plist-get cov :clocked-sec) 60.0))
                                (org-duration-from-minutes (/ (plist-get cov :leak-sec) 60.0)))
                      ""))
            "\n" (my/aw--hour-axis)
            "\n" (if cov
                     (my/aw--sparkline-colored (plist-get data :binned)
                                                (plist-get cov :ca) (plist-get cov :cf)
                                                (plist-get cov :ua) (plist-get cov :uf))
                   (my/aw--sparkline (plist-get data :binned)))
            "\n" (format "| %s | %s | %5s | %5s | %6s | %s |"
                         (truncate-string-to-width "Cat" 4 0 ?\s)
                         (truncate-string-to-width "Activity" 12 0 ?\s)
                         "Time" "%" "Leak" (truncate-string-to-width "Share" 14 0 ?\s))
            "\n|------+--------------+-------+-------+--------+----------------|"
            (mapconcat
             (lambda (r)
               (let ((cat (nth 0 r)) (name (nth 1 r)) (sec (nth 2 r)) (leak (nth 3 r)))
                 (format "\n| %s | %s | %5s | %5.1f | %6s | %s |"
                         (truncate-string-to-width (my/aw--cat-abbrev cat) 4 0 ?\s)
                         (truncate-string-to-width
                          (replace-regexp-in-string "[|\n\r]" " " name) 12 0 ?\s)
                         (org-duration-from-minutes (/ sec 60.0))
                         (* 100.0 (/ sec full))
                         (if (and leak (> leak 0)) (org-duration-from-minutes (/ leak 60.0)) "")
                         (truncate-string-to-width
                          (orgtbl-ascii-draw sec 0 (max maxrow 1) 14
                                             my/org-ascii-bar-chars)
                          14 0 ?\s))))
             rows "")
            (let ((em (plist-get data :emacs-projects)))
              (if em
                  (concat "\nemacs: "
                          (mapconcat
                           (lambda (kv)
                             (format "%s %.0f" (or (car kv) "?") (/ (cdr kv) 60.0)))
                           (seq-take em 5) " · "))
                "")))
           'face 'org-table)))))

  (defun my/org-clock--day-start (&optional day-offset)
    "Return the Emacs time value for local midnight, DAY-OFFSET days back."
    (let ((d (decode-time (current-time))))
      (encode-time 0 0 0 (- (nth 3 d) (or day-offset 0)) (nth 4 d) (nth 5 d))))

  (defun my/org-clock-scan (days)
    "Scan `org-agenda-files' LOGBOOK CLOCK lines over the last DAYS days
\(today inclusive) in one pass.  A running clock (no end timestamp) is
closed at `current-time', so its elapsed-so-far time always counts -- every
consumer built on this plist agrees on whether \"now\" is included, unlike
the three separate hand-rolled scans this replaces.  Return a plist:
  :rows           (CATEGORY . MINUTES) alist for the whole window, desc
  :total          whole-window total minutes
  :byday          DAYS-length vector of per-day minutes, index 0 = oldest
  :days           DAYS
  :today-rows     (CATEGORY . MINUTES) alist for today only, desc
  :today-total    today's total minutes
  :today-segments today's clock-segment count (fragmentation)
  :today-intervals  today's (START . END) time conses.
Each segment is attributed once to its heading's inherited CATEGORY, so
:rows/:today-rows partition their window (minutes sum to :total/:today-total).
The org hierarchy depth is irrelevant: CATEGORY is inherited, so a GTD
project marked with `:CATEGORY:' at any level collects all descendant clocks."
    (let* ((today0 (my/org-clock--day-start 0))
           (today1 (time-add today0 (days-to-time 1)))
           (from (my/org-clock--day-start (1- days)))
           (now (current-time))
           (table (make-hash-table :test 'equal))
           (today-table (make-hash-table :test 'equal))
           (byday (make-vector days 0))
           (total 0) (today-total 0) (today-segments 0)
           today-intervals
           (re (concat "^[ \t]*" org-clock-string
                       "[ \t]*\\(\\[[^]\n]+\\]\\)\\(?:--\\(\\[[^]\n]+\\]\\)\\)?")))
      (dolist (file (org-agenda-files))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             ;; Read both groups before converting: `org-time-string-to-time'
             ;; runs `string-match' internally and would clobber match data.
             (let* ((s-str (match-string-no-properties 1))
                    (e-str (match-string-no-properties 2))
                    (s (org-time-string-to-time s-str))
                    (e (if e-str (org-time-string-to-time e-str) now)))
               (when (and (time-less-p s e) (time-less-p from e) (time-less-p s today1))
                 (let* ((cs (if (time-less-p s from) from s))
                        (ce (if (time-less-p today1 e) today1 e))
                        (dur (/ (float-time (time-subtract ce cs)) 60.0))
                        (cat (or (org-entry-get (point) "CATEGORY" t)
                                 (org-get-category (point))
                                 "?"))
                        (idx (min (1- days)
                                  (floor (/ (float-time (time-subtract cs from)) 86400)))))
                   (setq total (+ total dur))
                   (puthash cat (+ dur (gethash cat table 0)) table)
                   (aset byday idx (+ dur (aref byday idx)))
                   ;; The portion of this segment (if any) inside today.
                   (when (time-less-p today0 ce)
                     (let ((ts (if (time-less-p cs today0) today0 cs)))
                       (when (time-less-p ts ce)
                         (let ((today-dur (/ (float-time (time-subtract ce ts)) 60.0)))
                           (setq today-total (+ today-total today-dur)
                                 today-segments (1+ today-segments))
                           (puthash cat (+ today-dur (gethash cat today-table 0))
                                    today-table)
                           (push (cons ts ce) today-intervals))))))))))))
      (let (rows today-rows)
        (maphash (lambda (k v) (push (cons k v) rows)) table)
        (maphash (lambda (k v) (push (cons k v) today-rows)) today-table)
        (list :rows (seq-sort-by #'cdr #'> rows)
              :total total :byday byday :days days
              :today-rows (seq-sort-by #'cdr #'> today-rows)
              :today-total today-total :today-segments today-segments
              :today-intervals (nreverse today-intervals)))))

  (defun my/org-agenda--category-table (rows total maxmin col2-label)
    "Return a CATEGORY/Time/%/Share ASCII table (<=80 cols), shared by the
daily and weekly Clocked views.  ROWS is a (CATEGORY . MINUTES) alist; TOTAL
and MAXMIN scale the % and bar columns; COL2-LABEL names the category column
\(e.g. \"Project\" or \"Area\")."
    (if (null rows)
        (propertize "(no clocked time)" 'face 'org-table)
      (propertize
       (concat
        (format "| %-14s | %5s | %5s | %-18s |" col2-label "Time" "%" "Share")
        "\n|" (make-string 16 ?-) "+" (make-string 7 ?-) "+" (make-string 7 ?-)
        "+" (make-string 20 ?-) "|\n"
        (mapconcat
         (lambda (r)
           (let ((cat (car r)) (min (cdr r)))
             (format "| %-14s | %5s | %5.1f | %s |"
                     (truncate-string-to-width
                      (replace-regexp-in-string "[|\n\r]" " " cat) 14 0 ?\s)
                     (org-duration-from-minutes min)
                     (if (> total 0) (* 100.0 (/ (float min) total)) 0)
                     (truncate-string-to-width
                      (orgtbl-ascii-draw min 0 (max maxmin 1) 18
                                         my/org-ascii-bar-chars)
                      18 0 ?\s))))
         rows "\n"))
       'face 'org-table)))

  (defun my/org-agenda-clocked-by-category (clock)
    "Return today's CATEGORY-share table with a focus-budget header (<=80 cols).
The bar column is scaled to the largest project; the % column carries the
exact share of today's clocked total (so the % values sum to 100).
CLOCK is the plist from `my/org-clock-scan' (called with DAYS large enough
to also cover the :vs7d comparison below)."
    (let* ((rows (plist-get clock :today-rows))
           (total (plist-get clock :today-total))
           (segments (plist-get clock :today-segments))
           (avg7 (/ (plist-get clock :total) 7.0))
           (aw (my/aw-today-data))
           (active-min (and aw (/ (plist-get aw :active) 60.0)))
           (maxmin (if rows (apply #'max (mapcar #'cdr rows)) 1))
           (budget
            (concat
             (format "Focus %s" (org-duration-from-minutes total))
             (when (and active-min (> active-min 0))
               (format " · %.0f%% of active" (* 100.0 (/ total active-min))))
             (when (> segments 0)
               (format " · avg %.0fm ×%d" (/ (float total) segments) segments))
             (when (> avg7 0)
               (format " · vs7d %+.0f%%" (* 100.0 (/ (- total avg7) avg7)))))))
      (concat budget "\n" (my/org-agenda--category-table rows total maxmin "Project"))))

  (defun my/org-agenda-planned-vs-actual ()
    "Return today's EFFORT-vs-actual table (each line <=80) for estimated tasks."
    (let (rows)
      (dolist (file (org-agenda-files))
        (let ((entries
               (nth 2 (with-current-buffer (find-file-noselect file)
                        (ignore-errors
                          (org-clock-get-table-data
                           file '(:block today :properties ("Effort")
                                         :maxlevel 99)))))))
          (dolist (e entries)
            (let ((headline (nth 1 e))
                  (time (nth 4 e))
                  (effort (cdr (assoc "Effort" (nth 5 e)))))
              (when (and effort (> (or time 0) 0))
                (push (list headline (org-duration-to-minutes effort) time) rows))))))
      (propertize
       (if (null rows)
           "(no estimated tasks clocked today)"
         (concat
          (format "| %-28s | %5s | %5s | %-12s |%5s" "Task" "Plan" "Act" "Progress" "%")
          "\n|" (make-string 30 ?-) "+" (make-string 7 ?-) "+" (make-string 7 ?-)
          "+" (make-string 14 ?-) "+" (make-string 5 ?-) "\n"
          (mapconcat
           (lambda (r)
             (let ((plan (nth 1 r)) (act (nth 2 r)))
               (format "| %s | %5s | %5s | %s |%4.0f%%"
                       (truncate-string-to-width
                        (replace-regexp-in-string "[|\n\r]" " " (nth 0 r)) 28 0 ?\s)
                       (org-duration-from-minutes plan)
                       (org-duration-from-minutes act)
                       (truncate-string-to-width
                        (orgtbl-ascii-draw (min act plan) 0 (max plan 1) 12
                                           my/org-ascii-bar-chars)
                        12 0 ?\s)
                       (if (> plan 0) (* 100.0 (/ (float act) plan)) 0))))
           (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b))))
           "\n"))) 'face 'org-table)))

  (defun my/org-agenda-week-review-clocked (clock)
    "Return a week-by-CATEGORY review table with a rhythm header (<=80 cols).
Mirrors the daily `my/org-agenda-clocked-by-category' layout (Area/Time/%/Share)
but aggregates the past 7 days, and leads with weekly-review insight: total,
daily average, active-day count, and the busiest day.
CLOCK is the plist from `my/org-clock-scan'."
    (let* ((rows (plist-get clock :rows))
           (total (plist-get clock :total))
           (days (plist-get clock :days))
           (byday (plist-get clock :byday))
           (maxmin (if rows (apply #'max (mapcar #'cdr rows)) 1))
           (active 0) (peak 0) (peakmin 0))
      (dotimes (i days)
        (when (> (aref byday i) 0) (setq active (1+ active)))
        (when (> (aref byday i) peakmin) (setq peakmin (aref byday i) peak i)))
      (if (null rows)
          (concat "Week 0:00\n"
                  (propertize "(no clocked time this week)" 'face 'org-table))
        (concat
         (format "Week %s · avg %s/day · %d/%d active · peak %s %s"
                 (org-duration-from-minutes total)
                 (org-duration-from-minutes (/ (float total) days))
                 active days
                 (format-time-string "%a" (my/org-clock--day-start (- days 1 peak)))
                 (org-duration-from-minutes peakmin))
         "\n" (my/org-agenda--category-table rows total maxmin "Area")))))

  (defun my/org-agenda-viz-body ()
    "Return the time-viz text for the current `my/org-agenda-viz-mode'.
`daily' = the today tables; `review' = the week-by-area table.  Either mode
scans clock data once via `my/org-clock-scan' and threads the result to
every table that needs it, rather than each table re-scanning independently."
    (pcase my/org-agenda-viz-mode
      ('daily
       (let ((clock (my/org-clock-scan 7)))
         (concat "\n"
                 (my/org-agenda-viz-title-string "Clocked" "share of focus today")
                 "\n"
                 (my/org-agenda-clocked-by-category clock)
                 "\n\n"
                 (my/org-agenda-viz-title-string "Estimate" "planned vs actual")
                 "\n"
                 (my/org-agenda-planned-vs-actual)
                 "\n\n"
                 (my/org-agenda-viz-title-string "Observed" "reality & rhythm, clocked vs leak · ActivityWatch")
                 "\n"
                 (my/aw-today-summary clock)
                 "\n")))
      ('review
       (concat "\n"
               (my/org-agenda-viz-title-string "Clocked" "by area · last 7 days")
               "\n"
               (my/org-agenda-week-review-clocked (my/org-clock-scan 7))
               "\n"))))

  (defun my/org-agenda-append-time-viz ()
    "Append the current mode's time-viz block to an agenda view.
The block rendered is chosen by `my/org-agenda-viz-mode' (daily three tables /
review week table / nil none).
Runs on `org-agenda-finalize-hook'.  `org-agenda-change-all-lines' (used by
`org-agenda-todo') calls `org-agenda-finalize' narrowed to the single changed
line, and `org-agenda-finalize' does not widen before running the hook; the
`point-max' insertion below would then land the blocks inline after that line.
Skip unless the whole buffer is accessible, and remove any blocks left by a
previous finalize first so a non-narrowed re-finalize (e.g. clearing a filter)
replaces rather than accumulates."
    (when (and (derived-mode-p 'org-agenda-mode)
               my/org-agenda-viz-mode
               (not (buffer-narrowed-p)))
      (condition-case nil
          (let ((inhibit-read-only t)
                (pos (point-min)))
            ;; Drop blocks inserted by an earlier finalize (marked below).
            (while (setq pos (text-property-any pos (point-max)
                                                'my/org-agenda-viz t))
              (delete-region
               pos (or (next-single-property-change pos 'my/org-agenda-viz)
                       (point-max))))
            (goto-char (point-max))
            (let ((beg (point)))
              (insert (my/org-agenda-viz-body))
              (put-text-property beg (point) 'my/org-agenda-viz t)))
        (error nil))))

  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-append-time-viz t))

(use-package adaptive-wrap
  :after org-agenda
  :config
  (setq adaptive-wrap-extra-indent 20)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq truncate-lines t)
              (adaptive-wrap-prefix-mode t))))

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

(use-package org-super-agenda
  :after org-agenda
  :config
  (my/define-key
   (:map org-super-agenda-header-map
         :key
         "j" #'org-agenda-next-line
         "k" #'org-agenda-previous-line
         "v" (lookup-key org-agenda-mode-map "v")
         "w" (lookup-key org-agenda-mode-map "w")
         "d" (lookup-key org-agenda-mode-map "d")))

  (defun my/org-super-agenda-propagate-header-type ()
    "Copy `org-agenda-type' from each entry onto its org-super-agenda header.
`org-super-agenda' inserts group header lines that carry no
`org-agenda-type' text property (only real entry lines do).  Since
`org-agenda-update-agenda-type' recomputes the buffer-local
`org-agenda-type' from point's text property on every command, point
resting on a header line turns it nil, and any command gated by
`org-agenda-check-type' errors with \"No Org agenda currently displayed\".
Grouping itself runs via a `:filter-return' advice on
`org-agenda-finalize-entries' (see `org-super-agenda-mode'), which
completes before `org-agenda-finalize-hook' runs, so this always sees the
finished header layout regardless of hook ordering."
    (save-excursion
      (goto-char (point-min))
      (let (pos)
        (while (setq pos (text-property-not-all (point) (point-max)
                                                 'org-super-agenda-header nil))
          (goto-char pos)
          (unless (get-text-property pos 'org-agenda-type)
            (let ((type (get-text-property
                         (next-single-property-change pos 'org-agenda-type
                                                       nil (point-max))
                         'org-agenda-type)))
              (when type
                (put-text-property pos (1+ (line-end-position)) 'org-agenda-type type))))
          (goto-char (or (next-single-property-change pos 'org-super-agenda-header)
                         (point-max)))))))

  (add-hook 'org-agenda-finalize-hook #'my/org-super-agenda-propagate-header-type t)

  (org-super-agenda-mode 1)

  (setq org-super-agenda-groups
        '((:name "By-Today" :and (:scheduled today :log state) :order 100)
          (:name "State-Change" :log state :order 99)
          (:name "First" :tag "first" :order 10)
          (:name "Time-Grid" :time-grid t :log t :order 40)
          (:name "Dash" :and (:tag "dash") :order 20)
          (:name "Habit/Daily-Task-List" :habit t :order 30)
          (:auto-group t :order 90))))

(use-package origami
  :after org-super-agenda
  :config
  (my/define-key
   (:map org-super-agenda-header-map :key "TAB" #'origami-toggle-node))
  
  (defvar org-super-agenda-auto-fold-groups
    '("Habit/Daily-Task-List" "State-Change"))

  (defvar my/org-super-agenda-auto-fold-regexp
    (rx-to-string `(seq bol " " (or ,@org-super-agenda-auto-fold-groups)))
    "Compiled once from `org-super-agenda-auto-fold-groups' (a static list).")

  (defun my/org-super-agenda-origami-fold ()
    "Fold pre-defined groups in Org Super Agenda buffer."
    (goto-char (point-min))
    (while (re-search-forward my/org-super-agenda-auto-fold-regexp nil t)
      (origami-forward-toggle-node (current-buffer) (point))))

  (my/add-hook
   (:hook org-agenda-mode-hook
          :func #'origami-mode))

  (add-hook 'org-agenda-finalize-hook #'my/org-super-agenda-origami-fold t))

(use-package org-clock-split
  :after org)

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

(use-package ox-pandoc
  :after org)

(use-package org-ql
  :after org
  :config
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

  (with-eval-after-load 'org-ql-view
    (dolist (key '("g"))
      (define-key org-ql-view-map (kbd key)
                  (lookup-key evil-motion-state-map (kbd key))))))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
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
         :prefix "C-c n"
         :key
         "r" #'org-roam-refile)
   ;; Page through *existing* dailies (gaps skipped); `goto-yesterday'/`-tomorrow'
   ;; instead step a fixed calendar day and create the note if missing.
   (:map org-mode-map
         :state normal motion
         :key
         "]d" #'org-roam-dailies-goto-next-note
         "[d" #'org-roam-dailies-goto-previous-note))

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
  (defun org-dblock-write:delegated (params)
    "Dynamic block listing WAIT/DELEG tasks whose `:DELEGATED_TO:' is :who.
Unlike the built-in `org-ql' dynamic block (current-buffer only), this searches
`org-agenda-files', so a `person' node can show everything currently out with
that person.  Header: `#+BEGIN: delegated :who \"名前\"'.  Refresh with C-c C-c
on the block or `org-update-all-dblocks'."
    (require 'org-ql)
    (let* ((who (or (plist-get params :who) ""))
           (items (ignore-errors
                    (org-ql-select (org-agenda-files)
                      `(and (todo "WAIT" "DELEG") (property "DELEGATED_TO" ,who))
                      :sort 'scheduled
                      :action (lambda ()
                                (format "- %s %s%s"
                                        (org-get-todo-state)
                                        (org-get-heading t t t t)
                                        (let ((s (org-entry-get nil "SCHEDULED")))
                                          (if s (format "  %s" s) ""))))))))
      (insert (if items (string-join items "\n") "- (none)"))))

  (defun my/org-roam-person-log-target ()
    "Capture target: the end of a chosen `person' node's \"Log\" subtree.
Prompts (within the generic capture flow) for the report, so a dated log entry
-- a 1on1, a hallway chat, an observation, feedback, at any granularity -- stays
consolidated in that person's node.  The task themselves are NOT refiled here;
the `delegated' block already shows what is out with them (a live query)."
    (require 'org-roam)
    (let* ((node (org-roam-node-read
                  nil
                  (lambda (n) (member "person" (org-roam-node-tags n)))
                  nil t "Log for: "))
           (file (org-roam-node-file node)))
      (unless (and file (file-exists-p file))
        (user-error "No person node selected"))
      (set-buffer (org-capture-target-buffer file))
      (widen)
      (goto-char (point-min))
      ;; Leave point ON the "Log" heading: org-capture then files the entry as
      ;; its child (re-leveling the "* " template to level 2), like file+headline.
      (unless (re-search-forward "^\\* Log[ \t]*$" nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* Log"))
      (goto-char (line-beginning-position))))

  ;; Two node-creation templates, so `org-roam-capture'/`org-roam-node-find' now
  ;; offer a chooser: [d]efault note and [p]erson (a direct report).  A person
  ;; node carries the `:person:' tag (discoverable via `org-roam-node-find'), a
  ;; live `delegated' block (what is currently out with them), and a free-form
  ;; "Log" (any interaction, any granularity).  Log *entries* are appended by the
  ;; plain org-capture "o" template (org-roam's `:target' has no `function' type;
  ;; appending to an existing node is org-capture's job, not org-roam-capture's).
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: %^{tags}\n")
           :unnarrowed t)
          ("p" "person / 部下" plain "%?"
           :target (file+head
                    "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags: :person:\n\n- Role ::\n- Since ::\n\n* Objectives / 期待\n\n* Delegated / Waiting\n#+BEGIN: delegated :who \"${title}\"\n#+END:\n\n* Log\n")
           :unnarrowed t)))

  ;; Daily notes live under daily/ but share the single central attachment store
  ;; (see `org-attach-id-dir'), so promoting a daily entry to a standalone note
  ;; keeps its `attachment:' links valid.
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

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

  (pdf-tools-install)       ;; Run every time to ensure setup
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

(use-package org-dayflow
  :straight (:host github :repo "yoshzucker/org-dayflow")
  :after (org evil)
  :load-path "site-lisp/org-dayflow"
  :config
  ;; Personal category coloring for the timeline.  Category names live ONLY here
  ;; (never in the org-dayflow package); adjust to match your calendar sources.
  (setq org-dayflow-category-faces
        '(("outlook" . font-lock-keyword-face)))
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

(use-package activity-watch-mode
  :diminish (activity-watch-mode " aw")
  :config
  (setopt activity-watch-org-clock-active t)
  (global-activity-watch-mode))

(provide 'my-app-org)
;;; my-app-org.el ends here
