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
  (defun my/org-parent-ongo-if-needed ()
    "If the current task is ONGO/WAIT/DONE/DELEG, update parent TODO to ONGO if it's NEXT."
    (when (member org-state '("ONGO" "WAIT" "DONE" "DELEG"))
      (save-excursion
        (when (org-up-heading-safe)
          (when (member (org-entry-get nil "TODO") '("NEXT"))
            (org-todo "ONGO"))))))

  (defun my/org-clock-in-if-ongo ()
    "Clock in if the current task is ONGO and not already clocked in."
    (when (and (equal org-state "ONGO")
               (equal (point) (point-at-bol)) ;; only for direct clock-in
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
    "Toggle `org-clock-continuously` when called with C-u (prefix 64)."
    (if (equal select '(64))
        (let ((org-clock-continuously (not org-clock-continuously)))
          (apply f nil start-time))
      (apply f select start-time)))
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
           :clock-in t :clock-resume t)))
  
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
      (insert (format "[[attachment:%s]]\n" (org-link-escape basename)))
      (org-display-inline-images))))

(use-package org-download
  :after org
  :init
  (setq org-download-timestamp "%Y-%m-%dT%H-%M-%S-")
  :config
  (setq org-download-method 'attach))

(use-package sxiv
  :if (eq system-type 'gnu/linux)
  :after org)

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
         "gr" #'org-agenda-redo
         "[" #'org-agenda-filter
         "]" #'org-agenda-filter-by-tag
         my/backslash #'evil-avy-goto-char-timer
         "C-f" #'evil-scroll-page-down
         "C-b" #'evil-scroll-page-up
         "C-w" #'evil-window-map))

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
        org-agenda-start-with-clockreport-mode t
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
        org-agenda-log-mode-items '(closed clock state)
        org-clock-report-include-clocking-task t
        org-agenda-clockreport-parameter-plist
        '(:maxlevel 5 :lang "en" :scope agenda :block today :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :emphasize t :link t :narrow 26! :indent t :timestamp nil :level nil :tcolumns 1 :properties ("TAGS") :sort (4 . ?T) :file nil :formula "$1='(org-shorten-string $1 4)::$2='(org-shorten-string $2 5)::$5='(org-clock-time% @2$4 $4..$4);%.1f::$6='(orgtbl-ascii-draw $5 0 100 10 my/org-ascii-bar-chars)")
        org-clocktable-defaults org-agenda-clockreport-parameter-plist)
  
  ;; Override indent string for clocktable
  (defun my/org-clocktable-indent-string (level)
    "Return indentation string for org clocktable at LEVEL.
Top-level (1) entries have no indent. Deeper levels are indented by spaces."
    (if (= level 1) ""
      (concat "\\_" (substring (make-string (1- level) ?\s) 1))))
  (advice-add 'org-clocktable-indent-string :override 'my/org-clocktable-indent-string)
  
  ;; Agenda skip helpers
  (defun my/org-agenda-skip-if-blocked ()
    (when (org-entry-blocked-p)
      (save-excursion (or (outline-next-heading) (point-max)))))
  
  (defun my/toggle-org-agenda-skip-blocked ()
    "Toggle skipping of blocked tasks in agenda."
    (interactive)
    (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
    (let ((fn 'my/org-agenda-skip-if-blocked))
      (setq org-agenda-skip-function-global
            (if (member fn org-agenda-skip-function-global)
                (remove fn org-agenda-skip-function-global)
              (cons fn org-agenda-skip-function-global)))
      (org-agenda-redo)
      (message "Blocked tasks are %s"
               (if (member fn org-agenda-skip-function-global)
                   "skipped" "not skipped"))))
  
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
  
  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("r" "Agenda for review"
           ((agenda "" ((org-agenda-overriding-header "Review")
                        (org-agenda-span 'week)
                        (org-agenda-start-day "-1w")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-show-log t)
                        (org-agenda-log-mode-items '(clock))
                        (org-agenda-todo-ignore-scheduled t)
                        (org-habit-show-habits nil)
                        (org-super-agenda-groups nil)))))
          
          ("n" "TODO without timestamp"
           ((todo "" ((org-agenda-overriding-header "TODO without timestamp. Scatter tasks in weekdays by m... C-u B S")
                      (org-agenda-todo-ignore-with-date t)))))
          
          ("o" "ONGO only"
           ((todo "ONGO" ((org-agenda-overriding-header "Ongoing tasks only")))))
          
          ("d" "Agenda for deadline"
           ((agenda "" ((org-agenda-overriding-header "Deadlines")
                        (org-agenda-entry-types '(:deadline))
                        (org-deadline-warning-days 0)
                        (org-agenda-compact-blocks t)
                        (org-agenda-span 'month)))))))

  ;; ---- Daily time visualization appended to the agenda -------------------
  ;; Two <=80-wide blocks after the existing clockreport (which is left
  ;; untouched): planned-vs-actual (EFFORT vs today's clock, estimated tasks
  ;; only) and ActivityWatch observed app/AFK time.  Field widths are budgeted
  ;; with `truncate-string-to-width' so multibyte titles stay within 80 columns.

  (defvar my/org-ascii-bar-chars " ▏▎▍▌▋▊▉█"
    "Shades (empty..full) for `orgtbl-ascii-draw' bars; unicode block elements.
Shared by the agenda clockreport formula and the two custom time-viz tables.")

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

  (defvar my/aw-base-url "http://localhost:5600/api/0"
    "Base URL of the local ActivityWatch REST API.")
  (defvar my/aw-cache nil
    "Cons (FETCH-TIME . STRING) caching `my/aw-today-summary'.")
  (defvar my/aw-cache-ttl 120
    "Seconds to reuse `my/aw-cache' before refetching.")

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

  (defun my/aw--find-bucket (prefix)
    "Return the id (string) of the first AW bucket whose id starts with PREFIX."
    (let ((b (seq-find (lambda (kv) (string-prefix-p prefix (symbol-name (car kv))))
                       (my/aw--get-json "/buckets/"))))
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

  (defun my/aw--sum-by-app (events)
    "Return alist (APP . SECONDS) for EVENTS, sorted descending."
    (let ((h (make-hash-table :test 'equal)) out)
      (dolist (e events)
        (let ((app (or (alist-get 'app (alist-get 'data e)) "?"))
              (dur (or (alist-get 'duration e) 0)))
          (puthash app (+ dur (gethash app h 0)) h)))
      (maphash (lambda (k v) (push (cons k v) out)) h)
      (seq-sort-by #'cdr #'> out)))

  (defun my/aw--afk-split (events)
    "Return (ACTIVE-SECONDS . AFK-SECONDS) from afk EVENTS."
    (let ((active 0) (afk 0))
      (dolist (e events)
        (let ((status (alist-get 'status (alist-get 'data e)))
              (dur (or (alist-get 'duration e) 0)))
          (cond ((equal status "not-afk") (setq active (+ active dur)))
                ((equal status "afk") (setq afk (+ afk dur))))))
      (cons active afk)))

  (defun my/aw-today-summary ()
    "Return today's ActivityWatch summary as a string (each line <=80 columns)."
    (if (and my/aw-cache
             (< (float-time (time-subtract (current-time) (car my/aw-cache)))
                my/aw-cache-ttl))
        (cdr my/aw-cache)
      (let ((lines
             (condition-case nil
                 (let ((wb (my/aw--find-bucket "aw-watcher-window")))
                   (if (not wb)
                       "(ActivityWatch unavailable)"
                     (let* ((ab (my/aw--find-bucket "aw-watcher-afk"))
                            (rng (my/aw--today-range))
                            (apps (my/aw--sum-by-app
                                   (my/aw--events wb (car rng) (cdr rng))))
                            (total (apply #'+ (mapcar #'cdr apps)))
                            (afk (and ab (my/aw--afk-split
                                          (my/aw--events ab (car rng) (cdr rng))))))
                       (concat
                        (format "active %.1fh / afk %.1fh"
                                (/ (or (car afk) 0) 3600.0)
                                (/ (or (cdr afk) 0) 3600.0))
                        "\n"
                        (mapconcat
                         (lambda (kv)
                           (format "| %s | %4.0fm | %s |"
                                   (truncate-string-to-width
                                    (replace-regexp-in-string "[|\n\r]" " " (car kv))
                                    16 0 ?\s)
                                   (/ (cdr kv) 60.0)
                                   (truncate-string-to-width
                                    (orgtbl-ascii-draw (cdr kv) 0 (max total 1) 24
                                                       my/org-ascii-bar-chars)
                                    24 0 ?\s)))
                         (seq-take apps 8) "\n")))))
               (error "(ActivityWatch unavailable)"))))
        (setq lines (propertize lines 'face 'org-table))
        (setq my/aw-cache (cons (current-time) lines))
        lines)))

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
          "\n")) 'face 'org-table)))

  (defun my/org-agenda-append-time-viz ()
    "Append planned-vs-actual and ActivityWatch blocks to an agenda view."
    (when (and (derived-mode-p 'org-agenda-mode)
               (bound-and-true-p org-agenda-clockreport-mode))
      (condition-case nil
          (let ((inhibit-read-only t))
            ;; Title the native clockreport for consistency with the two blocks
            ;; below.  Done before appending, so the first table row is the
            ;; clockreport's (our own tables are not inserted yet).
            (goto-char (point-min))
            (when (re-search-forward "^[ \t]*|" nil t)
              (beginning-of-line)
              (insert "\n"
                      (my/org-agenda-viz-title-string "Clocked" "time by area, today")
                      "\n"))
            (goto-char (point-max))
            (insert "\n"
                    (my/org-agenda-viz-title-string "Estimate" "planned vs actual")
                    "\n"
                    (my/org-agenda-planned-vs-actual)
                    "\n\n"
                    (my/org-agenda-viz-title-string "Observed" "apps & AFK · ActivityWatch")
                    "\n"
                    (my/aw-today-summary)
                    "\n"))
        (error nil))))

  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-append-time-viz))

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

  (defun my/org-super-agenda-origami-fold ()
    "Fold pre-defined groups in Org Super Agenda buffer."
    (goto-char (point-min))
    (while (re-search-forward
            (rx-to-string `(seq bol " " (or ,@org-super-agenda-auto-fold-groups)))
            nil t)
      (origami-forward-toggle-node (current-buffer) (point))))

  (my/add-hook
   (:hook org-agenda-mode-hook
          :func #'origami-mode)
   (:hook org-agenda-finalize-hook
          :func #'my/org-super-agenda-origami-fold)))

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

  ;; Single template, so neither `org-roam-capture' nor the create path of
  ;; `org-roam-node-find' prompts for a template choice.  Prompts for tags and
  ;; opens the note unnarrowed for editing.
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: %^{tags}\n")
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
                 (display-buffer-in-side-window)
                 (side . right) (slot . 0) (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)
                                       (mode-line-format . nome)
                                       (window-size-fixed . width)
                                       (dedicated . t))))))

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

(use-package activity-watch-mode
  :diminish (activity-watch-mode " aw")
  :config
  (setopt activity-watch-org-clock-active t)
  (global-activity-watch-mode))

(provide 'my-app-org)
;;; my-app-org.el ends here
