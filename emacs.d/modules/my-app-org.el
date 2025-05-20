;;; my-app-org.el --- Org-mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for org-mode including keybindings, clocking, and general behaviors.

;;; Code:

(use-package org
  :straight org-contrib
  :after evil
  :init
  (setq system-time-locale "C")
  (defcustom my/org-main-directory
    (file-name-as-directory "~/Documents/memex/")
    "Top directory for org-mode system.")
  (defcustom org-complexbrain-directory
    (file-name-as-directory (concat my/org-main-directory "complexbrain/"))
    "Directory for org-roam and other structured org files.")
  :config
  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "t" #'toggle-truncate-lines
         "e" #'echo-line
         "l" #'org-store-link
         "c" #'org-capture
         "a" #'org-agenda
         "p" #'org-cliplink)
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
         :key
         "C-c w"       #'org-refile-goto-last-stored
         "C-c C-v C-b" #'org-dblocks-babel-execute-buffer
         "C-RET"       #'org-insert-heading-respect-content
         "C-c C-."     #'org-todo)
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
        org-cycle-include-plain-lists 'integrate
        org-M-RET-may-split-line '((default . nil))
        org-indirect-buffer-display 'current-window
        org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto))
        org-todo-keyword-faces '(("ONGO" . org-ongo)
                                 ("WAIT" . org-wait)
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

  (setq org-directory org-complexbrain-directory)
  (setq org-default-notes-file (concat org-directory "inbox.org"))
  (setq org-agenda-files `(,org-default-notes-file)) 
  
  (defvar org-project-file (concat org-directory "project.org"))
  (defvar org-journal-file (concat org-directory "journal.org"))
  (defvar org-memex-file (concat org-directory "memex.org"))

  (defun find-org-recursive (&rest dirs)
    "Recursively find all .org files in DIRS."
    (seq-mapcat (lambda (dir)
                  (directory-files-recursively dir "\\.org\\'"))
                dirs))
  
  (defun my/find-todo-files (dir)
    (let ((abs (expand-file-name dir)))
      (with-temp-buffer
        (apply #'call-process
               "rg" nil t nil
               (append
                '("--type-add" "org:*.org"
                  "-torg"
                  "-l"
                  "--no-heading" "--no-config" "--max-columns" "300"
                  "^*+ NEXT|ONGO|WAIT")
                (list abs)))
        (split-string (buffer-string) "\n" t))))

  (when (and (executable-find "rg")
             (file-directory-p org-complexbrain-directory))
    (setq org-agenda-files
          (cl-union org-agenda-files
                    (my/find-todo-files org-complexbrain-directory)
                    :test #'string=)))

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

  ;; Refile Settings
  (setq org-refile-targets
        `((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 4)))
  
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes nil)

  ;; Archive behavior
  (setq org-archive-location "%s_archive::"
        org-cycle-open-archived-trees nil
        org-sparse-tree-open-archived-trees nil
        org-columns-skip-archived-trees t)
  
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

  (defun my/org-time-stamp-in-evil-insert (orig-fn &rest args)
    "Insert org timestamp with proper evil state handling."
    (if (memq evil-state '(motion normal visual))
        (let ((orig-state evil-state))
          (evil-open-below 1)
          (apply orig-fn args)
          (evil-change-state orig-state))
      (apply orig-fn args)))
  
  (advice-add 'org-time-stamp :around #'my/org-time-stamp-in-evil-insert)
  

  ;; 
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

  ;; cookie
  (setq org-provide-todo-statistics t
        org-hierarchical-todo-statistics t
        org-track-ordered-property-with-tag t)

  ;; Tag settings
  (setq org-tag-persistent-alist
        '((:startgroup . nil) ("responsible" . ?r) ("support" . ?s) ("observe" . ?o) (:endgroup . nil)
          ("first" . ?f) ("dash" . ?d) ("ignore" . ?i)
          (:startgroup . nil) ("@office" . ?c) ("@alone" . ?a) (:endgroup . nil)
          (:startgroup . nil) ("write" . ?w) ("read" . ?e) ("break" . ?b)
          ("organize" . ?g) ("comm" . ?m) ("paperwork" . ?p) ("move" . ?v) (:endgroup . nil)))
  
  (setq org-tags-column -76
        org-agenda-tags-column -79
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("kanban")
        org-agenda-use-tag-inheritance '(todo search timeline agenda)
        org-agenda-show-inherited-tags t
        org-tags-sort-function #'string<)
  
  ;; Property settings
  (setq org-global-properties
        '(("EFFORT_ALL"       . "0:00 0:02 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00")
          ("STYLE_ALL"        . "habit")
          ("COOKIE_DATA_ALL"  . "recursive")))
  
  (setq org-use-property-inheritance nil)

  ;; column
  (setq org-columns-default-format
        " %1TODO %35ITEM %5CATEGORY %8ALLTAGS %1PRIORITY %5EFFORT{:} %5CLOCKSUM_T{:}")

  ;; Clock settings
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
  
  (defun my/org-show-notification-message (msg)
    (message "%s" msg))
  (setq org-show-notification-handler #'my/org-show-notification-message)
  
  (defun my/org-clock-get-clock-string ()
    "Custom mode-line clock string showing [done/effort](heading)."
    (let ((clocked-time (org-clock-get-clocked-time)))
      (if org-clock-effort
          (let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
                 (work-done-str (propertize (org-duration-from-minutes clocked-time)
                                            'face (if (and org-clock-task-overrun
                                                           (not org-clock-task-overrun-text))
                                                      'org-mode-line-clock-overrun
                                                    'org-mode-line-clock)))
                 (effort-str (org-duration-from-minutes effort-in-minutes)))
            (format "[%s/%s](%s)" work-done-str effort-str org-clock-heading))
        (format "[%s](%s)"
                (org-duration-from-minutes clocked-time)
                org-clock-heading))))
  
  (advice-add 'org-clock-get-clock-string :override #'my/org-clock-get-clock-string)

  ;; Clock behavior
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
  
  ;; Clock mode-line color
  (defun my/org-clock-mode-line-color (&rest _)
    (set-face-attribute
     'org-mode-line-clock nil
     :foreground (face-attribute
                  (if org-clock-task-overrun
                      'org-mode-line-clock-overrun
                    'org-mode-line-clock-underrun)
                  :foreground)))
  (advice-add 'org-clock-update-mode-line :after #'my/org-clock-mode-line-color)
  
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
        '(("a" "add task" entry (file+datetree org-journal-file)
           "* NEXT %?\nSCHEDULED: %^t\n:LOGBOOK:\n- State \"NEXT\"       from              %U\n:END:")
          ("i" "interrupt task" entry (file+datetree org-journal-file)
           "* ONGO %?\n"
           :clock-in t :clock-resume t)
          ("s" "switch task" entry (file+datetree org-journal-file)
           "* ONGO %?\n"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("p" "appointment" entry (file+datetree org-journal-file)
           "* %? %^T\n"
           :jump-to-captured t)
          ("j" "journal" entry (file+datetree org-journal-file)
           "* %?\n- Note taken on %U \\\\\n"
           :jump-to-captured t)
          ("c" "clocking journal" entry (file+datetree org-journal-file)
           "* %?\n- Note taken on %U \\\\\n"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("n" "clocking note" plain (clock)
           "- Note taken on %U \\\\\n  Annotation %a\n  %?"
           :jump-to-captured t)
          ("l" "insert clock" entry (file+datetree org-journal-file)
           "* %?\n:LOGBOOK:\nCLOCK: %U--%U =>  0:00\n:END:")
          ("d" "insert done" entry (file+datetree org-journal-file)
           "* DONE %?\nCLOSED: %U\n:LOGBOOK:\nCLOCK: %U--%U =>  0:00\n:END:")
          ("w" "weekly review" entry (file+datetree org-journal-file)
           "* ONGO %?\n Note taken on %U \\\\\ng>"
           :clock-in t :clock-resume t)))
  
  ;; org-babel
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
  
  ;; ox, export
  (setq org-export-preserve-breaks nil)


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

(use-package org-tempo
  :straight nil
  :after org
  :config
  (setq tempo-interactive nil)

  (tempo-define-template "src-r"
                         '("#+begin_src R :results output"
                           n> p n>
                           "#+end_src" >)
                         "<r"
                         "<r[TAB] in evil-insert-state.")

  (tempo-define-template "src-r-graph"
                         '("#+begin_src R :results output graphics :file" p ".png"
                           n> n>
                           "#+end_src" >)
                         "<rg"
                         "<rg[TAB] in evil-insert-state."))

(use-package org-cliplink
  :after org)

(use-package org-super-links
  :straight (:host github :repo "toshism/org-super-links" :branch "develop" :files ("*.el" "out"))
  :after org
  :config
  (my/define-key
   (:map global-map
         :key
         "C-c l" #'org-super-links-store-link)
   (:map org-mode-map
         :key
         "C-c C-l" #'org-super-links-insert-link)))

(use-package org-attach
  :straight nil
  :after org)

(use-package org-download
  :after org
  :init
  (setq org-download-timestamp "%Y-%m-%dT%H-%M-%S-"
        org-download-screenshot-basename (concat (make-temp-name "") ".png"))
  :config
  (my/define-key
   (:map org-mode-map :key "C-c s" #'org-clipboard))
  
  (setq org-download-method 'attach)

  ;; Redefine image dir logic if using 'directory method
  (when (eq org-download-method 'directory)
    (defun my/org-download-image-dir ()
      (concat (file-name-directory (buffer-file-name))
              (file-name-nondirectory (buffer-file-name))
              "-attaches/"))

    (defun my/org-download-dir-change (f &rest args)
      (let ((org-download-image-dir (my/org-download-image-dir))
            (org-download-heading-lvl nil))
        (apply f args)))

    (advice-add 'org-download-screenshot :around #'my/org-download-dir-change))

  ;; Use custom screenshot script on Windows
  (when (eq system-type 'windows-nt)
    (defun org-clipboard ()
      (interactive)
      (let ((script (locate-file "save-clipboard-image" exec-path '(".ps1"))))
        (if script
            (let ((org-download-screenshot-method
                   (format "powershell %s %%s" script)))
              (org-download-screenshot))
          (user-error "Cannot find %s in PATH" script))))))

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
        org-agenda-current-time-string
        (if (and (display-graphic-p) (char-displayable-p ?←))
            "← now -------------------------------------------------"
          "now - - - - - - - - - - - - - - - - - - - - - - - - -")
        org-agenda-log-mode-items '(closed clock state)
        org-agenda-timegrid-use-ampm nil
        org-clock-report-include-clocking-task t
        org-agenda-clockreport-parameter-plist
        '(:maxlevel 5 :lang "en" :scope agenda :block today :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :emphasize t :link t :narrow 26! :indent t :timestamp nil :level nil :tcolumns 1 :properties ("TAGS") :sort (4 . ?T) :file nil :formula "$1='(org-shorten-string $1 4)::$2='(org-shorten-string $2 5)::$5='(org-clock-time% @2$4 $4..$4);%.1f::$6='(orgtbl-ascii-draw $5 0 100 10)")
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
                        (org-agenda-span 'month))))))))

(use-package adaptive-wrap
  :after org-agenda
  :config
  (setq adaptive-wrap-extra-indent 20)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq truncate-lines nil)
              (adaptive-wrap-prefix-mode 1))))

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
          'font-lock-face 'calendar-iso-week-header)))

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

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (my/define-key
   (:map global-map
         :prefix "C-c n"
         :key
         "l" #'org-roam-buffer-toggle
         "f" #'org-roam-node-find
         "g" #'org-id-get-create
         "i" #'org-roam-node-insert
         "c" #'org-roam-capture
         "z" #'org-roam-dailies-capture-today)
   (:map org-mode-map
         :prefix "C-c n"
         :key
         "r" #'org-roam-refile))

  (setq org-roam-directory (file-truename my/org-main-directory)
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-completion-everywhere t)

  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
        '(("r" "roam" plain "%?"
           :target (file+head "complexbrain/%<%Y-%m-%dT%H-%M-%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t
           :immediate-finish t)))

  ;; Display behavior
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-pop-up-window)
                 (dedicated . t))))

(use-package org-roam-protocol
  :straight nil
  :after org-roam
  :config
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :target (file+head "complexbrain/%<%Y-%m-%dT%H-%M-%S>-${slug}.org"
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
  (org-roam-ui-mode 1))

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
        org-noter-notes-search-path (list org-complexbrain-directory)
        org-noter-doc-property-in-notes t))

(use-package citar
  :after org
  :init
  (defcustom my/org-cite-directory
    (file-name-as-directory (concat my/org-main-directory "bib/"))
    "Directory where BibTeX files and related citation data are stored.")
  :config
  (my/define-key
   (:map global-map
         :key
         "C-c n a" #'citar-open-notes)
   (:map org-mode-map
         :key
         "C-c b" #'org-cite-insert))

  (make-directory my/org-cite-directory t)

  (defvar my/org-cite-data-directory
    (file-name-as-directory (concat my/org-cite-directory "data/")))
  (make-directory my/org-cite-data-directory t)

  (defvar my/org-cite-file "references.bib")

  (let ((bib-file (concat my/org-cite-directory my/org-cite-file)))
    (unless (file-exists-p bib-file)
      (with-temp-buffer (write-file bib-file))))

  (add-hook 'org-mode-hook #'citar-capf-setup)

  (setq org-cite-global-bibliography (list (concat my/org-cite-directory my/org-cite-file))
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        citar-bibliography org-cite-global-bibliography
        citar-library-paths (list my/org-cite-data-directory)))

(use-package citar-embark
  :after (citar embark)
  :diminish citar-embark-mode
  :config
  (citar-embark-mode 1))

(use-package citar-org-roam
  :after (citar org-roam)
  :diminish citar-org-roam-mode
  :config
  (setq citar-notes-source 'orb-citar-source
        citar-org-roam-capture-template-key "b")

  (citar-register-notes-source
   'orb-citar-source
   `(:name "Org-Roam Notes"
     :category org-roam-node
     :items ,#'citar-org-roam--get-candidates
     :hasitems ,#'citar-org-roam-has-notes
     :open ,#'citar-org-roam-open-note
     :create ,#'orb-citar-edit-note
     :annotate ,#'citar-org-roam--annotate))

  (citar-org-roam-mode 1))

(use-package org-roam-bibtex
  :after citar org-roam
  :config
  (my/define-key 
   (:map org-mode-map :key "C-c n b" #'orb-note-actions))

  (setq bibtex-completion-bibliography citar-bibliography
        orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file" "eprint" "abstract")
        orb-process-file-keyword nil
        orb-file-field-extensions '("pdf"))

  (defun my/orb-replace-colon (string)
    (replace-regexp-in-string ":" "." string))

  (defun my/orb-strip-bib-file (string)
    (let ((rules '(("^:" . "") (":pdf$" . ""))))
      (seq-reduce (lambda (s rule)
                    (replace-regexp-in-string (car rule) (cdr rule) s))
                  rules string)))

  (defun my/orb-bib-file-relative (bib-file-field)
    (file-relative-name (my/orb-strip-bib-file bib-file-field)
                        my/org-cite-directory))

  (defun my/orb-bib-ref-mv (bib-file-field)
    (let ((file (my/orb-strip-bib-file bib-file-field)))
      (org-attach-attach file nil 'mv)))

  (add-to-list 'org-roam-capture-templates
               '("b" "bibliography reference" plain "%?"
                 :immediate-finish t
                 :if-new
                 (file+head "bib/%(my/orb-replace-colon \"${citekey}\").org"
                            "#+title: ${title}\n- tags ::\n- keywords :: ${keywords}\n\n* ${title}\n:PROPERTIES:\n:URL: ${url}\n:AUTHOR: ${author-or-editor}\n:NOTER_DOCUMENT: %(my/orb-bib-file-relative \"${file}\")\n:CREATE: %U\n:END:\n- abstract\n  ${abstract}\n")))

  (org-roam-bibtex-mode 1))

(use-package arxiv-mode
  :after citar
  :config
  (my/define-key
   (:map arxiv-mode-map
         :state normal
         :key
         "p" #'arxiv-prev-entry
         "n" #'arxiv-next-entry
         "d" #'arxiv-download-pdf
         "e" #'arxiv-download-pdf-export-bibtex
         "b" #'arxiv-export-bibtex
         "B" #'arxiv-export-bibtex-to-buffer
         "r" #'arxiv-refine-search
         "q" #'arxiv-exit
         "?" #'arxiv-help-menu/body
         "RET" #'arxiv-open-current-url
         "SPC" #'arxiv-SPC
         "<mouse-1>" #'arxiv-click-select-entry))

  (setq arxiv-pop-up-new-frame nil
        arxiv-default-bibliography (car org-cite-global-bibliography)
        arxiv-default-download-folder
        (file-name-as-directory (concat my/org-cite-directory "data/"))))

(use-package biblio
  :after citar
  :config
  (setq biblio-download-directory
        (file-name-as-directory (concat my/org-cite-directory "data/"))))

(use-package deft
  :after (evil org)
  :config
  (my/define-key
   (:map global-map
         :key
         "C-c d" #'deft)
   (:map deft-mode-map
         :key
         "C-c C-c" #'deft-new-file
         "C-RET" #'deft-complete
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

  (setq deft-directory org-complexbrain-directory
        deft-archive-directory "archive/"
        deft-default-extension "org"
        deft-ignore-file-regexp (concat "\\(?:" "^$" "\\)" "\\|.#")
        deft-recursive nil
        deft-recursive-ignore-dir-regexp (concat "\\(?:" "\\." "\\|\\.\\." "\\)$" "\\|\\bdata$" "\\|.org-attaches$" "\\|\\bArchive$")
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
        (concat (replace-regexp-in-string "\\\\)" "" deft-strip-summary-regexp)
                "\\|^#.*$"
                "\\|^:PROPERTIES:.*$"
                "\\|^:ID:.*$"
                "\\|^:ROAM_REFS:.*$"
                "\\|^:END:.*$"
                "\\|- tags ::.*$"
                "\\|- source ::.*$"
                "\\|^;; -\\*-.*-\\*-$"
                "\\|\\(\\[\\[.*\\]\\[\\|\\]\\]\\)"))

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

(provide 'my-app-org)
;;; my-app-org.el ends here
