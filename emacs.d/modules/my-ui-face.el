;;; my-ui-face.el --- Theme and face configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles theme selection, face definitions, and color palette setup.

;;; Code:

(defgroup my/theme nil
  "Custom theme settings."
  :group 'my/ui)

(defcustom my/theme-name 'rustcity
  "Which theme to use."
  :type 'symbol
  :options '(solarized nord rustcity)
  :group 'my/ui)

(defcustom my/frame-background 'dark
  "Which background to use."
  :type 'symbol
  :options '(light dark)
  :group 'my/ui)

(defcustom my/semantic-colors-override nil
  "Alist of semantic roles to override theme-based color mapping.
Example: '((primary . brightyellow))"
  :type '(alist :key-type symbol :value-type symbol)
  :group 'my/ui)

;; Update from environment
(let ((term (not (display-graphic-p))))
  (when term
    (my/map-env my/theme-name "theme_name")
    (my/map-env my/frame-background "theme_variant")))

(use-package nord-theme
  :defer t)

(use-package solarized-theme
  :straight (:host github :repo "sellout/emacs-color-theme-solarized" :branch "master" :files ("*.el" "out"))
  :defer t)

(use-package rustcity-theme
  :straight (:host github :repo "yoshzucker/rustcity-theme")
  :defer t)

(defun my/semantic-colors ()
  "Return semantic color mapping based on theme and background, with user overrides."
  (let ((base
         (pcase (list my/theme-name my/frame-background)
           ('(rustcity light)
            '((background-near . brightwhite)
              (background-far  . white)
              (foreground-far  . brightblack)
              (foreground-near . black)
              (primary         . brightcyan)
              (secondary       . blue)))
           ('(rustcity dark)
            '((background-near . black)
              (background-far  . brightblack)
              (foreground-far  . white)
              (foreground-near . brightwhite)
              (primary         . brightblue)
              (secondary       . blue)))
           (_ nil))))
    (append my/semantic-colors-override
            (seq-remove (lambda (pair)
                          (assoc (car pair) my/semantic-colors-override))
                        base))))

;; Color assignment
(defun my/set-colors (palette)
  ;; base palette
  (dolist (entry palette)
    (set (intern (format "my/%s" (car entry))) (cdr entry)))
  ;; extra aliases or literals
  (dolist (entry (my/semantic-colors))
    (let ((target (intern (format "my/%s" (car entry))))
          (value (cdr entry)))
      (set target
           (if (symbolp value)
               (symbol-value (intern (format "my/%s" value)))
             value)))))

;; Load theme and apply color palette
(defun my/load-theme ()
  "Load the theme specified in my/theme-name and assign colors."
  (mapc #'disable-theme custom-enabled-themes)
  (setq frame-background-mode my/frame-background )
  (pcase my/theme-name
    ('nord      (load-theme 'nord t))
    ('solarized (load-theme 'solarized t)
                (enable-theme 'solarized))
    ('rustcity  (load-theme 'rustcity t)
                (my/set-colors (rustcity-colors)))
    (_ (message "Unknown theme: %s" my/theme-name))))

;; Face rules application
(defvar my/copy-face-rules
  '((org
     (org-todo           . my/org-ongo)
     (org-todo           . my/org-wait)
     (org-priority       . my/org-priority-a)
     (org-priority       . my/org-priority-b)
     (mode-line          . my/mode-line-under)
     (mode-line          . my/mode-line-over))
    (calendar
     (calendar-weekday-header . my/calendar-iso-week-header))))

(defun my/apply-copy-face-rules ()
  (dolist (group my/copy-face-rules)
    (let ((feature (car group))
          (face-pairs (cdr group)))
      (with-eval-after-load feature
        (dolist (pair face-pairs)
          (copy-face (car pair) (cdr pair)))))))

(defcustom my/font-default
  (if (eq system-type 'darwin)
      "HackGen Console"
    "HackGen Console NF")
  "Default font family."
  :type 'string
  :group 'my/ui)

(defcustom my/font-variable
  (if (eq system-type 'darwin)
      "HackGen"
    "HackGen NF")
  "Variable pitch font family."
  :type 'string
  :group 'my/ui)

(defcustom my/font-emoji
  (cond ((eq system-type 'darwin) "Apple Color Emoji")
        ((eq system-type 'windows-nt) "Segoe UI Emoji")
        (t "Noto Color Emoji"))
  "Emoji font family."
  :type 'string
  :group 'my/ui)

(defcustom my/font-height
  (if (eq system-type 'darwin)
      140
    120)
  "Default font height."
  :type 'integer
  :group 'my/ui)

(defun my/set-face-rules ()
  `((font-lock
     (default :foreground my/foreground :background my/background
              :family ,my/font-default :height ,my/font-height)
     (fixed-pitch :family ,my/font-default)
     (variable-pitch :family ,my/font-variable)
     (fringe :force-inherit 'unspecified)
     (border :force-inherit 'unspecified)
     (vertical-border :force-inherit 'unspecified :foreground my/background-near)
     (internal-border :force-inherit 'unspecified)
     (mode-line :force-inherit 'unspecified
                :foreground my/background :background my/primary)
     (mode-line-inactive :force-inherit 'unspecified
                         :foreground my/primary :background my/background-near)
     (mode-line-buffer-id :force-inherit 'unspecified)
     (header-line :foreground my/primary :background my/background-far
                  :weight 'unspecified)
     (minibuffer-prompt :force-inherit 'unspecified :foreground my/primary)
     (cursor :background my/primary))
    (faces
     (show-paren-match :background my/background-near :weight 'bold))
    (tab-bar
     (tab-bar :foreground my/foreground :background my/background)
     (tab-bar-tab :foreground my/background :background my/primary :box 'unspecified)
     (tab-bar-tab-inactive :foreground my/primary :background my/background-near))
    (evil-snipe
     (evil-snipe-first-match-face :background my/background-far))
    (tooltip
     (tooltip :force-inherit 'highlight :foreground my/background-near))
    (compile
     (compilation-info :weight 'unspecified)
     (compilation-mode-line-fail :weight 'unspecified)
     (compilation-mode-line-exit :weight 'unspecified))
    (vertico
     (vertico-current :force-inherit 'unspecified :background my/background-near))
    (orderless
     (orderless-match-face-0 :weight 'unspecified :foreground my/brightred)
     (orderless-match-face-1 :weight 'unspecified :foreground my/magenta)
     (orderless-match-face-2 :weight 'unspecified :foreground my/green)
     (orderless-match-face-3 :weight 'unspecified :foreground my/red))
    (consult
     (consult-buffer :foreground my/foreground-near)
     (consult-file :foreground my/foreground-far))
    (corfu
     (corfu-default :background my/background)
     (corfu-current :foreground my/primary :background my/background-near)
     (corfu-bar :background my/primary))
    (deadgrep
     (deadgrep-filename-face :force-inherit 'font-lock-builtin-face))
    (isearch
     (isearch :foreground my/background :background my/brightyellow)
     (lazy-highlight :foreground my/background :background my/brightcyan))
    (avy
     (avy-lead-face :foreground my/background :background my/blue)
     (avy-lead-face-0 :foreground my/background :background my/brightred)
     (avy-lead-face-1 :foreground my/background :background my/red)
     (avy-lead-face-2 :foreground my/background :background my/magenta))
    (magit
     (magit-section-heading :foreground my/yellow :background my/background))
    (eglot
     (eglot-mode-line :weight 'unspecified))
    (ein-cell
     (ein:cell-input-area :background my/background-near)
     (ein:cell-input-prompt :foreground my/background :background my/primary)
     (ein:cell-output-prompt :foreground my/background :background my/secondary))
    (eww
     (eww-valid-certificate :weight 'unspecified :foreground my/green))
    (dired
     (dired-directory :force-inherit 'font-lock-type-face))
    (dired-subtree
     (dired-subtree-depth-1-face :background 'unspecified)
     (dired-subtree-depth-2-face :background 'unspecified)
     (dired-subtree-depth-3-face :background 'unspecified)
     (dired-subtree-depth-4-face :background 'unspecified)
     (dired-subtree-depth-5-face :background 'unspecified)
     (dired-subtree-depth-6-face :background 'unspecified))
    (treemacs
     (treemacs-root-face :height 'unspecified))
    (bookmark
     (bookmark-face :distant-foreground my/blue :background 'unspecified))
    (outline
     (outline-1 :inherit 'font-lock-type-face)
     (outline-2 :inherit 'font-lock-variable-name-face)
     (outline-3 :inherit 'font-lock-constant-face)
     (outline-4 :inherit 'font-lock-builtin-face)
     (outline-5 :inherit 'font-lock-function-name-face)
     (outline-6 :inherit 'font-lock-string-face)
     (outline-7 :inherit 'font-lock-warning-face)
     (outline-8 :inherit 'font-lock-keyword-face))
    (org
     (org-headline-done :foreground 'unspecified)
     (org-agenda-dimmed-todo-face :force-inherit 'font-lock-comment-face)
     (org-todo :force-inherit 'unspecified :inverse-video t 
               :foreground my/red :background my/background)
     (my/org-ongo :force-inherit 'unspecified :inverse-video t
                  :foreground my/brightred :background my/background)
     (my/org-wait :force-inherit 'font-lock-comment-face :inverse-video t)
     (org-done :force-inherit 'unspecified :inverse-video t
               :foreground my/green :background my/background)
     (org-document-title :force-inherit 'font-lock-constant-face)
     (org-column :force-inherit 'unspecified :background my/background-near)
     (org-column-title :force-inherit 'org-column)
     (org-table :foreground my/cyan)
     (org-tag :weight 'unspecified)
     (org-archived :force-inherit 'org-headline-done)
     (org-drawer :force-inherit 'font-lock-comment-face)
     (org-special-keyword :inherit 'font-lock-comment-face)
     (org-column :force-inherit 'font-lock-type-face)
     (org-date :force-inherit 'font-lock-type-face)
     (org-time-grid :force-inherit font-lock-comment-face)
     (org-scheduled :force-inherit 'unspecified :foreground my/green)
     (org-scheduled-today :force-inherit 'unspecified :foreground my/blue)
     (org-scheduled-previously :force-inherit 'unspecified :foreground my/brightred)
     (org-upcoming-deadline :force-inherit 'org-scheduled-previously)
     (org-agenda-structure :foreground my/green :weight 'unspecified)
     (org-agenda-current-time :force-inherit 'font-lock-keyword-face)
     (org-agenda-date-today :force-inherit 'font-lock-variable-name-face)
     (org-agenda-date-weekend :force-inherit 'font-lock-type-face)
     (org-agenda-clocking :force-inherit 'unspecified :slant 'italic)
     (my/mode-line-under :foreground my/background :background my/cyan)
     (my/mode-line-over :foreground my/background :background my/red))
    (org-habit
     (org-habit-overdue-face :background my/brightmagenta))
    (org-roam
     (org-roam-header-line :force-inherit 'header-line))
    (org-noter
     (org-noter-notes-exist-face :force-inherit 'unspecified :foreground my/green)
     (org-noter-no-notes-exist-face :force-inherit 'unspecified :foreground my/brightred))
    (deft
     (deft-header-face :force-inherit 'font-lock-builtin-face)
     (deft-title-face :force-inherit 'font-lock-constant-face))
    (calendar
     (calendar-today :force-inherit 'font-lock-warning-face)
     (calendar-weekend-header :force-inherit 'font-lock-type-face)
     (my/calendar-iso-week-header :inherit 'font-lock-function-name-face))))

(defun my/apply-set-face-rules ()
  "Apply all face settings declared in `my/set-face-rules`."
  (dolist (group (my/set-face-rules))
    (let ((feature (car group))
          (faces (cdr group)))
      (with-eval-after-load feature
        (dolist (face-spec faces)
          (let* ((face (car face-spec))
                 (props (cdr face-spec))
                 (force-inherit (plist-get props :force-inherit))
                 (clean-props (cl-loop for (key val) on props by #'cddr
                                       unless (eq key :force-inherit)
                                       collect key
                                       and collect val)))
            (when force-inherit
              (set-face-attribute face nil
                                  :weight 'unspecified
                                  :foreground 'unspecified
                                  :background 'unspecified
                                  :slant 'unspecified
                                  :underline 'unspecified
                                  :inverse-video 'unspecified
                                  :inherit force-inherit))
            (apply #'set-face-attribute face nil
                   (mapcar (lambda (x)
			                 (if (functionp x)
				                 (funcall x)
			                   (eval x)))
			               clean-props))))))))

(defvar my/face-remap-rules
  '((org
     (secondary-selection :background my/background))
    (org-agenda
     (hl-line :background 'unspecified :weight 'bold :slant 'unspecified))
    (deft
     (hl-line :background 'unspecified :weight 'bold :slant 'unspecified))
    (eww
     (variable-pitch :foreground my/yellow))
    (ein:notebook
     (header-line :foreground my/background :background my/secondary))))

(defun my/apply-face-remap-rules ()
  (dolist (group my/face-remap-rules)
    (let ((feature (car group))
          (mode-hook (intern (concat (symbol-name (car group)) "-mode-hook")))
          (faces (cdr group)))
      (with-eval-after-load feature
        (add-hook mode-hook
                  (lambda ()
                    (dolist (face-spec faces)
                      (apply #'face-remap-add-relative (car face-spec)
                             (mapcan (lambda (pair)
                                       (list (car pair)
                                             (let ((val (cdr pair)))
                                               (if (functionp val) (funcall val) val))))
                                     (seq-partition (cdr face-spec) 2))))
                    (when (assoc 'hl-line faces)
                      (hl-line-mode 1))))))))

;; Extra packages
(use-package dired-rainbow
  :config
  (defun my/set-dired-rainbow-faces ()
    (dolist (rule `((src ,my/brightmagenta ("el" "lisp" "sh" "r" "c" "h" "py"))
                    (txt ,my/brightmagenta ("txt" "org" "md"))
                    (doc ,my/blue ("docx" "docm"))
                    (xls ,my/green ("xlsx" "xlsm"))
                    (ppt ,my/brightred ("pptx" "pptm"))
                    (pdf ,my/red ("pdf"))))
      (eval `(dired-rainbow-define ,@rule)))))

(use-package smartrep
  :defer t
  :config
  (defun my/set-smartrep-faces ()
    (setq smartrep-mode-line-active-bg my/brightwhite)))

(use-package rainbow-mode)

(use-package transwin
  :after smartrep
  :config
  (smartrep-define-key global-map "C-w" '(("i" . transwin-inc)
                                          ("d" . transwin-dec))))

(defun my/apply-face-extra-packages ()
  (when (featurep 'dired-rainbow)
    (my/set-dired-rainbow-faces))
  (when (featurep 'smartrep)
    (my/set-smartrep-faces)))

(defun my/apply-font-emoji ()
  (set-fontset-font t 'emoji my/font-emoji nil 'prepend))

(defun my/setup-theme ()
  (my/load-theme)
  (when (eq my/theme-name 'rustcity)
    (my/apply-copy-face-rules)
    (my/apply-set-face-rules)
    (my/apply-face-remap-rules)
    (my/apply-face-extra-packages))
  (my/apply-font-emoji))

(my/setup-theme)

(defun my/toggle-theme ()
  "Interactively select theme and background variant."
  (interactive)
  (let ((theme (intern (completing-read
                        "Select theme: "
                        (mapcar #'symbol-name (custom-available-themes))
                        nil t) ))
        (background (intern (completing-read
                             "Select background: "
                             '("light" "dark")
                             nil t) )))
    (customize-save-variable 'my/theme-name theme)
    (customize-save-variable 'my/frame-background background)
    (my/setup-theme)))

(provide 'my-ui-face)
;;; my-ui-face.el ends here
