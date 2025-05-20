;;; my-ui-theme.el --- Theme and face configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles theme selection, face definitions, and color palette setup.

;;; Code:

(defcustom my/theme-name 'nord
  "Which theme to use."
  :options '(solarized nord gruvbox)
  :type 'symbol)

(defcustom my/frame-background 'dark
  "Which background to use."
  :options '(light dark)
  :type 'symbol)

;; Update from environment
(let ((term (not (display-graphic-p))))
  (when term
    (my/map-env my/theme-name "theme_name")
    (my/map-env my/frame-background "theme_variant")))

(setq frame-background-mode my/frame-background)

(use-package nord-theme
  :defer t
  :config
  (defconst nord-colors
    '((nord0  "#2e3440") (nord1  "#3b4252") (nord2  "#434c5e") (nord3  "#4c566a")
      (nord4  "#d8dee9") (nord5  "#e5e9f0") (nord6  "#eceff4") (nord7  "#8fbcbb")
      (nord8  "#88c0d0") (nord9  "#81a1c1") (nord10 "#5e81ac") (nord11 "#bf616a")
      (nord12 "#d08770") (nord13 "#ebcb8b") (nord14 "#a3be8c") (nord15 "#b48ead")))
  (defvar my/nord-colors
    `((black         . ,(cdr (assoc 'nord1 nord-colors)))
      (red           . ,(cdr (assoc 'nord11 nord-colors)))
      (green         . ,(cdr (assoc 'nord14 nord-colors)))
      (yellow        . ,(cdr (assoc 'nord13 nord-colors)))
      (blue          . ,(cdr (assoc 'nord9 nord-colors)))
      (magenta       . ,(cdr (assoc 'nord15 nord-colors)))
      (cyan          . ,(cdr (assoc 'nord7 nord-colors)))
      (white         . ,(cdr (assoc 'nord5 nord-colors)))
      (brightblack   . ,(cdr (assoc 'nord3 nord-colors)))
      (brightred     . ,(cdr (assoc 'nord11 nord-colors)))
      (brightgreen   . ,(cdr (assoc 'nord14 nord-colors)))
      (brightyellow  . ,(cdr (assoc 'nord12 nord-colors)))
      (brightblue    . ,(cdr (assoc 'nord10 nord-colors)))
      (brightmagenta . ,(cdr (assoc 'nord15 nord-colors)))
      (brightcyan    . ,(cdr (assoc 'nord7 nord-colors)))
      (brightwhite   . ,(cdr (assoc 'nord6 nord-colors)))
      (foreground    . ,(cdr (assoc 'nord4 nord-colors)))
      (background    . ,(cdr (assoc 'nord0 nord-colors))))))

(use-package solarized-theme
  :straight (:host github :repo "sellout/emacs-color-theme-solarized" :branch "master" :files ("*.el" "out"))
  :defer t
  :init
  (setq solarized-termcolors 16)
  :config
  (defun solarized-color (name)
    (nth (cond ((and (not (display-graphic-p)) (eq solarized-termcolors 16)) 4)
               ((and (not (display-graphic-p)) (eq solarized-termcolors 256)) 3)
               (solarized-degrade 3)
               (solarized-broken-srgb 2)
               (t 1))
         (assoc name solarized-colors)))
  (defvar my/solarized-colors
    `((black         . ,(solarized-color 'base02))
      (red           . ,(solarized-color 'red))
      (green         . ,(solarized-color 'green))
      (yellow        . ,(solarized-color 'yellow))
      (blue          . ,(solarized-color 'blue))
      (magenta       . ,(solarized-color 'magenta))
      (cyan          . ,(solarized-color 'cyan))
      (white         . ,(solarized-color 'base2))
      (brightblack   . ,(solarized-color 'base03))
      (brightred     . ,(solarized-color 'orange))
      (brightgreen   . ,(solarized-color 'base01))
      (brightyellow  . ,(solarized-color 'base00))
      (brightblue    . ,(solarized-color 'base0))
      (brightmagenta . ,(solarized-color 'violet))
      (brightcyan    . ,(solarized-color 'base1))
      (brightwhite   . ,(solarized-color 'base3))
      (foreground    . ,(if (eq my/frame-background 'light)
                            (solarized-color 'base01)
                          (solarized-color 'base2)))
      (background    . ,(if (eq my/frame-background 'light)
                            (solarized-color 'base3)
                          (solarized-color 'base03))))))

(use-package gruvbox-theme
  :defer t
  :config
  (defconst gruvbox-colors
    '((gruvbox-dark0 "#282828") (gruvbox-light0 "#fbf1c7")
      (gruvbox-dark1 "#3c3836") (gruvbox-light1 "#ebdbb2")
      (gruvbox-gray  "#928374")
      (gruvbox-neutral_red "#cc241d") (gruvbox-bright_red "#fb4934")
      (gruvbox-neutral_green "#98971a") (gruvbox-bright_green "#b8bb26")
      (gruvbox-neutral_yellow "#d79921") (gruvbox-bright_yellow "#fabd2f")
      (gruvbox-neutral_blue "#458588") (gruvbox-bright_blue "#83a598")
      (gruvbox-neutral_purple "#b16286") (gruvbox-bright_purple "#d3869b")
      (gruvbox-neutral_aqua "#689d6a") (gruvbox-bright_aqua "#8ec07c")))
  (defvar my/gruvbox-colors
    (if (eq my/frame-background 'light)
        `((black         . ,(cdr (assoc 'gruvbox-light0 gruvbox-colors)))
          (red           . ,(cdr (assoc 'gruvbox-neutral_red gruvbox-colors)))
          (green         . ,(cdr (assoc 'gruvbox-neutral_green gruvbox-colors)))
          (yellow        . ,(cdr (assoc 'gruvbox-neutral_yellow gruvbox-colors)))
          (blue          . ,(cdr (assoc 'gruvbox-neutral_blue gruvbox-colors)))
          (magenta       . ,(cdr (assoc 'gruvbox-neutral_purple gruvbox-colors)))
          (cyan          . ,(cdr (assoc 'gruvbox-neutral_aqua gruvbox-colors)))
          (white         . ,(cdr (assoc 'gruvbox-dark1 gruvbox-colors)))
          (brightblack   . ,(cdr (assoc 'gruvbox-gray gruvbox-colors)))
          (brightred     . ,(cdr (assoc 'gruvbox-neutral_red gruvbox-colors)))
          (brightgreen   . ,(cdr (assoc 'gruvbox-neutral_green gruvbox-colors)))
          (brightyellow  . ,(cdr (assoc 'gruvbox-neutral_yellow gruvbox-colors)))
          (brightblue    . ,(cdr (assoc 'gruvbox-neutral_blue gruvbox-colors)))
          (brightmagenta . ,(cdr (assoc 'gruvbox-neutral_purple gruvbox-colors)))
          (brightcyan    . ,(cdr (assoc 'gruvbox-neutral_aqua gruvbox-colors)))
          (brightwhite   . ,(cdr (assoc 'gruvbox-light1 gruvbox-colors)))
          (foreground    . ,(cdr (assoc 'gruvbox-dark1 gruvbox-colors)))
          (background    . ,(cdr (assoc 'gruvbox-light0 gruvbox-colors))))
      `((black         . ,(cdr (assoc 'gruvbox-dark0 gruvbox-colors)))
        (red           . ,(cdr (assoc 'gruvbox-neutral_red gruvbox-colors)))
        (green         . ,(cdr (assoc 'gruvbox-neutral_green gruvbox-colors)))
        (yellow        . ,(cdr (assoc 'gruvbox-neutral_yellow gruvbox-colors)))
        (blue          . ,(cdr (assoc 'gruvbox-neutral_blue gruvbox-colors)))
        (magenta       . ,(cdr (assoc 'gruvbox-neutral_purple gruvbox-colors)))
        (cyan          . ,(cdr (assoc 'gruvbox-neutral_aqua gruvbox-colors)))
        (white         . ,(cdr (assoc 'gruvbox-light1 gruvbox-colors)))
        (brightblack   . ,(cdr (assoc 'gruvbox-gray gruvbox-colors)))
        (brightred     . ,(cdr (assoc 'gruvbox-bright_red gruvbox-colors)))
        (brightgreen   . ,(cdr (assoc 'gruvbox-bright_green gruvbox-colors)))
        (brightyellow  . ,(cdr (assoc 'gruvbox-bright_yellow gruvbox-colors)))
        (brightblue    . ,(cdr (assoc 'gruvbox-bright_blue gruvbox-colors)))
        (brightmagenta . ,(cdr (assoc 'gruvbox-bright_purple gruvbox-colors)))
        (brightcyan    . ,(cdr (assoc 'gruvbox-bright_aqua gruvbox-colors)))
        (brightwhite   . ,(cdr (assoc 'gruvbox-light4 gruvbox-colors)))
        (foreground    . ,(cdr (assoc 'gruvbox-light1 gruvbox-colors)))
        (background    . ,(cdr (assoc 'gruvbox-dark0 gruvbox-colors)))))))

(defcustom my/semantic-colors nil
  "Semantic color mapping (to be set at runtime)."
  :type '(alist :key-type symbol :value-type string))

(setq my/semantic-colors
      (pcase (list my/theme-name my/frame-background)
        ('(nord dark)
         '((fgdim . brightblack)
           (bghighlight . black)
           (primary . blue)
           (secondary . magenta)
           (accent . brightyellow)))
        ('(solarized light)
         '((fgdim . brightblack)
           (bghighlight . black)
           (primary . blue)
           (secondary . magenta)
           (accent . brightyellow)))
        ('(solarized dark)
         '((fgdim . brightyellow)
           (bghighlight . brightgreen)
           (primary . blue)
           (secondary . magenta)
           (accent . yellow)))
        ('(gruvbox light)
         '((fgdim . brightblack)
           (bghighlight . brightwhite)
           (primary . blue)
           (secondary . magenta)
           (accent . yellow)))
        ('(gruvbox dark)
         '((fgdim . brightwhite)
           (bghighlight . brightblack)
           (primary . blue)
           (secondary . magenta)
           (accent . yellow)))
        (_ nil)))

;; Color assignment
(defun my/set-colors (palette)
  ;; base palette
  (dolist (entry palette)
    (set (intern (format "my/%s" (car entry))) (cadr entry)))
  ;; extra aliases or literals
  (dolist (entry my/semantic-colors)
    (let ((target (intern (format "my/%s" (car entry))))
          (value (cdr entry)))
      (set target
           (if (symbolp value)
               (symbol-value (intern (format "my/%s" value)))
             value)))))

;; Load theme and apply color palette
(defun my/load-theme ()
  "Load the theme specified in `my/theme-name` and assign colors."
  (pcase my/theme-name
    ('nord (progn (load-theme 'nord t) (my/set-colors my/nord-colors)))
    ('solarized (progn (load-theme 'solarized t) (my/set-colors my/solarized-colors)))
    ('gruvbox (progn (load-theme 'gruvbox t) (my/set-colors my/gruvbox-colors)))
    (_ (message "Unknown theme: %s" my/theme-name))))

;; Face rules application
(defvar my/copy-face-rules
  '((org
     (org-todo . org-ongo)
     (org-todo . org-wait)
     (org-priority . org-priority-a)
     (org-priority . org-priority-b)
     (org-mode-line-clock . org-mode-line-clock-underrun))
    (calendar
     (calendar-weekday-header . calendar-iso-week-header))))

(defun my/apply-copy-face-rules ()
  (dolist (group my/copy-face-rules)
    (let ((feature (car group))
          (face-pairs (cdr group)))
      (with-eval-after-load feature
        (dolist (pair face-pairs)
          (copy-face (car pair) (cdr pair)))))))

(defvar my/set-face-rules
  `((font-lock
     (default :family ,(if (eq system-type 'darwin) "HackGen Console" "HackGen Console NF")
              :height ,(if (eq system-type 'darwin) 150 120))
     (variable-pitch :family ,(if (eq system-type 'darwin) "HackGen" "HackGen NF"))
     (fringe :background 'unspecified)
     (border :foreground 'unspecified)
     (vertical-border :foreground my/bghighlight)
     (internal-border :background 'unspecified)
     (underline :underline nil)
     (warning :weight 'unspecified)
     (mode-line :foreground my/background :background my/primary
		        :underline 'unspecified :inverse-video nil)
     (mode-line-inactive :foreground my/primary :background my/bghighlight
			             :underline 'unspecified :inverse-video nil)
     (mode-line-buffer-id :weight 'unspecified)
     (minibuffer-prompt :weight 'unspecified :foreground my/primary)
     (cursor :background my/primary)
     (header-line :foreground my/background :background my/primary))
    (font-lock
     (font-lock-builtin-face :foreground my/red))
    (tab-bar
     (tab-bar :foreground my/foreground :background my/background)
     (tab-bar-tab :foreground my/background :background my/primary :box 'unspecified)
     (tab-bar-tab-inactive :foreground my/primary :background my/bghighlight))
    (evil-snipe
     (evil-snipe-first-match-face :background my/bghighlight))
    (tooltip
     (tooltip :foreground my/bghighlight
              :background (lambda () (face-attribute 'highlight :background))
              :inherit nil))
    (compile
     (compilation-info :weight 'unspecified)
     (compilation-line-number :foreground my/primary)
     (compilation-warning :foreground my/red)
     (compilation-mode-line-fail :foreground my/red)
     (compilation-mode-line-exit :weight 'unspecified :foreground my/cyan))
    (orderless
     (orderless-match-face-0 :foreground my/brightyellow)
     (orderless-match-face-1 :foreground my/brightred)
     (orderless-match-face-2 :foreground my/green)
     (orderless-match-face-3 :foreground my/magenta))
    (corfu
     (corfu-default :background my/background)
     (corfu-current :foreground my/primary :background my/bghighlight)
     (corfu-bar :background my/brightyellow))
    (company
     (company-tooltip-selection :foreground my/cyan)
     (company-tooltip-common-selection :foreground my/secondary))
    (deadgrep
     (deadgrep-filename-face :weight 'normal :foreground my/yellow)
     (deadgrep-meta-face :foreground my/blue))
    (avy
     (avy-background-face :foreground my/background)
     (avy-goto-char-timer-face :foreground my/background :background my/blue)
     (avy-lead-face :foreground my/background :background my/yellow)
     (avy-lead-face-0 :foreground my/background :background my/brightmagenta)
     (avy-lead-face-1 :foreground my/background :background my/blue)
     (avy-lead-face-2 :foreground my/background :background my/red))
    (magit
     (magit-section-heading :background my/background))
    (eglot
     (eglot-mode-line :weight 'unspecified))
    (ein-cell
     (ein:cell-input-area :background my/bghighlight)
     (ein:cell-input-prompt :foreground my/background :background my/primary)
     (ein:cell-output-prompt :foreground my/background :background my/secondary))
    (eww
     (eww-valid-certificate :weight 'unspecified :foreground my/green))
    (dired-subtree
     (dired-subtree-depth-1-face :background 'unspecified)
     (dired-subtree-depth-2-face :background 'unspecified)
     (dired-subtree-depth-3-face :background 'unspecified)
     (dired-subtree-depth-4-face :background 'unspecified)
     (dired-subtree-depth-5-face :background 'unspecified)
     (dired-subtree-depth-6-face :background 'unspecified))
    (bookmark
     (bookmark-face :foreground my/blue :background 'unspecified))
    (org
     (org-level-1 :weight 'unspecified)
     (org-level-2 :weight 'unspecified)
     (org-level-3 :weight 'unspecified)
     (org-headline-done :foreground 'unspecified)
     (org-agenda-dimmed-todo-face :inverse-video 'unspecified
                                  :foreground my/fgdim :background my/background)
     (org-todo :weight 'unspecified :inverse-video t 
               :foreground my/red :background my/background)
     (org-ongo :weight 'unspecified :inverse-video t
               :foreground my/brightyellow :background my/background)
     (org-wait :weight 'unspecified :inverse-video t
               :foreground (lambda () (face-attribute 'org-agenda-dimmed-todo-face :foreground))
               :background (lambda () (face-attribute 'org-agenda-dimmed-todo-face :background)))
     (org-done :weight 'unspecified :inverse-video t)
     (org-column :inverse-video nil)
     (org-tag :weight 'unspecified)
     (org-archived :weight 'normal
                   :foreground (lambda () (face-attribute 'org-headline-done :foreground) ))
     (org-drawer :foreground 'unspecified :weight 'unspecified :inherit 'org-special-keyword)
     (org-column :slant 'unspecified :weight 'unspecified :foreground 'unspecified :inverse-video 'unspecified)
     (org-priority-a :foreground my/blue)
     (org-priority-b :foreground my/cyan)
     (org-mode-line-clock :foreground my/cyan)
     (org-mode-line-clock-overrun :foreground my/red :background 'unspecified)
     (org-link :foreground my/magenta)
     (org-scheduled :weight 'unspecified :slant 'unspecified)
     (org-scheduled-today :weight 'unspecified :slant 'unspecified)
     (org-scheduled-previously :weight 'unspecified :foreground (lambda () (face-attribute 'org-time-grid :foreground)))
     (org-warning :weight 'unspecified :foreground my/red)
     (org-upcoming-deadline :weight 'unspecified)
     (org-agenda-structure :weight 'unspecified)
     (org-time-grid :foreground my/blue)
     (org-agenda-current-time :foreground my/brightyellow)
     (org-agenda-date-today :weight 'unspecified :foreground my/secondary)
     (org-agenda-date-weekend :weight 'unspecified :foreground my/secondary)
     (org-agenda-clocking :slant 'italic :foreground 'unspecified :inherit 'unspecified))
    (org-habit
     (org-habit-overdue-face :background my/brightmagenta))
    (org-pomodoro
     (org-pomodoro-mode-line :foreground my/red))
    (org-timeline
     (org-timeline-elapsed :foreground my/brightcyan :background my/white
                           :inherit 'unspecified)
     (org-timeline-block :background my/yellow)
     (org-timeline-clocked :background my/cyan))
    (org-roam
     (org-roam-header-line :foreground my/background))
    (org-noter
     (org-noter-notes-exist-face :foreground my/green)
     (org-noter-no-notes-exist-face :foreground my/brightred))
    (deft
     (deft-header-face :weight 'unspecified :foreground my/secondary)
     (deft-title-face :weight 'unspecified))
    (calendar
     (calendar-today :foreground my/secondary :underline 'unspecified)
     (calendar-weekend-header :foreground my/secondary)
     (calendar-iso-week-header :inherit 'font-lock-function-name-face))))

(defun my/apply-set-face-rules ()
  "Apply all face settings declared in `my/set-face-rules`."
  (dolist (group my/set-face-rules)
    (let ((feature (car group))
          (faces (cdr group)))
      (with-eval-after-load feature
        (dolist (face-spec faces)
          (let ((face (car face-spec))
                (props (cdr face-spec)))
            (apply #'set-face-attribute face nil
                   (mapcar (lambda (x)
			     (if (functionp x)
				 (funcall x)
			       (eval x)))
			   props))))))))

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

(defun my/theme-setup ()
  (my/load-theme)
  (my/apply-copy-face-rules)
  (my/apply-set-face-rules)
  (my/apply-face-remap-rules))

(my/theme-setup)

;; Extra packages
(use-package rainbow-mode)

(use-package dired-rainbow
  :config
  (dolist (rule `((src ,my/magenta ("el" "lisp" "sh" "R" "c" "h" "py" "org"))
                  (doc ,my/blue ("docx" "docm"))
                  (xls ,my/green ("xlsx" "xlsm"))
                  (ppt ,my/brightyellow ("pptx" "pptm"))
                  (pdf ,my/red ("pdf"))))
    (eval `(dired-rainbow-define ,@rule))))

(use-package smartrep
  :defer t
  :config
  ;; Highlight mode-line when smartrep is active
  (setq smartrep-mode-line-active-bg my/brightwhite))

(use-package transwin
  :after smartrep
  :config
  (smartrep-define-key global-map "C-w" '(("i" . transwin-inc)
                                          ("d" . transwin-dec))))

(provide 'my-ui-theme)
;;; my-ui-theme.el ends here
