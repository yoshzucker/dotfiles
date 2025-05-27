;;; my-ui-face.el --- Theme and face configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Handles theme selection, face definitions, and color palette setup.

;;; Code:

(defgroup my/theme nil
  "Custom theme settings."
  :group 'appearance)

(defcustom my/theme-name 'nord
  "Which theme to use."
  :type 'symbol
  :options '(solarized nord my-tokyo my-rustcity)
  :group 'my/theme)

(defcustom my/frame-background 'dark
  "Which background to use."
  :type 'symbol
  :options '(light dark)
  :group 'my/theme)

;; Update from environment
(let ((term (not (display-graphic-p))))
  (when term
    (my/map-env my/theme-name "theme_name")
    (my/map-env my/frame-background "theme_variant")))

(use-package nord-theme
  :defer t
  :config
  (defconst my/nord-colors
    '((nord0  . "#2e3440") (nord1  . "#3b4252")
      (nord2  . "#434c5e") (nord3  . "#4c566a")
      (nord4  . "#d8dee9") (nord5  . "#e5e9f0")
      (nord6  . "#eceff4") (nord7  . "#8fbcbb")
      (nord8  . "#88c0d0") (nord9  . "#81a1c1")
      (nord10 . "#5e81ac") (nord11 . "#bf616a")
      (nord12 . "#d08770") (nord13 . "#ebcb8b")
      (nord14 . "#a3be8c") (nord15 . "#b48ead")))

  (defun my/nord-colors ()
    `((black         . ,(alist-get 'nord1 my/nord-colors))
      (red           . ,(alist-get 'nord11 my/nord-colors))
      (green         . ,(alist-get 'nord14 my/nord-colors))
      (yellow        . ,(alist-get 'nord13 my/nord-colors))
      (blue          . ,(alist-get 'nord9 my/nord-colors))
      (magenta       . ,(alist-get 'nord15 my/nord-colors))
      (cyan          . ,(alist-get 'nord7 my/nord-colors))
      (white         . ,(alist-get 'nord5 my/nord-colors))
      (brightblack   . ,(alist-get 'nord3 my/nord-colors))
      (brightred     . ,(alist-get 'nord11 my/nord-colors))
      (brightgreen   . ,(alist-get 'nord14 my/nord-colors))
      (brightyellow  . ,(alist-get 'nord12 my/nord-colors))
      (brightblue    . ,(alist-get 'nord10 my/nord-colors))
      (brightmagenta . ,(alist-get 'nord15 my/nord-colors))
      (brightcyan    . ,(alist-get 'nord7 my/nord-colors))
      (brightwhite   . ,(alist-get 'nord6 my/nord-colors))
      (foreground    . ,(alist-get 'nord4 my/nord-colors))
      (background    . ,(alist-get 'nord0 my/nord-colors)))))

(use-package solarized-theme
  :straight (:host github :repo "sellout/emacs-color-theme-solarized" :branch "master" :files ("*.el" "out"))
  :defer t
  :init
  (setq solarized-termcolors 16)
  :config
  (defun my/get-solarized-color (name)
    (nth (cond ((and (not (display-graphic-p)) (eq solarized-termcolors 16)) 4)
               ((and (not (display-graphic-p)) (eq solarized-termcolors 256)) 3)
               (solarized-degrade 3)
               (solarized-broken-srgb 2)
               (t 1))
         (assoc name solarized-colors)))

  (defun my/solarized-colors ()
    `((black         . ,(my/get-solarized-color 'base02))
      (red           . ,(my/get-solarized-color 'red))
      (green         . ,(my/get-solarized-color 'green))
      (yellow        . ,(my/get-solarized-color 'yellow))
      (blue          . ,(my/get-solarized-color 'blue))
      (magenta       . ,(my/get-solarized-color 'magenta))
      (cyan          . ,(my/get-solarized-color 'cyan))
      (white         . ,(my/get-solarized-color 'base2))
      (brightblack   . ,(my/get-solarized-color 'base03))
      (brightred     . ,(my/get-solarized-color 'orange))
      (brightgreen   . ,(my/get-solarized-color 'base01))
      (brightyellow  . ,(my/get-solarized-color 'base00))
      (brightblue    . ,(my/get-solarized-color 'base0))
      (brightmagenta . ,(my/get-solarized-color 'violet))
      (brightcyan    . ,(my/get-solarized-color 'base1))
      (brightwhite   . ,(my/get-solarized-color 'base3))
      (foreground    . ,(if (eq my/frame-background 'light)
                            (my/get-solarized-color 'base01)
                          (my/get-solarized-color 'base2)))
      (background    . ,(if (eq my/frame-background 'light)
                            (my/get-solarized-color 'base3)
                          (my/get-solarized-color 'base03))))))

(use-package my-tokyo-theme
  :straight nil
  :load-path "themes"
  :defer t)

(use-package my-rustcity-theme
  :straight nil
  :load-path "themes"
  :defer t)

(defun my/semantic-colors ()
  "Return semantic color mapping based on theme and background."
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
       (primary . green)
       (secondary . magenta)
       (accent . brightyellow)))
    ('(solarized dark)
     '((fgdim . brightyellow)
       (bghighlight . brightgreen)
       (primary . blue)
       (secondary . magenta)
       (accent . yellow)))
    ('(my-tokyo light)
     '((fgdim . brightblack)
       (bghighlight . brightwhite)
       (primary . blue)
       (secondary . magenta)
       (accent . yellow)))
    ('(my-tokyo dark)
     '((fgdim . brightwhite)
       (bghighlight . brightblack)
       (primary . blue)
       (secondary . magenta)
       (accent . yellow)))
    (_
     '((fgdim . brightwhite)
       (bghighlight . brightblack)
       (primary . blue)
       (secondary . magenta)
       (accent . yellow)))))

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
    ('nord      (load-theme 'nord t)
                (my/set-colors (my/nord-colors)))
    ('solarized (load-theme 'solarized t)
                (enable-theme 'solarized)
                (my/set-colors (my/solarized-colors)))
    ('my-tokyo  (load-theme 'my-tokyo t)
                (my/set-colors (my/tokyo-colors)))
    ('my-rustcity  (load-theme 'my-rustcity t)
                (my/set-colors (my/rustcity-colors)))
    (_          (message "Unknown theme: %s" my/theme-name))))

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

;; Extra packages
(use-package dired-rainbow
  :config
  (defun my/set-dired-rainbow-faces ()
    (dolist (rule `((src ,my/magenta ("el" "lisp" "sh" "R" "c" "h" "py" "org"))
                    (doc ,my/blue ("docx" "docm"))
                    (xls ,my/green ("xlsx" "xlsm"))
                    (ppt ,my/brightyellow ("pptx" "pptm"))
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

(defun my/apply-extra-packages-faces ()
  (when (featurep 'dired-rainbow)
    (my/set-dired-rainbow-faces))
  (when (featurep 'smartrep)
    (my/set-smartrep-faces)))

(defun my/setup-theme ()
  (my/load-theme)
  (my/apply-copy-face-rules)
  (my/apply-set-face-rules)
  (my/apply-face-remap-rules)
  (my/apply-extra-packages-faces))

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
