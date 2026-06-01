;;; my-ui-face.el --- Theme selection and user environment configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; - Multi-theme support (nord / solarized / rustcity) with toggle.
;; - User fonts, emoji, and environment-driven theme selection.
;; - Personal workflow faces (custom org keywords, clock indicators, etc.).
;; - Package-specific tweaks (dired-rainbow, smartrep).
;;
;; De facto division:
;;   rustcity-theme package owns the visual identity and faces for popular packages.
;;   This file owns user prefs + personal workflow extensions.

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

;; Load theme (rustcity now owns the majority of its face rules inside the package)
(defun my/load-theme ()
  "Load the theme specified in my/theme-name."
  (mapc #'disable-theme custom-enabled-themes)
  (setq frame-background-mode my/frame-background)
  (pcase my/theme-name
    ('nord      (load-theme 'nord t))
    ('solarized (load-theme 'solarized t)
                (enable-theme 'solarized))
    ('rustcity  (load-theme 'rustcity t))
    (_ (message "Unknown theme: %s" my/theme-name))))

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

;; -------------------------------------------------------------------
;; Personal workflow faces.
;; These belong to the user's org + UI conventions (not the theme).
;; Referenced from my-app-org.el.
;;
;; Defined with defface so they always exist (no "Invalid face" errors
;; on any theme). Rustcity-specific colors are applied at setup time.
;; -------------------------------------------------------------------

(defface my/org-ongo
  '((t (:inverse-video t)))
  "Face for the ONGO todo keyword (rustcity personal workflow).")

(defface my/org-wait
  '((t (:inverse-video t :inherit font-lock-comment-face)))
  "Face for the WAIT todo keyword (rustcity personal workflow).")

(defface my/mode-line-over
  '((t ()))
  "Face for clock string in mode-line/tab-bar when task is overrunning.")

(defface my/mode-line-under
  '((t ()))
  "Face for clock string in mode-line/tab-bar under normal conditions.")

(defface my/calendar-iso-week-header
  '((t (:inherit font-lock-function-name-face)))
  "Face for ISO week numbers shown in the calendar buffer.")

(defun my/apply-rustcity-workflow-faces ()
  "Customize the personal workflow faces with rustcity palette colors."
  (when (eq my/theme-name 'rustcity)
    (when (fboundp 'rustcity-colors)
      (let* ((colors    (rustcity-colors))
             (red       (alist-get 'red colors))
             (cyan      (alist-get 'cyan colors))
             (brightred (alist-get 'brightred colors))
             (background (alist-get 'background colors)))
        (set-face-attribute 'my/org-ongo nil
                            :inverse-video t :foreground brightred :background background)
        (set-face-attribute 'my/org-wait nil
                            :inverse-video t :inherit 'font-lock-comment-face)
        (set-face-attribute 'my/mode-line-over nil
                            :foreground background :background red)
        (set-face-attribute 'my/mode-line-under nil
                            :foreground background :background cyan)
        (set-face-attribute 'my/calendar-iso-week-header nil
                            :inherit 'font-lock-function-name-face)))))

;; Extra packages (package-specific color config)
;; For rustcity we pull colors from the theme's exported palette.
(use-package dired-rainbow
  :config
  (defun my/set-dired-rainbow-faces ()
    (let* ((colors (and (eq my/theme-name 'rustcity)
                        (fboundp 'rustcity-colors)
                        (rustcity-colors)))
           (brightmagenta (or (alist-get 'brightmagenta colors) "#b48ead"))
           (blue          (or (alist-get 'blue          colors) "#81a1c1"))
           (green         (or (alist-get 'green         colors) "#a3be8c"))
           (brightred     (or (alist-get 'brightred     colors) "#d08770"))
           (red           (or (alist-get 'red           colors) "#bf616a")))
      (dolist (rule `((src ,brightmagenta ("el" "lisp" "sh" "r" "c" "h" "py"))
                      (txt ,brightmagenta ("txt" "org" "md"))
                      (doc ,blue          ("docx" "docm"))
                      (xls ,green         ("xlsx" "xlsm"))
                      (ppt ,brightred     ("pptx" "pptm"))
                      (pdf ,red           ("pdf"))))
        (eval `(dired-rainbow-define ,@rule))))))

(use-package smartrep
  :defer t
  :config
  (defun my/set-smartrep-faces ()
    (let* ((colors (and (eq my/theme-name 'rustcity)
                        (fboundp 'rustcity-colors)
                        (rustcity-colors)))
           (brightwhite (or (alist-get 'brightwhite colors) "#eceff4")))
      (setq smartrep-mode-line-active-bg brightwhite))))

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

(defun my/apply-user-fonts (&optional _frame)
  "Apply user font preferences to the default, fixed-pitch and variable-pitch faces.
This is intentionally kept in dotfiles (not in the theme package) because
font choice is a user environment / preference concern."
  (set-face-attribute 'default nil
                      :family my/font-default
                      :height my/font-height)
  (set-face-attribute 'fixed-pitch nil :family my/font-default)
  (set-face-attribute 'variable-pitch nil :family my/font-variable))

(defun my/setup-theme ()
  (my/load-theme)
  (when (eq my/theme-name 'rustcity)
    (my/apply-rustcity-workflow-faces)
    (my/apply-face-extra-packages))
  (my/apply-user-fonts)
  (my/apply-font-emoji))

;; Re-apply fonts when new frames are created (important for emacs --daemon)
(add-hook 'after-make-frame-functions #'my/apply-user-fonts)

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
