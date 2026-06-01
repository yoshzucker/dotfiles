;;; my-ui-face.el --- Theme selection and user environment configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; - Support for arbitrary themes via my/toggle-theme.
;; - User fonts, emoji, and environment-driven theme + background selection.
;; - Personal workflow faces (custom org keywords, clock indicators, etc.).
;; - Purpose-built generic helpers for packages with non-standard face needs:
;;     wdired (transient background remap via advice), dired-rainbow (define
;;     macro), smartrep (mode-line variable).  The helpers themselves are
;;     theme-agnostic; rustcity-theme's :config block (inside this file) calls
;;     them with rustcity-appropriate colors to reproduce the classic behavior.
;;
;; De facto division:
;;   Themes (including rustcity-theme) own visual identity and faces for popular
;;   packages.  This file owns user prefs, font settings, personal workflow
;;   extensions, and the small generic helpers.

;;; Code:

(require 'cl-lib)

(defgroup my/theme nil
  "Custom theme settings."
  :group 'my/ui)

(defcustom my/theme-name 'rustcity
  "Which theme to use.
Any theme available via `custom-available-themes' can be used
(via `M-x my/toggle-theme'). The listed options are just common choices."
  :type 'symbol
  :options '(nord rustcity)
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

(defun my/set-faces (specs)
  "Set faces from SPECS, a list of (FACE . PLIST) forms.
This is a generic helper.  The caller is responsible for resolving
any colors and building the property lists.

Example:
  (my/set-faces
   \\='((my/org-ongo :inverse-video t :foreground \"#ff0000\")
     (my/mode-line-over :foreground \"#000000\" :background \"#ff0000\")))"
  (dolist (spec specs)
    (apply #'set-face-attribute (car spec) nil (cdr spec))))

(use-package rustcity-theme
  :straight (:host github :repo "yoshzucker/rustcity-theme")
  :defer t
  :config
  (let* ((colors (rustcity-colors))
         (brightmagenta (alist-get 'brightmagenta colors))
         (blue          (alist-get 'blue          colors))
         (green         (alist-get 'green         colors))
         (brightred     (alist-get 'brightred     colors))
         (red           (alist-get 'red           colors))
         (cyan          (alist-get 'cyan          colors))
         (brightwhite   (alist-get 'brightwhite   colors))
         (background    (alist-get 'background    colors))
         ;; For wdired edit background we pick reasonable values from the
         ;; rustcity palette (light variant uses a light tone, dark uses
         ;; a dark tone).  These replace the former my/white / my/black usage.
         (wdired-light  (alist-get 'brightwhite colors))
         (wdired-dark   (alist-get 'black colors)))
    (my/set-dired-rainbow-faces
     `((("el" "lisp" "sh" "r" "c" "h" "py") . ,brightmagenta)
       (("txt" "org" "md")                  . ,brightmagenta)
       (("docx" "docm")                     . ,blue)
       (("xlsx" "xlsm")                     . ,green)
       (("pptx" "pptm")                     . ,brightred)
       (("pdf")                             . ,red)))
    (my/set-smartrep-active-background brightwhite)
    (my/set-wdired-edit-background :light wdired-light :dark wdired-dark)
    (my/set-faces
     `((my/org-ongo :inverse-video t :foreground ,brightred :background ,background)
       (my/org-wait :inverse-video t :inherit font-lock-comment-face)
       (my/mode-line-over :foreground ,background :background ,red)
       (my/mode-line-under :foreground ,background :background ,cyan)
       (my/calendar-iso-week-header :inherit font-lock-function-name-face)))))

(use-package nord-theme
  :defer t)

;; -------------------------------------------------------------------
;; Special package helpers (wdired, dired-rainbow, smartrep)
;;
;; Purpose-built helpers for packages whose "face" configuration is not a
;; plain `set-face-attribute'.
;; -------------------------------------------------------------------

(defvar my/wdired-edit-backgrounds nil
  "Plist (:light COLOR :dark COLOR) for wdired edit-mode background.
Set via `my/set-wdired-edit-background'.")

(defvar-local my/wdired--bg-cookie nil
  "Cookie returned by `face-remap-add-relative' for the wdired bg override.")

(defun my/set-dired-rainbow-faces (rules)
  "Define dired-rainbow faces.
RULES: list of ((exts...) . color) or (name color (exts...))."
  (when (featurep 'dired-rainbow)
    (let ((i 0))
      (dolist (rule rules)
        (let (name color exts)
          (cond
           ((and (consp rule) (consp (car rule)))
            (setq exts  (car rule)
                  color (cdr rule)
                  name  (intern (format "my-dired-rainbow-%04d" (cl-incf i)))))
           ((and (consp rule) (cdr rule))
            (setq name  (car rule)
                  color (cadr rule)
                  exts  (if (consp (cddr rule)) (caddr rule) (cddr rule)))))
          (when (and name color exts)
            (eval `(dired-rainbow-define ,name ,color ,exts))))))))

(defun my/set-smartrep-active-background (color)
  "Set `smartrep-mode-line-active-bg' to COLOR (a concrete color string)."
  (setq smartrep-mode-line-active-bg color))

(defun my/wdired--apply-edit-background (&rest _)
  "Advice (after): temporarily remap `default' background in wdired buffers."
  (when my/wdired-edit-backgrounds
    (let ((bg (if (eq (bound-and-true-p frame-background-mode) 'light)
                  (plist-get my/wdired-edit-backgrounds :light)
                (plist-get my/wdired-edit-backgrounds :dark))))
      (when (and bg (not (string= bg "")))
        (setq my/wdired--bg-cookie
              (face-remap-add-relative 'default :background bg))))))

(defun my/wdired--restore-background (&rest _)
  "Advice (after): remove the wdired edit-mode background remap."
  (when my/wdired--bg-cookie
    (face-remap-remove-relative my/wdired--bg-cookie)
    (setq my/wdired--bg-cookie nil)))

(defun my/set-wdired-edit-background (&rest plist)
  "Configure the background shown while editing filenames in wdired.
PLIST is e.g. (:light \"#f0f0f0\" :dark \"#222222\").
COLOR values are concrete color strings.

Call this from user configuration (use-package :config etc.).
The advices are managed idempotently so re-calling after a theme toggle
does not leak duplicate advice entries.

This replaces the old rustcity-only logic that lived in my-files-ops.el."
  (setq my/wdired-edit-backgrounds plist)
  (my/apply-wdired-background))

(defun my/apply-wdired-background ()
  "Install/refresh wdired background-remap advices from stored config.
Uses per-buffer cookie tracking for correct cleanup on mode exit.
Mainly for advanced re-application; normal activation is from rustcity-theme :config.")
(advice-remove 'wdired-change-to-wdired-mode #'my/wdired--apply-edit-background)
(advice-remove 'wdired-change-to-dired-mode #'my/wdired--restore-background)
(when my/wdired-edit-backgrounds
  (advice-add 'wdired-change-to-wdired-mode :after #'my/wdired--apply-edit-background)
  (advice-add 'wdired-change-to-dired-mode :after #'my/wdired--restore-background))

(use-package rainbow-mode)

(use-package transwin
  :after smartrep
  :config
  (smartrep-define-key global-map "C-w" '(("i" . transwin-inc)
                                          ("d" . transwin-dec))))

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
  ;; Generic theme loading (no more base18 provider switching).
  (mapc #'disable-theme custom-enabled-themes)
  (setq frame-background-mode my/frame-background)
  (load-theme my/theme-name t)

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
