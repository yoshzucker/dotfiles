;;; my-ui-face.el --- Theme selection and user environment configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Theme selection, user fonts, and personal workflow faces.
;; Generic helpers for special packages (dired-rainbow, smartrep, etc.)
;; are registered by each theme's use-package :config and applied from
;; my/setup-theme (including on my/toggle-theme).

;;; Code:

;; Customization and variables

(defgroup my/theme nil
  "Custom theme settings."
  :group 'my/ui)

(defcustom my/theme-name 'rustcity
  "Theme to use."
  :type 'symbol
  :options '(nord rustcity)
  :group 'my/ui)

(defcustom my/frame-background 'dark
  "Background variant."
  :type 'symbol
  :options '(light dark)
  :group 'my/ui)

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

(defvar my/theme-special-setups nil
  "Alist of (THEME-SYMBOL . FUNCTION) for applying theme-specific
special package configurations (dired-rainbow, smartrep, etc.).
Called from my/setup-theme (so my/toggle-theme also re-applies them).
Themes without an entry simply leave previous settings as-is.")

;; Faces

(defface my/org-ongo '((t (:inverse-video t)))
  "ONGO todo keyword face.")

(defface my/org-wait '((t (:inverse-video t :inherit font-lock-comment-face)))
  "WAIT todo keyword face.")

(defface my/mode-line-over '((t ()))
  "Overrunning clock face.")

(defface my/mode-line-under '((t ()))
  "Normal clock face.")

(defface my/calendar-iso-week-header '((t (:inherit font-lock-function-name-face)))
  "Calendar ISO week header face.")

(defface my/wdired-edit-face '((t nil))
  "Temporary face used while in wdired edit mode.")

;; General utilities

(defun my/set-faces (specs)
  "Set faces from SPECS, a list of (FACE . PLIST) forms."
  (dolist (spec specs)
    (apply #'set-face-attribute (car spec) nil (cdr spec))))

(defun my/apply-font-emoji ()
  (set-fontset-font t 'emoji my/font-emoji nil 'prepend))

(defun my/apply-user-fonts (&optional _frame)
  "Apply user font preferences."
  (set-face-attribute 'default nil
                      :family my/font-default
                      :height my/font-height)
  (set-face-attribute 'fixed-pitch nil :family my/font-default)
  (set-face-attribute 'variable-pitch nil :family my/font-variable))

;; Special package helpers

(defun my/set-dired-rainbow-faces (rules)
  "Define dired-rainbow faces.
RULES: list of ((exts...) . color) or (name color (exts...)).
Safe to call even if dired-rainbow is not yet loaded (guarded by featurep)."
  (when (featurep 'dired-rainbow)
    (let ((i 0))
      (dolist (rule rules)
        (let (name color exts)
          (cond
           ((and (consp rule) (consp (car rule)))
            (setq exts  (car rule)
                  color (cdr rule)
                  name  (intern (format "my-dired-rainbow-%04d" (setq i (1+ i))))))
           ((and (consp rule) (cdr rule))
            (setq name  (car rule)
                  color (cadr rule)
                  exts  (if (consp (cddr rule)) (caddr rule) (cddr rule)))))
          (when (and name color exts)
            (eval `(dired-rainbow-define ,name ,color ,exts))))))))

;; wdired edit background is driven by the dedicated face my/wdired-edit-face.
;; The remap is applied/removed by advising the canonical wdired mode transition functions.

(defvar-local my/wdired--bg-cookie nil
  "Face remap cookie for wdired edit background.")

(defun my/wdired--apply-edit-background (&rest _)
  "Remap default to my/wdired-edit-face when entering wdired edit mode."
  (setq my/wdired--bg-cookie
        (face-remap-add-relative 'default 'my/wdired-edit-face)))

(defun my/wdired--restore-background (&rest _)
  "Remove the wdired edit background remap on exit."
  (when my/wdired--bg-cookie
    (face-remap-remove-relative my/wdired--bg-cookie)
    (setq my/wdired--bg-cookie nil)))

(advice-add 'wdired-change-to-wdired-mode :after #'my/wdired--apply-edit-background)
(advice-add 'wdired-change-to-dired-mode  :after #'my/wdired--restore-background)

;; Theme helper packages

(use-package rainbow-mode)

(use-package transwin
  :after smartrep
  :config
  (smartrep-define-key global-map "C-w" '(("i" . transwin-inc)
                                          ("d" . transwin-dec))))

;; Theme packages

(use-package rustcity-theme
  :straight (:host github :repo "yoshzucker/rustcity-theme")
  :defer t
  :config
  (setq my/theme-special-setups
        (cons (cons 'rustcity
                    (lambda ()
                      (let* ((colors (rustcity-colors))
                             (background    (alist-get 'background    colors))
                             (red           (alist-get 'red           colors))
                             (green         (alist-get 'green         colors))
                             (blue          (alist-get 'blue          colors))
                             (cyan          (alist-get 'cyan          colors))
                             (black         (alist-get 'black         colors))
                             (brightred     (alist-get 'brightred     colors))
                             (brightmagenta (alist-get 'brightmagenta colors))
                             (brightwhite   (alist-get 'brightwhite   colors)))
                        (setq smartrep-mode-line-active-bg brightwhite)
                        (my/set-dired-rainbow-faces
                         `((("el" "lisp" "sh" "r" "c" "h" "py") . ,brightmagenta)
                           (("txt" "org" "md")                  . ,brightmagenta)
                           (("docx" "docm")                     . ,blue)
                           (("xlsx" "xlsm")                     . ,green)
                           (("pptx" "pptm")                     . ,brightred)
                           (("pdf")                             . ,red)))
                        (my/set-faces
                         `((my/wdired-edit-face :background ,(if (eq my/frame-background 'light)
                                                                 brightwhite
                                                               black))
                           (my/org-ongo :inverse-video t :foreground ,brightred :background ,background)
                           (my/org-wait :inverse-video t :inherit font-lock-comment-face)
                           (my/mode-line-over :foreground ,background :background ,red)
                           (my/mode-line-under :foreground ,background :background ,cyan)
                           (my/calendar-iso-week-header :inherit font-lock-function-name-face))))))
              (assq-delete-all 'rustcity my/theme-special-setups))))

(use-package nord-theme
  :defer t)

;; Core setup

(defun my/setup-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (setq frame-background-mode my/frame-background)
  (load-theme my/theme-name t)

  (my/apply-user-fonts)
  (my/apply-font-emoji)
  (let ((fn (alist-get my/theme-name my/theme-special-setups)))
    (when (functionp fn)
      (funcall fn))))

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

;; Initialization

(add-hook 'after-make-frame-functions #'my/apply-user-fonts)

(my/setup-theme)

(provide 'my-ui-face)
;;; my-ui-face.el ends here
