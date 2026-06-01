;;; my-ui-face.el --- Theme selection and user environment configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Theme selection, user fonts, and personal workflow faces.
;; Generic helpers for special packages (dired-rainbow, smartrep, etc.)
;; are registered by each theme's use-package :config and applied from
;; my/setup-theme (including on my/toggle-theme).

;;; Code:

(require 'cl-lib)

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

(defface my/org-ongo
  '((t (:inverse-video t)))
  "ONGO todo keyword face.")

(defface my/org-wait
  '((t (:inverse-video t :inherit font-lock-comment-face)))
  "WAIT todo keyword face.")

(defface my/mode-line-over
  '((t ()))
  "Overrunning clock face.")

(defface my/mode-line-under
  '((t ()))
  "Normal clock face.")

(defface my/calendar-iso-week-header
  '((t (:inherit font-lock-function-name-face)))
  "Calendar ISO week header face.")

(defvar my/theme-special-setups nil
  "Alist of (THEME-SYMBOL . FUNCTION) for applying theme-specific
special package configurations (dired-rainbow, smartrep, etc.).
Populated inside each theme's use-package :config.
Called from my/setup-theme (so my/toggle-theme also re-applies them).
Themes without an entry simply leave previous settings as-is.")

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

;; Special package helpers (non-standard face configuration)

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
                  name  (intern (format "my-dired-rainbow-%04d" (cl-incf i)))))
           ((and (consp rule) (cdr rule))
            (setq name  (car rule)
                  color (cadr rule)
                  exts  (if (consp (cddr rule)) (caddr rule) (cddr rule)))))
          (when (and name color exts)
            (eval `(dired-rainbow-define ,name ,color ,exts))))))))

(defun my/set-smartrep-active-background (color)
  "Set `smartrep-mode-line-active-bg' to COLOR (a concrete color string).
Safe to call before smartrep is loaded; the variable will be read when the
package initializes."
  (setq smartrep-mode-line-active-bg color))

(defvar my/wdired-edit-backgrounds nil
  "Edit background for wdired (:light and :dark).")

(defvar-local my/wdired--bg-cookie nil
  "Face remap cookie for wdired background.")

(defun my/wdired--apply-edit-background (&rest _)
  "Temporarily remap default background for wdired edit mode."
  (when my/wdired-edit-backgrounds
    (let ((bg (if (eq (bound-and-true-p frame-background-mode) 'light)
                  (plist-get my/wdired-edit-backgrounds :light)
                (plist-get my/wdired-edit-backgrounds :dark))))
      (when (and bg (not (string= bg "")))
        (setq my/wdired--bg-cookie
              (face-remap-add-relative 'default :background bg))))))

(defun my/wdired--restore-background (&rest _)
  "Remove wdired edit background remap."
  (when my/wdired--bg-cookie
    (face-remap-remove-relative my/wdired--bg-cookie)
    (setq my/wdired--bg-cookie nil)))

(defun my/set-wdired-edit-background (&rest plist)
  "Set edit background for wdired (:light and :dark).
This installs the necessary advice to temporarily change the default
background when entering wdired mode.
Safe to call before wdired is loaded (advice-add works on undefined functions)."
  (setq my/wdired-edit-backgrounds plist)
  (advice-remove 'wdired-change-to-wdired-mode #'my/wdired--apply-edit-background)
  (advice-remove 'wdired-change-to-dired-mode #'my/wdired--restore-background)
  (when my/wdired-edit-backgrounds
    (advice-add 'wdired-change-to-wdired-mode :after #'my/wdired--apply-edit-background)
    (advice-add 'wdired-change-to-dired-mode :after #'my/wdired--restore-background)))

(use-package rainbow-mode)

(use-package transwin
  :after smartrep
  :config
  (smartrep-define-key global-map "C-w" '(("i" . transwin-inc)
                                          ("d" . transwin-dec))))

(use-package rustcity-theme
  :straight (:host github :repo "yoshzucker/rustcity-theme")
  :defer t
  :config
  ;; Register the special package setup for rustcity so that
  ;; my/setup-theme (including calls from my/toggle-theme) can apply it.
  (setq my/theme-special-setups
        (cons (cons 'rustcity
                    (lambda ()
                      (let* ((colors (rustcity-colors))
                             (brightmagenta (alist-get 'brightmagenta colors))
                             (blue          (alist-get 'blue          colors))
                             (green         (alist-get 'green         colors))
                             (brightred     (alist-get 'brightred     colors))
                             (red           (alist-get 'red           colors))
                             (cyan          (alist-get 'cyan          colors))
                             (brightwhite   (alist-get 'brightwhite   colors))
                             (background    (alist-get 'background    colors))
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
                           (my/calendar-iso-week-header :inherit font-lock-function-name-face))))))
              (assq-delete-all 'rustcity my/theme-special-setups))))

(use-package nord-theme
  :defer t)

(defun my/setup-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (setq frame-background-mode my/frame-background)
  (load-theme my/theme-name t)

  (let ((fn (alist-get my/theme-name my/theme-special-setups)))
    (when (functionp fn)
      (funcall fn)))

  (my/apply-user-fonts)
  (my/apply-font-emoji))

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
