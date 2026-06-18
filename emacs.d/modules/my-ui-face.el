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
  `(:family "FirgeNerd Console"
            :weight normal
            :height ,(if (eq system-type 'darwin) 140 130))
  "Default face font spec (plist of :family, :weight, :height)."
  :type '(list (const :family) string
               (const :weight) (choice (const thin)
                                       (const ultra-light)
                                       (const light)
                                       (const semi-light)
                                       (const normal)
                                       (const semi-bold)
                                       (const bold)
                                       (const ultra-bold)
                                       (const heavy))
               (const :height) integer)
  :group 'my/ui)

(defcustom my/font-variable-pitch
  `(:family "Source Han Sans JP"
            :weight normal)
  "Variable-pitch face font spec (plist of :family and :weight)."
  :type '(list (const :family) string
               (const :weight) (choice (const thin)
                                       (const ultra-light)
                                       (const light)
                                       (const semi-light)
                                       (const normal)
                                       (const semi-bold)
                                       (const bold)
                                       (const ultra-bold)
                                       (const heavy)))
  :group 'my/ui)

(defcustom my/font-emoji
  (cond ((eq system-type 'darwin) "Apple Color Emoji")
        ((eq system-type 'windows-nt) "Segoe UI Emoji")
        (t "Noto Color Emoji"))
  "Emoji font family."
  :type 'string
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

;; mode-line / mode-line-inactive themselves carry the EDGE color, so
;; the auto-fill area at the right of the mode-line is naturally edge-
;; colored.  The center content area is overlaid with the LINE faces
;; below to restore the mono0 (active) / mono1 (inactive) center.
(defface my/mode-line-line '((t (:inherit mode-line)))
  "Background of the active mode-line center content area.")

(defface my/mode-line-inactive-line '((t (:inherit mode-line-inactive)))
  "Background of the inactive mode-line center content area.")

;; General utilities

(defun my/set-faces (specs)
  "Set faces from SPECS, a list of (FACE . PLIST) forms."
  (dolist (spec specs)
    (apply #'set-face-attribute (car spec) nil (cdr spec))))

(defun my/apply-font-emoji ()
  (set-fontset-font t 'emoji my/font-emoji nil 'prepend))

(defun my/apply-user-fonts (&optional _frame)
  "Apply user font preferences."
  (apply #'set-face-attribute 'default nil my/font-default)
  (apply #'set-face-attribute 'fixed-pitch nil my/font-default)
  (apply #'set-face-attribute 'variable-pitch nil my/font-variable-pitch))

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

;; Tab-bar and mode-line visuals

;; Trapezoidal active tab using Nerd Font lower-triangle glyphs
;; (powerline range U+E0B8 / U+E0BA), so the active tab visually
;; widens at the bottom and flows into the editing surface below.
;; Requires a Nerd Font (e.g. `FirgeNerd Console' set in my-ui-face).
;; Inactive tabs render plain; only the current tab carries the slant.
(defconst my/tab-slant-left  "\xe0ba"
  "Glyph placed before the current tab name to widen its left edge.")
(defconst my/tab-slant-right "\xe0b8"
  "Glyph placed after the current tab name to widen its right edge.")

(defun my/tab-bar-tab-name-format (tab _i)
  "Wrap each tab with slanted edges that fan outward to body.
The slant glyphs use the `tab-bar' bg above the diagonal and the
tab's own bg below, producing a trapezoidal tab whose lower edge
merges with the chrome layer underneath (the active tab merges
with body `mono0', the inactive with the chrome neutral `mono1').
Both current and inactive tabs share the same format, so the
visual width is constant across switches."
  (let* ((current-p (eq (car tab) 'current-tab))
         (face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive))
         (name (alist-get 'name tab))
         (text (concat " " name " "))
         (bar-bg (face-background 'tab-bar nil 'default))
         (tab-bg (face-background face nil 'default))
         (slant `(:foreground ,tab-bg :background ,bar-bg)))
    (concat
     (propertize my/tab-slant-left  'face slant)
     (propertize text                'face face)
     (propertize my/tab-slant-right 'face slant))))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)

;; Org-clock left-edge slant for the tab-bar global area.  Figure-
;; ground inverted vs. an "upper-right triangle painted with clock fg"
;; approach: we paint the CHROME wedge as fg on a solid CLOCK bg, so
;; the top row of the slant cell is a clean clock-color fill (no anti-
;; alias sliver at the top edge — the same fix used for the mode-line
;; slants).  The fg wedge sits in the lower-left where it meets the
;; surrounding chrome, so any rendering artifact there is hidden by
;; same-color continuation.  Net visual: upper-right of the cell is
;; clock, lower-left is chrome — clock "rises" from lower-right to
;; upper-left into the chrome layer.
(defconst my/tab-bar-clock-slant "\xe0b8"
  "Lower-left triangle drawn at the left edge of the org-clock area.")

(defun my/org-clock-decorate (string)
  "Filter-return advice for `org-clock-get-clock-string'.
Color STRING by `org-clock-task-overrun' and prepend a slant
glyph whose fg paints a `tab-bar' chrome wedge on a solid clock-
colored bg, producing a triangular bridge into the clock area."
  (let* ((face (if org-clock-task-overrun
                   'my/mode-line-over
                 'my/mode-line-under))
         (clock-bg (face-background face nil 'default))
         (bar-bg (face-background 'tab-bar nil 'default))
         (slant `(:foreground ,bar-bg :background ,clock-bg)))
    (concat (propertize my/tab-bar-clock-slant 'face slant)
            (propertize string 'face face))))

(with-eval-after-load 'org-clock
  (advice-add 'org-clock-get-clock-string
              :filter-return #'my/org-clock-decorate))

;; Trapezoidal mode-line using lower-triangle glyphs (U+E0B8 / U+E0BA).
;; Figure-ground inversion of the tab-bar: instead of painting the line
;; color as a triangle on the edge color, we paint the EDGE color as a
;; wedge on the line bg.  This keeps the slant cell's upper area as
;; pure line bg (no rendering sliver at the top), so the open side of
;; the inverted-ハ merges cleanly with the body above.  The wedge
;; itself (foreground) connects to the leading/trailing edge padding,
;; producing the trapezoidal silhouette.
;; Both active and inactive mode-lines get the same shape; color pairs
;; are theme-driven via `my/mode-line-edge' / `my/mode-line-inactive-edge'.
(defconst my/mode-line-slant-left  "\xe0b8"
  "Lower-left triangle drawn at the start of the mode-line.")
(defconst my/mode-line-slant-right "\xe0ba"
  "Lower-right triangle drawn at the end of the mode-line.")

(defun my/mode-line-line-face ()
  "Return the line (content) face for the current mode-line."
  (if (mode-line-window-selected-p)
      'my/mode-line-line
    'my/mode-line-inactive-line))

(defun my/mode-line-edge-face ()
  "Return the edge face for the current mode-line.
The `mode-line' / `mode-line-inactive' faces themselves carry the
edge color, so Emacs' implicit fill at the trailing end of the
mode-line is naturally edge-colored."
  (if (mode-line-window-selected-p)
      'mode-line
    'mode-line-inactive))

(defun my/mode-line-slant (side)
  "Return a slant glyph for SIDE (`left' or `right') of the mode-line.
The glyph's bg uses the line color (matches the center content),
and its fg paints the edge color as a wedge in the lower corner,
connecting visually to the surrounding edge fill."
  (let* ((line-bg (face-background (my/mode-line-line-face) nil 'default))
         (edge-bg (face-background (my/mode-line-edge-face) nil 'default))
         (face `(:foreground ,edge-bg :background ,line-bg))
         (glyph (if (eq side 'left)
                    my/mode-line-slant-left
                  my/mode-line-slant-right)))
    (propertize glyph 'face face)))

(defun my/mode-line-edge-pad ()
  "Return a 1-char space carrying the edge (mode-line) face."
  (propertize " " 'face (my/mode-line-edge-face)))

(defvar my/mode-line-default-format
  (default-value 'mode-line-format)
  "Snapshot of the standard `mode-line-format' before our wrappers.")

(setq-default mode-line-format
              `((:eval (my/mode-line-edge-pad))
                (:eval (my/mode-line-slant 'left))
                (:eval (propertize (format-mode-line my/mode-line-default-format)
                                   'face (my/mode-line-line-face)))
                (:eval (my/mode-line-slant 'right))
                (:eval (my/mode-line-edge-pad))))

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
                      (let* ((colors (rustcity-palette))
                             (mono0  (alist-get 'mono0  colors))
                             (mono1  (alist-get 'mono1  colors))
                             (mono2  (alist-get 'mono2  colors))
                             (red    (alist-get 'red    colors))
                             (orange (alist-get 'orange colors))
                             (green  (alist-get 'green  colors))
                             (cyan   (alist-get 'cyan   colors))
                             (blue   (alist-get 'blue   colors)))
                        (setq smartrep-mode-line-active-bg mono2)
                        (my/set-dired-rainbow-faces
                         `((("docx" "docm") . ,blue)
                           (("xlsx" "xlsm") . ,green)
                           (("pptx" "pptm") . ,orange)
                           (("pdf")         . ,red)))
                        (my/set-faces
                         `((my/wdired-edit-face :background ,mono1)
                           (my/org-ongo :inverse-video t :foreground ,orange :background ,mono0)
                           (my/org-wait :inverse-video t :inherit font-lock-comment-face)
                           (my/mode-line-over :foreground ,mono0 :background ,red)
                           (my/mode-line-under :foreground ,mono0 :background ,cyan)
                           (mode-line :background ,mono1)
                           (mode-line-inactive :background ,mono2)
                           (my/mode-line-line :background ,mono0)
                           (my/mode-line-inactive-line :background ,mono1)
                           (my/calendar-iso-week-header :inherit font-lock-function-name-face))))))
              (assq-delete-all 'rustcity my/theme-special-setups))))

(use-package gensho-theme
  :straight (:host github :repo "yoshzucker/gensho-theme")
  :defer t
  :config
  (setq my/theme-special-setups
        (cons (cons 'gensho
                    (lambda ()
                      (let* ((colors (gensho-palette))
                             (mono0  (alist-get 'mono0  colors))
                             (mono1  (alist-get 'mono1  colors))
                             (mono2  (alist-get 'mono2  colors))
                             (red    (alist-get 'red    colors))
                             (orange (alist-get 'orange colors))
                             (green  (alist-get 'green  colors))
                             (cyan   (alist-get 'cyan   colors))
                             (blue   (alist-get 'blue   colors)))
                        (setq smartrep-mode-line-active-bg mono2)
                        (my/set-dired-rainbow-faces
                         `((("docx" "docm") . ,blue)
                           (("xlsx" "xlsm") . ,green)
                           (("pptx" "pptm") . ,orange)
                           (("pdf")         . ,red)))
                        (my/set-faces
                         `((my/wdired-edit-face :background ,mono1)
                           (my/org-ongo :inverse-video t :foreground ,orange :background ,mono0)
                           (my/org-wait :inverse-video t :inherit font-lock-comment-face)
                           (my/mode-line-over :foreground ,mono0 :background ,red)
                           (my/mode-line-under :foreground ,mono0 :background ,cyan)
                           (mode-line :background ,mono1)
                           (mode-line-inactive :background ,mono2)
                           (my/mode-line-line :background ,mono0)
                           (my/mode-line-inactive-line :background ,mono1)
                           (my/calendar-iso-week-header :inherit font-lock-function-name-face)))
                        )))
              (assq-delete-all 'gensho my/theme-special-setups))))

(use-package nord-theme
  :defer t)

(use-package auto-dim-other-buffers
  :config
  (add-hook 'after-init-hook (lambda ()
                               (when (fboundp 'auto-dim-other-buffers-mode)
                                 (auto-dim-other-buffers-mode t)))))

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
