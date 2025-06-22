;;; my-tokyo-theme.el --- Tokyo Night Storm inspired 16-color theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple 16-color Emacs theme inspired by Tokyo Night's "Storm" variant.

;;; Code:

(deftheme my-tokyo "Tokyo Night Storm inspired 16-color theme.")

(defconst my/tokyo-storm-palette
  '((background    . "#24283b") (foreground    . "#c0caf5")
    (black         . "#1d202f") (red           . "#f7768e")
    (green         . "#9ece6a") (yellow        . "#e0af68")
    (blue          . "#7aa2f7") (magenta       . "#bb9af7")
    (cyan          . "#7dcfff") (white         . "#a9b1d6")
    (brightblack   . "#414868") (brightred     . "#ff899d")
    (brightgreen   . "#9fe044") (brightyellow  . "#faba4a")
    (brightblue    . "#8db0ff") (brightmagenta . "#c7a9ff")
    (brightcyan    . "#a4daff") (brightwhite   . "#c0caf5")))

(defconst my/tokyo-night-palette
  '((background    . "#1a1b26") (foreground    . "#c0caf5")
    (black         . "#15161e") (red           . "#f7768e")
    (green         . "#9ece6a") (yellow        . "#e0af68")
    (blue          . "#7aa2f7") (magenta       . "#bb9af7")
    (cyan          . "#7dcfff") (white         . "#a9b1d6")
    (brightblack   . "#414868") (brightred     . "#ff899d")
    (brightgreen   . "#9fe044") (brightyellow  . "#faba4a")
    (brightblue    . "#8db0ff") (brightmagenta . "#c7a9ff")
    (brightcyan    . "#a4daff") (brightwhite   . "#c0caf5")))

(defconst my/tokyo-moon-palette
  '((background    . "#222436") (foreground    . "#c8d3f5")
    (black         . "#1b1d2b") (red           . "#ff757f")
    (green         . "#c3e88d") (yellow        . "#ffc777")
    (blue          . "#82aaff") (magenta       . "#c099ff")
    (cyan          . "#86e1fc") (white         . "#828bb8")
    (brightblack   . "#444a73") (brightred     . "#ff8d94")
    (brightgreen   . "#c7fb6d") (brightyellow  . "#ffd8ab")
    (brightblue    . "#9ab8ff") (brightmagenta . "#caabff")
    (brightcyan    . "#b2ebff") (brightwhite   . "#c8d3f5")))

(defconst my/tokyo-day-palette
  '((background    . "#e1e2e7") (foreground    . "#3760bf")
    (black         . "#b4b5b9") (red           . "#f52a65")
    (green         . "#587539") (yellow        . "#8c6c3e")
    (blue          . "#2e7de9") (magenta       . "#9854f1")
    (cyan          . "#007197") (white         . "#6172b0")
    (brightblack   . "#a1a6c5") (brightred     . "#ff4774")
    (brightgreen   . "#5c8524") (brightyellow  . "#a27629")
    (brightblue    . "#358aff") (brightmagenta . "#a463ff")
    (brightcyan    . "#007ea8") (brightwhite   . "#3760bf")))

(defcustom my/tokyo-dark-variant 'storm
  "Which Tokyo Night dark variant to use. Options are 'storm, 'night, or 'moon."
  :type '(choice (const :tag "Storm" storm)
                 (const :tag "Night" night)
                 (const :tag "Moon" moon))
  :group 'my/ui)

(defun my/tokyo-colors ()
  "Return color mapping: 16 ANSI colors + foreground/background."
  (pcase frame-background-mode
    ('light my/tokyo-day-palette)
    ('dark
     (pcase my/tokyo-dark-variant
       ('storm my/tokyo-storm-palette)
       ('night my/tokyo-night-palette)
       ('moon  my/tokyo-moon-palette)
       (_      my/tokyo-storm-palette)))))

(let* ((class '((class color) (min-colors 89)))
       (colors (my/tokyo-colors))
       (background    (alist-get 'background    colors))
       (foreground    (alist-get 'foreground    colors))
       (red           (alist-get 'red           colors))
       (green         (alist-get 'green         colors))
       (yellow        (alist-get 'yellow        colors))
       (blue          (alist-get 'blue          colors))
       (magenta       (alist-get 'magenta       colors))
       (cyan          (alist-get 'cyan          colors))
       (black         (alist-get 'black         colors))
       (white         (alist-get 'white         colors))
       (brightblack   (alist-get 'brightblack   colors))
       (brightred     (alist-get 'brightred     colors))
       (brightgreen   (alist-get 'brightgreen   colors))
       (brightyellow  (alist-get 'brightyellow  colors))
       (brightblue    (alist-get 'brightblue    colors))
       (brightmagenta (alist-get 'brightmagenta colors))
       (brightcyan    (alist-get 'brightcyan    colors))
       (brightwhite   (alist-get 'brightwhite   colors)))

  (custom-theme-set-faces
   'my-tokyo
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(cursor ((,class (:background ,foreground))))
   `(region ((,class (:background ,brightblack))))
   `(fringe ((,class (:background ,background))))
   `(mode-line ((,class (:foreground ,foreground :background ,brightblack))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,black))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,brightblue))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-comment-face ((,class (:foreground ,white))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-warning-face ((,class (:foreground ,brightred :weight bold))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foregorund ,brightblue :underline t))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-tokyo)
(provide 'my-tokyo)
;;; my-tokyo-theme.el ends here
