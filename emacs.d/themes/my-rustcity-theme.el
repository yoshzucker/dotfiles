;;; my-rustcity-theme.el --- Rustcity theme: neon nights and rainy days -*- lexical-binding: t; -*-

;; Author: yoshzucker
;; Maintainer: yoshzucker
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, themes, rustcity, neon, rain
;; Homepage: https://github.com/yourname/yourrepo
;; License: MIT

;;; Commentary:

;; my-rustcity is a dual-style theme inspired by the forgotten edges of an industrial city.
;; It captures two contrasting moods:
;;
;; - `neon` (dark mode): A deep, moody night drenched in neon rain and silence.
;; - `downpour` (light mode): A pale, rusted daytime world, hollow under a heavy rain.
;;
;; The theme is designed for readability, emotional tone, and storytelling.
;;
;; To use:
;;   (load-theme 'my-rustcity t)

;;; Code:

(require 'hsluv)

(deftheme my-rustcity
  "A theme inspired by a rusted cityscape—silent under neon rain, and hollow in a daylight downpour.")

(defconst my/rustcity-neon-hsl
  '((background    . (260  30  15))
    (black         . (260  30  25))
    (brightblack   . (260  30  35))
    (white         . (260  30  45))
    (brightwhite   . (260  30  55))
    (foreground    . (260  30  65))
    (red           . (  0 100  65))
    (yellow        . ( 70 100  65))
    (green         . (110 100  65))
    (cyan          . (200 100  65))
    (blue          . (250 100  65))
    (magenta       . (310 100  65))
    (brightred     . ( 30 100  65))     ; orange
    (brightyellow  . ( 70  50  50))
    (brightgreen   . (110  50  50))
    (brightcyan    . (180  50  50))
    (brightblue    . (250  50  50))
    (brightmagenta . (270 100  65))))   ; violet

(defconst my/rustcity-neon
  (cl-loop for (name . hsl) in my/rustcity-neon-hsl
           collect
           `(,name . ,(hsluv-hsluv-to-hex hsl))))

(defconst my/rustcity-downpour
  '(
    ;; TODO: Refine this palette for consistent contrast and storytelling
    (background    . "#e6e9ef")
    (foreground    . "#2c2f36")

    (black         . "#d1d4da")
    (red           . "#cc6666")
    (green         . "#669966")
    (yellow        . "#d4b06f")
    (blue          . "#5f8dcf")
    (magenta       . "#b294bb")
    (cyan          . "#70c0ba")
    (white         . "#404552")

    (brightblack   . "#c0c3c9")
    (brightred     . "#dca3a3")
    (brightgreen   . "#a6dba6")
    (brightyellow  . "#f0dfaf")
    (brightblue    . "#a3b8ef")
    (brightmagenta . "#c7a9dd")
    (brightcyan    . "#b3e5e5")
    (brightwhite   . "#1e1e1e")))

(defun my/rustcity-colors ()
  "Return color mapping: 16 ANSI colors + foreground/background."
  (if (eq frame-background-mode 'light)
      my/rustcity-downpour
    my/rustcity-neon))

(let* ((class '((class color) (min-colors 89)))
       (colors (my/rustcity-colors))
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
   'my-rustcity
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(font-lock-builtin-face ((,class (:foreground ,green))))
   `(font-lock-function-name-face ((,class (:foreground ,brightblue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,brightyellow))))
   `(font-lock-comment-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,brightmagenta))))
   `(font-lock-warning-face ((,class (:foreground ,brightred))))
   `(link ((,class (:foreground ,brightgreen :underline t))))
   `(link-visited ((,class (:foreground ,brightmagenta :underline t))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(cursor ((,class (:background ,foreground))))
   `(region ((,class (:background ,black))))
   `(fringe ((,class (:background ,background))))
   `(vertical-border ((,class (:foreground ,foreground :background ,brightblack))))
   `(mode-line ((,class (:foreground ,foreground :background ,brightblack))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,brightblack))))
   `(header-line ((,class (:foreground ,foreground :background ,brightblack))))
   `(highlight ((,class (:background ,brightblack))))
   `(shadow ((,class (:foreground ,white))))
   `(match ((,class (:background ,yellow))))
   `(warning ((,class (:foreground ,yellow))))
   `(error ((,class (:foreground ,red))))
   `(success ((,class (:foreground ,green))))
   `(tooltip ((,class (:foreground ,foreground :background ,yellow))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'my-rustcity)
(provide 'my-rustcity)
;;; my-rustcity-theme.el ends here
