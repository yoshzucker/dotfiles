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

(defconst my/rustcity-downpour-hsl
  '((background    . (260  30  65))
    (black         . (260  30  55))
    (brightblack   . (260  30  45))
    (white         . (260  30  35))
    (brightwhite   . (260  30  25))
    (foreground    . (260  30  15))
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
  (cl-loop for (name . hsl) in my/rustcity-downpour-hsl
           collect
           `(,name . ,(hsluv-hsluv-to-hex hsl))))

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
       (brightwhite   (alist-get 'brightwhite   colors))

       (lightp (eq frame-background-mode 'light))
       (background-near (alist-get (if lightp 'white 'black) colors))
       (background-far  (alist-get (if lightp 'brightwhite 'brightblack) colors))
       (foreground-far  (alist-get (if lightp 'black 'white) colors))
       (foreground-near (alist-get (if lightp 'brightblack 'brightwhite) colors)))

  (custom-theme-set-faces
   'my-rustcity
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(font-lock-builtin-face ((,class (:foreground ,green))))
   `(font-lock-function-name-face ((,class (:foreground ,red))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,white))))
   `(font-lock-comment-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,brightmagenta))))
   `(font-lock-warning-face ((,class (:foreground ,brightred))))
   `(link ((,class (:foreground ,brightgreen :underline t))))
   `(link-visited ((,class (:foreground ,green :underline t))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(cursor ((,class (:background ,foreground))))
   `(region ((,class (:background ,black))))
   `(fringe ((,class (:background ,background))))
   `(vertical-border ((,class (:foreground ,foreground :background ,background-far))))
   `(mode-line ((,class (:foreground ,foreground :background ,background-far))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,background-far))))
   `(header-line ((,class (:foreground ,foreground :background ,background-far))))
   `(highlight ((,class (:background ,background-far))))
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
