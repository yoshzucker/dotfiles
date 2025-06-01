;;; my-rustcity-theme.el --- Rustcity theme: neon nights and rainy days -*- lexical-binding: t; -*-

;; Author: yoshzucker
;; Maintainer: yoshzucker
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, themes, rustcity, neon, rain
;; Homepage: https://github.com/yoshzucker/dotfiles
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
  '((background    . (260  30  13))     ; 6 base tones
    (black         . (260  30  23))     ; 6 base tones
    (brightblack   . (260  30  33))     ; 6 base tones
    (white         . (260  30  42))     ; 6 base tones
    (brightwhite   . (260  30  52))     ; 6 base tones
    (foreground    . (260  30  62))     ; 6 base tones
    (red           . (355 100  62))     ; 8 neon hues
    (yellow        . ( 70 100  62))     ; 8 neon hues
    (green         . (110 100  62))     ; 8 neon hues
    (cyan          . (200 100  62))     ; 8 neon hues
    (blue          . (250 100  62))     ; 8 neon hues
    (magenta       . (310 100  62))     ; 8 neon hues
    (brightred     . ( 30 100  62))     ; 8 neon hues orange
    (brightmagenta . (270 100  62))     ; 8 neon hues violet
    (brightyellow  . (355  55  62))     ; 4 diffused hues
    (brightgreen   . (110  55  62))     ; 4 diffused hues
    (brightcyan    . (180  55  62))     ; 4 diffused hues
    (brightblue    . (270  55  62))))   ; 4 diffused hues

(defconst my/rustcity-downpour-hsl
  '((background    . (260  30  87))     ; 6 base tones
    (black         . (260  30  77))     ; 6 base tones
    (brightblack   . (260  30  67))     ; 6 base tones
    (white         . (260  30  58))     ; 6 base tones
    (brightwhite   . (260  30  48))     ; 6 base tones
    (foreground    . (260  30  38))     ; 6 base tones
    (red           . (  0 100  38))     ; 8 neon huess
    (yellow        . ( 70 100  38))     ; 8 neon huess
    (green         . (110 100  38))     ; 8 neon huess
    (cyan          . (200 100  38))     ; 8 neon huess
    (blue          . (250 100  38))     ; 8 neon huess
    (magenta       . (310 100  38))     ; 8 neon huess
    (brightred     . ( 30 100  38))     ; 8 neon huess orange
    (brightmagenta . (270 100  38))     ; 8 neon huess violet
    (brightyellow  . ( 70  55  57))     ; 4 diffused hues
    (brightgreen   . (110  55  57))     ; 4 diffused hues
    (brightcyan    . (180  55  57))     ; 4 diffused hues
    (brightblue    . (310  55  57))))   ; 4 diffused hues

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
   `(font-lock-comment-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-string-face ((,class (:foreground ,yellow))))
   `(font-lock-doc-face ((,class (:foreground ,white))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-builtin-face ((,class (:foreground ,green))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-function-name-face ((,class (:foreground ,red))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-constant-face ((,class (:foreground ,brightmagenta))))
   `(font-lock-warning-face ((,class (:foreground ,brightred))))
   `(link ((,class (:foreground ,brightgreen :underline t))))
   `(link-visited ((,class (:foreground ,green :underline t))))
   `(minibuffer-prompt ((,class (:foreground ,foreground))))
   `(cursor ((,class (:background ,background-far))))
   `(region ((,class (:background ,background-near :extend t))))
   `(fringe ((,class (:background ,background-far))))
   `(vertical-border ((,class (:foreground ,foreground :background ,background-far))))
   `(mode-line ((,class (:foreground ,foreground :background ,background-far))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,background-far))))
   `(header-line ((,class (:foreground ,foreground :background ,background-far))))
   `(highlight ((,class (:background ,background-near))))
   `(shadow ((,class (:foreground ,white))))
   `(match ((,class (:foreground ,background :background ,brightgreen))))
   `(warning ((,class (:foreground ,yellow))))
   `(error ((,class (:foreground ,red))))
   `(success ((,class (:foreground ,green))))
   `(tooltip ((,class (:foreground ,foreground :background ,brightyellow))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'my-rustcity)
(provide 'my-rustcity)
;;; my-rustcity-theme.el ends here
