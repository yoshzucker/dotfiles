;;; rustcity-theme.el --- Rustcity theme: neon nights and rainy days -*- lexical-binding: t; -*-

;; Author: yoshzucker
;; Maintainer: yoshzucker
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, themes, rustcity, downpour, neon
;; Homepage: https://github.com/yoshzucker/dotfiles
;; License: MIT

;;; Commentary:

;; rustcity is a dual-style theme inspired by the forgotten edges of an industrial city.
;; It captures two contrasting moods:
;;
;; - `downpour` (light mode): A pale, rusted daytime world, hollow under a heavy rain.
;; - `neon` (dark mode): A deep, moody night drenched in neon rain and silence.
;;
;; The theme is designed for readability, emotional tone, and storytelling.
;;
;; To use:
;;   (load-theme 'rustcity t)

;;; Code:

(require 'hsluv)

(deftheme rustcity
  "A theme inspired by a rusted cityscape—silent under neon rain, and hollow in a daylight downpour.")

(defconst rustcity-downpour-hsl
  '((background    . (260  20  87))     ; 6 base tones
    (brightwhite   . (260  20  77))     ; 6 base tones
    (white         . (260  20  67))     ; 6 base tones
    (brightblack   . (260  20  57))     ; 6 base tones
    (black         . (260  20  47))     ; 6 base tones
    (foreground    . (260  20  37))     ; 6 base tones
    (red           . (  0 100  57))     ; 8 neon huess
    (yellow        . ( 70 100  57))     ; 8 neon huess
    (green         . (110 100  57))     ; 8 neon huess
    (cyan          . (200 100  57))     ; 8 neon huess
    (blue          . (250 100  57))     ; 8 neon huess
    (magenta       . (310 100  57))     ; 8 neon huess
    (brightred     . ( 30 100  57))     ; 8 neon huess orange
    (brightmagenta . (280 100  57))     ; 8 neon huess violet
    (brightyellow  . (  0  55  57))     ; 4 diffused hues
    (brightgreen   . (110  55  57))     ; 4 diffused hues
    (brightcyan    . (250  55  57))     ; 4 diffused hues
    (brightblue    . (280  55  57))))   ; 4 diffused hues

(defconst rustcity-neon-hsl
  '((background    . (260  55  13))     ; 6 base tones
    (black         . (260  55  23))     ; 6 base tones
    (brightblack   . (260  55  33))     ; 6 base tones
    (white         . (260  55  43))     ; 6 base tones
    (brightwhite   . (260  55  53))     ; 6 base tones
    (foreground    . (260  55  63))     ; 6 base tones
    (red           . (  0 100  63))     ; 8 neon hues
    (yellow        . ( 70 100  63))     ; 8 neon hues
    (green         . (110 100  63))     ; 8 neon hues
    (cyan          . (200 100  63))     ; 8 neon hues
    (blue          . (250 100  63))     ; 8 neon hues
    (magenta       . (310 100  63))     ; 8 neon hues
    (brightred     . ( 30 100  63))     ; 8 neon hues orange
    (brightmagenta . (280 100  63))     ; 8 neon hues violet
    (brightyellow  . (  0  55  63))     ; 4 diffused hues
    (brightgreen   . (110  55  63))     ; 4 diffused hues
    (brightcyan    . (250  55  63))     ; 4 diffused hues
    (brightblue    . (280  55  63))))   ; 4 diffused hues

(defconst rustcity-downpour
  (cl-loop for (name . hsl) in rustcity-downpour-hsl
           collect
           `(,name . ,(hsluv-hsluv-to-hex hsl))))

(defconst rustcity-neon
  (cl-loop for (name . hsl) in rustcity-neon-hsl
           collect
           `(,name . ,(hsluv-hsluv-to-hex hsl))))

(defun rustcity-colors ()
  "Return color mapping: 16 ANSI colors + foreground/background."
  (if (eq frame-background-mode 'light)
      rustcity-downpour
    rustcity-neon))

(let* ((class '((class color) (min-colors 89)))
       (colors (rustcity-colors))
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
       (background-near (alist-get (if lightp 'brightwhite 'black) colors))
       (background-far  (alist-get (if lightp 'white 'brightblack) colors))
       (foreground-far  (alist-get (if lightp 'brightblack 'white) colors))
       (foreground-near (alist-get (if lightp 'black 'brightwhite) colors)))

  (custom-theme-set-faces
   'rustcity
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(font-lock-comment-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-string-face ((,class (:foreground ,yellow))))
   `(font-lock-doc-face ((,class (:foreground ,white))))
   `(font-lock-keyword-face ((,class (:foreground ,brightmagenta))))
   `(font-lock-builtin-face ((,class (:foreground ,green))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-function-name-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-constant-face ((,class (:foreground ,brightred))))
   `(font-lock-warning-face ((,class (:foreground ,red))))
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

(provide-theme 'rustcity)
(provide 'rustcity)
;;; rustcity-theme.el ends here
