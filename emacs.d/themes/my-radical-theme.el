;;; my-radical-theme.el --- Rustcity theme: neon nights and rainy days -*- lexical-binding: t; -*-

;; Author: yoshzucker
;; Maintainer: yoshzucker
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces, themes, rustcity, neon, rain
;; Homepage: https://github.com/yoshzucker/dotfiles
;; License: MIT

;;; Commentary:

;; my-radical is a dual-style theme inspired by the forgotten edges of an industrial city.
;; It captures two contrasting moods:
;;
;; - `neon` (dark mode): A deep, moody night drenched in neon rain and silence.
;; - `downpour` (light mode): A pale, rusted daytime world, hollow under a heavy rain.
;;
;; The theme is designed for readability, emotional tone, and storytelling.
;;
;; To use:
;;   (load-theme 'my-radical t)

;;; Code:

(require 'hsluv)

(deftheme my-radical
  "A theme inspired by a rusted cityscape—silent under neon rain, and hollow in a daylight downpour.")

(defconst my/radical-hsl
  '((background    . (260  30  10))     ; 6 base tones
    (black         . (260  30  15))     ; 6 base tones
    (brightblack   . (260  30  20))     ; 6 base tones
    (white         . ( 12  10  50))     ; 6 base tones
    (brightwhite   . ( 12  10  55))     ; 6 base tones
    (foreground    . ( 12  10  60))     ; 6 base tones
    (red           . ( 12  99  53))     ; 8 radical **
    (yellow        . ( 12  99  53))     ; 8 radical **
    (green         . ( 12  99  53))     ; 8 radical **
    (cyan          . ( 12  99  60))     ; 8 radical **
    (blue          . ( 12  99  44))     ; 8 radical **
    (magenta       . ( 12  99  35))     ; 8 radical **
    (brightred     . (206  90  65))     ; 8 radical **
    (brightmagenta . (206  90  65))     ; 8 radical **
    (brightyellow  . (104  99  90))     ; 4 accent tones
    (brightgreen   . (104  90  80))     ; 4 accent tones
    (brightcyan    . (104  90  70))     ; 4 accent tones
    (brightblue    . (104  90  60))))   ; 4 accent tones

(defun my/radical-colors ()
  "Return color mapping: 16 ANSI colors + foreground/background."
  (cl-loop for (name . hsl) in my/radical-hsl
           collect
           `(,name . ,(hsluv-hsluv-to-hex hsl))))

(let* ((class '((class color) (min-colors 89)))
       (colors (my/radical-colors))
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

       (background-near (alist-get 'black       colors))
       (background-far  (alist-get 'brightblack colors))
       (foreground-far  (alist-get 'white       colors))
       (foreground-near (alist-get 'brightwhite colors)))

  (custom-theme-set-faces
   'my-radical
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

(provide-theme 'my-radical)
(provide 'my-radical)
;;; my-radical-theme.el ends here
