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

(require 'cl-lib)

(deftheme my-rustcity
  "A theme inspired by a rusted cityscape—silent under neon rain, and hollow in a daylight downpour.")

(defun my/hsl-to-hex (h s l)
  "Convert HSL (0–360, 0–100, 0–100) to #RRGGBB."
  (let* ((h (/ h 360.0))
         (s (/ s 100.0))
         (l (/ l 100.0))
         (a (* s (min l (- 1 l))))
         (f (lambda (n)
              (let* ((k (mod (float (+ n (* 12 h))) 12.0))
                     (c (- l (* a (max (min (min (- k 3) (- 9 k)) 1) -1)))))
                (round (* 255 c))))))
    (format "#%02X%02X%02X"
            (funcall f 0)  ; R
            (funcall f 8)  ; G
            (funcall f 4)))) ; B

(defun my/hue-lerp (h1 h2 alpha)
  "Interpolate hue circularly between H1 and H2 by ALPHA (0–1)."
  (let* ((delta (mod (- h2 h1) 360))
         (shortest (if (> delta 180)
                       (- delta 360) ;; negative = go backward
                     delta)))
    (mod (+ h1 (* alpha shortest)) 360.0)))

(defun my/hsl-lerp (hsl1 hsl2 alpha)
  "Interpolate between two HSL tuples (H S L) with ALPHA."
  (cl-destructuring-bind (h1 s1 l1) hsl1
    (cl-destructuring-bind (h2 s2 l2) hsl2
      (let ((h (my/hue-lerp h1 h2 alpha))
            (s (+ (* (- 1 alpha) s1) (* alpha s2)))
            (l (+ (* (- 1 alpha) l1) (* alpha l2))))
        (list h s l)))))

(defun my/alpha-by-hue (h1 h2 min base max)
  "Auto-tune alpha based on hue similarity." 
  (let* ((abs-diff (abs (- h1 h2)))
         (dist (min abs-diff (- 360 abs-diff))) ;; 0–180
         (scaled (- 1 (/ dist 180.0)))          ;; closer hues → scaled near 1
         (adjusted (- base (* scaled (- base min)))))
    (max min (min max adjusted))))             ;; clamp to [min, max]

(defconst my/rustcity-neon-hsl
  '((background    . (230  20  15))
    (black         . (230  20  25))
    (brightblack   . (230  20  35))
    (white         . (230  20  55))
    (brightwhite   . (230  20  65))
    (foreground    . (230  20  75))
    (brightred     . (350 100  65))
    (brightyellow  . ( 50 100  65))
    (brightgreen   . (120 100  65))
    (brightcyan    . (190 100  65))
    (brightblue    . (260 100  65))
    (brightmagenta . (290 100  65))))

(defconst my/rustcity-neon
  (append
   (cl-loop for (name . hsl) in my/rustcity-neon-hsl
            collect
            `(,name . ,(apply #'my/hsl-to-hex hsl)))
   (cl-loop for name in '(red yellow green cyan blue magenta)
            for bright-hsl = (alist-get (intern (format "bright%s" name)) my/rustcity-neon-hsl)
            for bg-hsl = (alist-get 'background my/rustcity-neon-hsl)
            for alpha = (my/alpha-by-hue (car bright-hsl) (car bg-hsl) 0.15 0.20 0.35)
            collect
            `(,name . ,(apply #'my/hsl-to-hex (my/hsl-lerp bright-hsl bg-hsl alpha))))))

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
   `(cursor ((,class (:background ,foreground))))
   `(region ((,class (:background ,black))))
   `(fringe ((,class (:background ,background))))
   `(mode-line ((,class (:foreground ,foreground :background ,brightblack))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,black))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,brightblue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-comment-face ((,class (:foreground ,brightblack :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,cyan))))
   `(font-lock-warning-face ((,class (:foreground ,brightred :weight bold))))
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,brightblue :underline t))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'my-rustcity)
(provide 'my-rustcity)
;;; my-rustcity-theme.el ends here
