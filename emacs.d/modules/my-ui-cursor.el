;;; my-ui-cursor.el --- Cursor appearance and behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Cursor appearance and scrolling.

;;; Code:

;; Cursor appearance
(setq-default cursor-in-non-selected-windows nil)
(setq blink-cursor-blinks 1)

;; Scroll behavior
(setq scroll-conservatively 5
      scroll-margin 1)

;; Keys first, colours after.
;;
;; A key held down -- `j' down an outline, `C-f' through a file -- asks for
;; screens faster than they can be fontified, and by default each one is
;; fontified in full before the next key is read.  With these two, a redisplay
;; that has input waiting skips the fontification it can, and a scroll command
;; moves without fontifying the text it scrolls past.  The colour arrives as
;; soon as the keys stop; what is never late is where point is.
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t)

(provide 'my-ui-cursor)
;;; my-ui-cursor.el ends here
