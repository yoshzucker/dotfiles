;;; my-ui-frame.el --- Frame configuration and resizing utilities -*- lexical-binding: t; -*-
;;; Commentary:
;; This module defines frame appearance, title, and dynamic resizing behavior.

;;; Code:

(defun my/frame-apply-default-alist ()
  "Set default frame appearance for new frames."
  (setq default-frame-alist
        '((top . 100) (left . 400) (width . 81) (height . 38)
          (left-fringe . nil) (right-fringe . nil)
          (menu-bar-lines . nil) (tool-bar-lines . nil)
          (vertical-scroll-bars . nil)
          (alpha . (0.96 0.96))
          (ns-transparent-titlebar . t)
          (ns-appearance . dark))))

(defun my/frame-set-title ()
  "Set the frame title and disable menu and tool bars."
  (setq frame-title-format "Emacs %f")
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defvar my/frame-size-list
  '((81 . 38)
    (163 . 38))
  "List of (width . height) frame sizes to cycle through.")

(defvar my/frame-current-size-index 0
  "Current index in `my/frame-size-list`.")

(defcustom my/frame-base-side 'center
  "Which side to base when adjusting frame position after resizing.
Used by `my/frame-cycle-size`."
  :type '(choice (const left) (const center) (const right)))

(defun my/cycle-frame-size ()
  "Cycle to the next frame size and reposition the frame accordingly."
  (interactive)
  (let* ((next-size (my/frame-next-size))
         (new-pos (my/frame-new-position next-size)))
    (my/frame-apply-size-and-position next-size new-pos)))

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "e" #'my/cycle-frame-size
       "m" #'toggle-frame-maximized))

(defun my/frame-next-size ()
  "Advance to the next frame size and return it."
  (setq my/frame-current-size-index
        (mod (1+ my/frame-current-size-index)
             (length my/frame-size-list)))
  (nth my/frame-current-size-index my/frame-size-list))

(defun my/frame-new-position (next-size)
  "Calculate the new position for NEXT-SIZE to keep it aligned with `my/frame-base-side`."
  (let* ((prev-width (frame-width))
         (next-width (car next-size))
         (prev-pos (frame-position))
         (delta-x (* (frame-char-width)
                     (pcase my/frame-base-side
                       ('left 0)
                       ('center (/ (- prev-width next-width) 2))
                       ('right (- prev-width next-width))
                       (_ 0)))))
    (cons (+ (car prev-pos) delta-x)
          (cdr prev-pos))))

(defun my/frame-apply-size-and-position (size pos)
  "Apply SIZE and POS to the current frame, depending on display type."
  (if (display-graphic-p)
      (progn
        (set-frame-size (selected-frame) (car size) (cdr size))
        (set-frame-position (selected-frame) (car pos) (cdr pos)))
    (let* ((cmd (format "\033[8;%d;%dt" (cdr size) (car size)))
           (wrapped (if (getenv "TMUX")
                        (concat "\ePtmux;\e" cmd "\e\\")
                      cmd)))
      (send-string-to-terminal wrapped))))

(defun my/frame-setup ()
  "Initialize frame configuration and title."
  (my/frame-apply-default-alist)
  (my/frame-set-title))

(my/frame-setup)

(provide 'my-ui-frame)
;;; my-ui-frame.el ends here
