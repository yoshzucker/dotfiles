;;; my-ui-frame.el --- Frame configuration and resizing utilities -*- lexical-binding: t; -*-
;;; Commentary:
;; This module defines frame appearance, title, and dynamic resizing behavior.

;;; Code:
(require 'cl-lib)

(defgroup my/frame nil
  "Custom frame configuration."
  :group 'appearance
  :prefix "my/frame-")

(defcustom my/frame-size-list
  '((81 . 36)
    (163 . 36))
  "List of (WIDTH . HEIGHT) frame sizes to cycle through.
Each element is a cons cell (WIDTH . HEIGHT)."
  :type '(repeat (cons (integer :tag "Width")
                       (integer :tag "Height")))
  :group 'my/frame)

(defcustom my/frame-default-alist
  '((top . 100) (left . 400)
    (left-fringe . nil) (right-fringe . nil)
    (menu-bar-lines . nil) (tool-bar-lines . nil)
    (vertical-scroll-bars . nil)
    (alpha . (0.96 0.96))
    (ns-transparent-titlebar . t)
    (ns-appearance . dark))
  "Default frame parameters applied to new frames.
Used by `my/frame-apply-default-alist`."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'my/frame)

(defcustom my/frame-base-side 'center
  "Which side to base when adjusting frame position after resizing.
Used by `my/cycle-frame-size`."
  :type '(choice (const left) (const center) (const right))
  :group 'my/frame)

(defvar my/frame-current-size-index 0
  "Current index in `my/frame-size-list`.")

(defun my/frame-apply-default-alist ()
  "Apply `my/frame-default-alist` to `default-frame-alist`."
  (let ((size (car my/frame-size-list)))
    (setq default-frame-alist
          (append (list (cons 'width (car size))
                        (cons 'height (cdr size)))
                  my/frame-default-alist))))

(defun my/frame-set-title ()
  "Set the frame title and disable menu and tool bars."
  (setq frame-title-format "Emacs %f")
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(defun my/cycle-frame-size (&optional index)
  "Cycle to the next frame size, or jump to INDEX if given.
INDEX is 1-based (1 = first entry in `my/frame-size-list`)."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let* ((next-size
          (cond
           (index
            (setq my/frame-current-size-index (1- index))
            (nth my/frame-current-size-index my/frame-size-list))
           (t
            (setq my/frame-current-size-index
                  (mod (1+ my/frame-current-size-index)
                       (length my/frame-size-list)))
            (nth my/frame-current-size-index my/frame-size-list))))
         (new-pos (my/frame-new-position next-size)))
    (my/frame-apply-size-and-position next-size new-pos)))

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

(defun my/frame-sidebar-adjust (delta)
  "Adjust current frame width by DELTA columns, keeping base side alignment."
  (let* ((fw (frame-width))
         (fh (frame-height))
         (next (cons (+ fw delta) fh))
         (pos  (my/frame-new-position next)))
    (my/frame-apply-size-and-position next pos)))

(defvar my/sidebar--in-adjust nil
  "Reentrancy guard for auto sidecar width adjustment.")

(defun my/sidebar--applied-width ()
  (or (frame-parameter nil 'my/sidecar-comp-width-applied) 0))

(defun my/sidebar--set-applied-width (cols)
  (set-frame-parameter nil 'my/sidecar-comp-width-applied cols))

(defun my/sidebar--clear-applied-width (&rest _)
  "Invalidate the currently applied sidebar width so the next frame size change."
  (my/sidebar--set-applied-width 0))

(defun my/sidebar--compute-desired-width ()
  "Return the total number of columns occupied by all active side windows — the sum of each side’s maximum width plus one."
  (let ((left 0) (right 0))
    (dolist (w (window-list nil 'nomini))
      (pcase (window-parameter w 'window-side)
        ('left  (setq left  (max left  (window-total-width w t))))
        ('right (setq right (max right (window-total-width w t))))))
    (+ (if (> left 0)  (1+ left)  0)
       (if (> right 0) (1+ right) 0))))

(defun my/sidebar--auto-adjust (&rest _)
  "Automatically adjust the frame width to match the current side window configuration."
  (unless my/sidebar--in-adjust
    (let* ((desired (my/sidebar--compute-desired-width))
           (applied (my/sidebar--applied-width))
           (delta   (- desired applied)))
      (when (/= delta 0)
        (let ((my/sidebar--in-adjust t))
          (my/frame-sidebar-adjust delta)
          (my/sidebar--set-applied-width desired))))))

(add-hook 'window-configuration-change-hook #'my/sidebar--auto-adjust)
(advice-add 'my/cycle-frame-size :before #'my/sidebar--clear-applied-width)
(advice-add 'my/cycle-frame-size :after #'my/sidebar--auto-adjust)

(defun my/frame-setup ()
  "Initialize frame configuration and title."
  (my/frame-apply-default-alist)
  (my/frame-set-title))

(my/frame-setup)

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "e" #'my/cycle-frame-size
       "m" #'toggle-frame-maximized
       "RET" #'iconify-frame))

(provide 'my-ui-frame)
;;; my-ui-frame.el ends here
