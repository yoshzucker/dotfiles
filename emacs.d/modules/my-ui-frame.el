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

(defcustom my/frame-sidebar-width 21
  "Columns to grow/shrink the frame when opening/closing sidecars."
  :type 'integer
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

(defun my/frame-sidebar-adjust (delta)
  "Adjust current frame width by DELTA columns, keeping base side alignment."
  (let* ((fw (frame-width))
         (fh (frame-height))
         (next (cons (+ fw delta) fh))
         (pos  (my/frame-new-position next)))
    (my/frame-apply-size-and-position next pos)))

(defun my/sidebar--tags ()
  (or (frame-parameter nil 'my/sidecar-tags) '()))

(defun my/sidebar--mark (tag)
  (set-frame-parameter nil 'my/sidecar-tags (cons tag (my/sidebar--tags))))

(defun my/sidebar--unmark (tag)
  (set-frame-parameter nil 'my/sidecar-tags
                       (cl-remove tag (my/sidebar--tags) :test #'eq)))

(defun my/sidebar--marked-p (tag)
  (cl-member tag (my/sidebar--tags) :test #'eq))

(provide 'my-ui-frame)
;;; my-ui-frame.el ends here
