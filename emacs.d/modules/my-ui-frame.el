;;; my-WI-frame.el --- Frame configuration and resizing utilities -*- lexical-binding: t; -*-
;;; Commentary:
;; This module defines frame appearance, title, and dynamic resizing behavior.

;;; Code:
(require 'cl-lib)

(defgroup my/frame nil
  "Custom frame configuration."
  :group 'appearance
  :prefix "my/frame-")

(defcustom my/frame-size-list
  '((81 . 24)
    (81 . 34)
    (163 . 34))
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
    (alpha . (1.00 1.00))
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

;; A window docked to the left or right takes its width out of the text, and
;; on a frame that is one column of text wide there is not enough to take.
;; So the frame moves to the widest shape in `my/frame-size-list' the monitor
;; has room for, and back to the shape it left when the docked window goes.
;;
;; Shapes rather than columns.  Growing by the width of whatever appeared
;; puts the frame anywhere at all -- past the edge of the screen, for a
;; terminal that asks for a hundred columns beside an eighty-column frame --
;; and growing by as much as is left over puts a useless sliver beside the
;; text.  The list is the set of sizes this frame is meant to have, and one
;; of them is the two-column shape, which is what a docked window wants.
;;
;; A frame already at that shape has nowhere to move to, and neither has a
;; maximized one: their windows divide what is there, which is what two
;; panes of a wide frame should do anyway.

(defconst my/frame--restore-parameter 'my/frame-size-before-docking
  "Frame parameter holding the shape to return to, or nil for none.")

(defvar my/frame--in-adjust nil
  "Reentrancy guard: resizing a frame changes its window configuration.")

(defun my/frame--text-columns-available ()
  "Columns of text this frame's monitor has room for, its borders counted out."
  (/ (- (nth 2 (frame-monitor-workarea))
        (- (frame-pixel-width) (frame-text-width)))
     (frame-char-width)))

(defun my/frame--widest-size ()
  "The widest shape in `my/frame-size-list' that fits the monitor, or nil."
  (let ((room (my/frame--text-columns-available)))
    (car (last (seq-filter (lambda (size) (<= (car size) room))
                           my/frame-size-list)))))

(defun my/frame--docked-window-p ()
  "Non-nil when a window is docked to the left or right of this frame.

The foot of the frame is not asked about: the row sill draws there and the
menus transient opens take height, and the width of the text is what this
is about."
  (seq-find (lambda (window)
              (memq (window-parameter window 'window-side) '(left right)))
            (window-list nil 'nomini)))

(defun my/frame--forget-docking (&rest _)
  "Let go of the shape to return to.
A size chosen by hand is the choice that wins, and going back to what was
there before a sidebar opened would undo it."
  (set-frame-parameter nil my/frame--restore-parameter nil))

(defun my/frame--adjust-for-docking (&rest _)
  "Take the wide shape while a window is docked beside the text."
  (unless (or my/frame--in-adjust (frame-parameter nil 'fullscreen))
    (let ((my/frame--in-adjust t)
          (saved (frame-parameter nil my/frame--restore-parameter))
          (docked (my/frame--docked-window-p)))
      (cond
       ((and docked (not saved))
        (when-let* ((wide (my/frame--widest-size))
                    ((> (car wide) (frame-width))))
          (set-frame-parameter nil my/frame--restore-parameter
                               (cons (frame-width) (frame-height)))
          (my/frame-apply-size-and-position wide (my/frame-new-position wide))))
       ((and saved (not docked))
        (set-frame-parameter nil my/frame--restore-parameter nil)
        (my/frame-apply-size-and-position saved (my/frame-new-position saved)))))))

(add-hook 'window-configuration-change-hook #'my/frame--adjust-for-docking)
(advice-add 'my/cycle-frame-size :before #'my/frame--forget-docking)

(defun my/frame-setup ()
  "Initialize frame configuration and title."
  (my/frame-apply-default-alist)
  (my/frame-set-title))

(my/frame-setup)

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "m" #'toggle-frame-maximized
       "RET" #'iconify-frame))

(provide 'my-ui-frame)
;;; my-ui-frame.el ends here
