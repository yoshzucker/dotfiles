;;; my-ui-frame-test.el --- Tests for the docking sidecar  -*- lexical-binding: t; -*-

;;; Commentary:

;; `my/frame--adjust-for-docking' widens the frame while a sidebar is open
;; and puts it back afterwards.  What these test is the putting back, which
;; is where it went wrong: the frame drifted sideways a little further every
;; time a sidebar opened and closed.
;;
;; `set-frame-position' hands the move to the window manager and returns;
;; the frame is where it was until the manager gets to it.  So a return trip
;; that works out where to go from `frame-position' can read the position
;; from before the outward move and apply the same offset twice.  The stub
;; below is that machine: a size that takes effect immediately, a position
;; that has not caught up.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)

(defvar my-ui-frame-test--loaded
  (my-test-load (my-test-module "my-ui-frame") "my/frame")
  "How many definitions were taken out of the module.")

(ert-deftest my-ui-frame-test-definitions-are-there ()
  "The names these tests are about still exist under those names."
  (should (> my-ui-frame-test--loaded 0))
  (dolist (fn '(my/frame--adjust-for-docking
                my/frame-new-position
                my/frame--forget-docking))
    (should (fboundp fn)))
  (should (boundp 'my/frame--restore-parameter)))

(defmacro my-ui-frame-test--with-frame (&rest body)
  "Run BODY against a frame whose moves the window manager has not made yet.

`applied' collects the (SIZE POSITION) pairs asked for, newest last.
Resizing takes effect on `width'/`height' because Emacs knows its own
text dimensions; the position does not, which is the case being tested."
  (declare (indent 0))
  `(let ((applied nil)
         (width 81) (height 24)
         (position '(400 . 100))
         (docked nil)
         (my/frame-base-side 'center))
     (cl-letf (((symbol-function 'my/frame--docked-window-p) (lambda () docked))
               ((symbol-function 'my/frame--widest-size) (lambda () '(163 . 34)))
               ((symbol-function 'frame-width) (lambda (&rest _) width))
               ((symbol-function 'frame-height) (lambda (&rest _) height))
               ((symbol-function 'frame-position) (lambda (&rest _) position))
               ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
               ((symbol-function 'my/frame-apply-size-and-position)
                (lambda (size pos)
                  (push (list size pos) applied)
                  (setq width (car size) height (cdr size)))))
       (set-frame-parameter nil my/frame--restore-parameter nil)
       (ignore docked position)
       ,@body)))

(ert-deftest my-ui-frame-test-widens-when-a-sidebar-opens ()
  "A sidebar takes the frame to the widest shape there is."
  (my-ui-frame-test--with-frame
    (setq docked t)
    (my/frame--adjust-for-docking)
    (should (equal (car (car applied)) '(163 . 34)))))

(ert-deftest my-ui-frame-test-returns-to-where-it-was ()
  "And closing it puts the frame back, at the size and the place it had.

The position is the half that used to be wrong: worked out from
`frame-position' on the way back, it came to 400 + (163-81)/2 columns =
810, because the outward move had not happened yet as far as the window
manager was concerned."
  (my-ui-frame-test--with-frame
    (setq docked t)
    (my/frame--adjust-for-docking)
    (setq docked nil)
    (my/frame--adjust-for-docking)
    (should (equal (car applied) '((81 . 24) (400 . 100))))))

(ert-deftest my-ui-frame-test-does-not-drift-over-several-sidebars ()
  "Which has to hold however many times a sidebar comes and goes."
  (my-ui-frame-test--with-frame
    (dotimes (_ 3)
      (setq docked t)
      (my/frame--adjust-for-docking)
      (setq docked nil)
      (my/frame--adjust-for-docking))
    (should (= (length applied) 6))
    (dolist (call applied)
      (when (equal (car call) '(81 . 24))
        (should (equal (cadr call) '(400 . 100)))))))

(ert-deftest my-ui-frame-test-leaves-a-frame-that-is-wide-enough-alone ()
  "Nothing to widen to, nothing to put back."
  (my-ui-frame-test--with-frame
    (setq width 163 height 34 docked t)
    (my/frame--adjust-for-docking)
    (should-not applied)
    (setq docked nil)
    (my/frame--adjust-for-docking)
    (should-not applied)))

(ert-deftest my-ui-frame-test-a-size-chosen-by-hand-wins ()
  "Cycling the size while a sidebar is open forgets the way back.
Otherwise closing the sidebar would undo the size that was just asked for."
  (my-ui-frame-test--with-frame
    (setq docked t)
    (my/frame--adjust-for-docking)
    (should (frame-parameter nil my/frame--restore-parameter))
    (my/frame--forget-docking)
    (setq applied nil docked nil)
    (my/frame--adjust-for-docking)
    (should-not applied)))

(provide 'my-ui-frame-test)
;;; my-ui-frame-test.el ends here
