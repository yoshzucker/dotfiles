;;; my-ui-window.el --- Window layout utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides utilities for managing and toggling window splits.

;;; Code:

(defun my/toggle-window-split-direction ()
  "Toggle window split between horizontal and vertical when exactly two windows are open."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "This function only supports exactly two windows."))
  (let* ((win1 (nth 0 (window-list)))
         (win2 (nth 1 (window-list)))
         (edges1 (window-edges win1))
         (edges2 (window-edges win2))
         (buf1 (window-buffer win1))
         (buf2 (window-buffer win2))
         (start1 (window-start win1))
         (start2 (window-start win2))
         (horizontal (eq (cadr edges1) (cadr edges2))) ; compare TOP coordinates
         (split-fn (if horizontal
                       #'split-window-vertically
                     #'split-window-horizontally)))
    (delete-other-windows)
    (let ((new-win (funcall split-fn)))
      (set-window-buffer (selected-window) buf1)
      (set-window-start (selected-window) start1)
      (set-window-buffer new-win buf2)
      (set-window-start new-win start2))
    (balance-windows)))

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "q" #'my/toggle-window-split-direction))

(provide 'my-ui-window)
;;; my-ui-window.el ends here
