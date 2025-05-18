;;; my-ui-window.el --- Window layout utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides utilities for managing and toggling window splits.

;;; Code:

(defun my/toggle-window-split-direction ()
  "Toggle window split between horizontal and vertical when exactly two windows are open."
  (interactive)
  (if (/= (count-windows) 2)
      (user-error "This function only supports exactly two windows.")
    (let* ((win1 (selected-window))
           (win2 (next-window win1))
           (buf1 (window-buffer win1))
           (buf2 (window-buffer win2))
           (edges1 (window-edges win1))
           (edges2 (window-edges win2))
           (horizontal (eq (car edges1) (car edges2)))
           (split-fn (if horizontal #'split-window-vertically #'split-window-horizontally)))
      (delete-other-windows)
      (funcall split-fn)
      (set-window-buffer (selected-window) buf1)
      (set-window-buffer (next-window) buf2))))

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "q" #'my/toggle-window-split-direction))

(provide 'my-ui-window)
;;; my-ui-window.el ends here
