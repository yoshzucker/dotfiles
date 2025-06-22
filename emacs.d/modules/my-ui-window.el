;;; my-ui-window.el --- Window layout utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides utilities for managing and toggling window splits.

;;; Code:

(defcustom my/allocate-window-min-height 10
  "Minimum height (in lines) that any window should retain when redistributing height."
  :type 'integer
  :group 'my/ui)

(defun my/allocate-window-height ()
  "Try to give more vertical space to the selected window by shrinking others."
  (interactive)
  (let* ((win (selected-window))
         (others (seq-filter
                  (lambda (w)
                    (and (not (eq w win))
                         (= (window-width w) (window-width win))))
                  (window-list)))
         (delta 0))
    (dolist (ow others)
      (let ((h (window-height ow)))
        (when (> h my/allocate-window-min-height)
          (let ((shrink (- h my/allocate-window-min-height)))
            (condition-case nil
                (progn
                  (window-resize ow (- shrink))
                  (setq delta (+ delta shrink)))
              (error nil))))))
    (when (> delta 0)
      (condition-case nil
          (window-resize win delta)
        (error nil)))))

(defun my/allocate-window--selection-hook (_)
  (unless (minibufferp)
    (my/allocate-window-height)))

(define-minor-mode my/allocate-window-mode
  "Automatically allocate window height when window selection changes."
  :init-value nil
  :global nil
  :lighter " maw"
  (if my/allocate-window-mode
      (add-hook 'window-selection-change-functions
                #'my/allocate-window--selection-hook
                nil t)
    (remove-hook 'window-selection-change-functions
                 #'my/allocate-window--selection-hook
                 t)))

;;;###autoload
(define-globalized-minor-mode my/allocate-window-global-mode
  my/allocate-window-mode
  (lambda () (my/allocate-window-mode 1)))

(my/allocate-window-global-mode 1)

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
