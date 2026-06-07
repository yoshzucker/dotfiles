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

(my/allocate-window-global-mode -1)

(defun my/non-side-windows ()
  "Return a list of non-side windows, excluding sidebars and Treemacs."
  (seq-filter
   (lambda (win)
     (let ((buf (window-buffer win)))
       (and (not (window-parameter win 'window-side))
            (not (string-prefix-p "*Treemacs" (buffer-name buf)))
            (not (eq (buffer-local-value 'major-mode buf) 'treemacs-mode)))))
   (window-list nil 'nomini)))

(defun my/toggle-window-split-direction ()
  "Toggle the split direction of exactly two non-side windows, ignoring any sidebars."
  (interactive)
  (pcase (my/non-side-windows)
    (`(,win1 ,win2)
     (let* ((horizontal (eq (cadr (window-edges win1))
                            (cadr (window-edges win2))))
            (split-fn   (if horizontal
                            #'split-window-vertically
                          #'split-window-horizontally))
            (selected   (selected-window))
            (base       (if (memq selected (list win1 win2)) selected win1))
            (other      (if (eq base win1) win2 win1))
            (other-buf  (window-buffer other))
            (other-start (window-start other)))

       (select-window base)
       (delete-window other)
       (let ((new-win (funcall split-fn)))
         (set-window-buffer new-win other-buf)
         (set-window-start new-win other-start))
       (balance-windows)))

    (_ (user-error "There are not exactly two non-side windows"))))

(my/define-key
 (:map evil-window-map
       :after evil
       :key
       "q" #'my/toggle-window-split-direction))

(provide 'my-ui-window)
;;; my-ui-window.el ends here
