;;; my-ui-splash.el --- Startup visuals and idle effects -*- lexical-binding: t; -*-
;;; Commentary:
;; Fancy splash art and idle zone animations.

;;; Code:

(defvar my/computer-art
  '("                                                     "
    "                                                     "
    "  |----------------------------------------------o+  "
    "  |.|-------------------------------------------| |  "
    "  | |  --  ---  -------    --        |-----+    | |  "
    "  | |  --\\- ---------  \\------       +-----|    | |  "
    "  | +--------------------------------+----------+ |  "
    "  +-+---------------------------------------------+  "
    "  | ---ooo o --   o   o   o   o   oo o ---    --- |  "
    "  |  o   o o   o  oo  o o o o   o  o  o o         |  "
    "  |  ooo oo  oo --ooo oo  o o o o o  oo   oo   o  |  "
    "  |  o  oo o o  o o      o---o  o   ooo  o oo     |  "
    "--+-------------------o-------------------------- |  "
    "                                                     "))

(defvar my/art-alist
  '((my/computer-art . bottom-to-top)))

(defun my/insert-art (art direction)
  (let ((inhibit-read-only t)
        (lines (if (eq direction 'top-to-bottom) art (reverse art))))
    (goto-char (point-min))
    (open-line (1+ (length lines)))
    (cl-loop with ln = (length lines)
             for line in lines
             for i from 0
             for l = (if (eq direction 'top-to-bottom) i (- ln i 1))
             do (animate-string line l))
    (goto-char (point-max))))

(defun my/random-insert-art (&optional buffer)
  "Insert a random ASCII art into BUFFER (or current buffer)."
  (let* ((entry (nth (cl-random (length my/art-alist)) my/art-alist))
         (art (symbol-value (car entry)))
         (direction (cdr entry)))
    (with-current-buffer (or buffer (current-buffer))
      (my/insert-art art direction))))

(advice-add 'fancy-splash-head :override (lambda () ""))
(add-hook 'window-setup-hook
          (lambda ()
            (run-with-timer
             0.5 nil
             (lambda ()
               (when-let ((buf (get-buffer "*GNU Emacs*")))
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   ;; Clear any unwanted face remapping like :stipple
                   (setq-local face-remapping-alist
                               (assq-delete-all 'default face-remapping-alist))
                   (my/random-insert-art)))))))

(when (eq system-type 'windows-nt)
  (advice-add 'my/random-insert-art :after (lambda () (evil-normal-state))))

(provide 'my-ui-splash)
;;; my-ui-splash.el ends here
