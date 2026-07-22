;;; my-ui-splash.el --- Startup visuals and idle effects -*- lexical-binding: t; -*-
;;; Commentary:
;; Fancy splash art and idle zone animations.

;;; Code:

(use-package animate
  :demand t
  :custom
  ;; The whole art swoops in as ONE parallel pass (see `my/insert-art'), so
  ;; `animate-total-added-delay' is the total swoop time for the entire art,
  ;; and `animate-n-steps' is its smoothness (frames). Per-step compute is
  ;; negligible, so more steps is cheap. Tune these to taste.
  (animate-total-added-delay 0.6)
  (animate-n-steps 20)
  :config
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
    "Animate every line of ART into place in a single parallel pass.
All characters swoop in together (rather than one line at a time),
so the effect stays natural even at high speed.  DIRECTION only
selects each line's final row; with a single pass it no longer
affects ordering."
    (let* ((inhibit-read-only t)
           (lines (if (eq direction 'top-to-bottom) art (reverse art)))
           (ln (length lines))
           ;; Build one combined character list across all lines, each with
           ;; its own centered destination, then animate them all at once.
           (characters
            (cl-loop for line in lines
                     for i from 0
                     for vpos = (if (eq direction 'top-to-bottom) i (- ln i 1))
                     append (animate-initialize
                             line vpos
                             (max 0 (/ (- (window-width) (length line)) 2)))))
           (show-trailing-whitespace nil)
           (indent-tabs-mode nil))
      (goto-char (point-min))
      (open-line (1+ ln))
      (dotimes (i animate-n-steps)
        (let (buffer-undo-list list-to-undo)
          (animate-step characters (/ i 1.0 animate-n-steps))
          (set-window-start nil 1)
          (sit-for (/ (float animate-total-added-delay) (max animate-n-steps 1)))
          (setq list-to-undo buffer-undo-list)
          (while list-to-undo
            (let ((undo-in-progress t))
              (setq list-to-undo (primitive-undo 1 list-to-undo))))))
      (animate-step characters 1)
      (goto-char (point-min))))

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
                     ;; (setq-local face-remapping-alist
                     ;;             (assq-delete-all 'default face-remapping-alist))
                     (setq-local face-remapping-alist
                                 (mapcar (lambda (entry)
                                           (if (eq (car entry) 'default)
                                               (assq-delete-all :stipple entry)
                                             entry))
                                         face-remapping-alist))
                     (my/random-insert-art)))))))

  (when (eq system-type 'windows-nt)
    (advice-add 'my/random-insert-art :after (lambda () (evil-normal-state)))))

(provide 'my-ui-splash)
;;; my-ui-splash.el ends here
