;;; my-emacs-ops.el --- Operational settings for core Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; How Emacs behaves as a process rather than as an editor: garbage
;; collection, the server, startup behaviour, warnings, what the machine
;; underneath is doing, and the other global runtime adjustments.

;;; Code:

(setq backup-directory-alist '((".*" . "~/.saves"))
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      ;; Off, and `gcmh-verbose' on below instead.  This narrates *every*
      ;; collection, which during a long command is a second writer fighting
      ;; whatever that command is reporting -- and flicker reads as a fault.
      ;; What is worth seeing is the deliberate collection and what it cost,
      ;; which is what `gcmh-verbose' says and this cannot.  For the totals,
      ;; `my/gc-report'.
      garbage-collection-messages nil)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package server
  :demand t
  :config
  ;; When emacsclientw.exe errors out (e.g. "handle is invalid" or "No socket"),
  ;; quit Emacs completely and delete ~/.emacs.d/server/server
  ;; (this is a common issue after Scoop updates on Windows) 
  (unless (server-running-p)
    (server-start)))

(use-package gcmh
  :diminish gcmh-mode
  :config
  ;; What gcmh is worth, measured on this machine's own agenda: ten builds,
  ;; five seconds, and the collections that happen inside them.
  ;;
  ;;   threshold   time   GCs   in GC
  ;;      781k     5.2s    12    0.37s   7%
  ;;       32M     5.1s     7    0.35s   7%
  ;;      128M     5.2s     6    0.36s   7%
  ;;      256M     5.1s     3    0.25s   5%
  ;;      512M     5.0s     1    0.14s   3%
  ;;        1G     4.8s     0    0.00s   0%
  ;;
  ;; The count falls all the way down and the time does not, because a
  ;; collection costs what the *live* heap costs to walk -- fewer of them,
  ;; each dearer.  What removes the cost is the threshold being higher than
  ;; the command conses, so that none happens at all and the bill is settled
  ;; later.  That is the whole of gcmh, and it is worth having.
  (setq gcmh-verbose t
        ;; The default, written down: `auto' recomputes it as twenty times
        ;; the last collection, which for a fast one is a third of a second.
        gcmh-idle-delay 15
        ;; Raised from Emacs's ancient 800k.  It buys no time -- see the
        ;; table -- but it is the threshold in force whenever the high one
        ;; has been let go of, and half as many collections there is half as
        ;; much interruption.
        gcmh-low-cons-threshold (* 32 1024 1024))

  ;; And the reason it was being let go of mid-command.  `gcmh-idle-garbage-collect'
  ;; is armed with `run-with-timer' from `post-command-hook', and a timer
  ;; fires whenever Emacs waits -- for a process, for `sit-for' -- not only
  ;; when the user has gone away.  So a command long enough to wait got
  ;; collected underneath it and, worse, left on the *low* threshold for the
  ;; rest of its run, because `post-command-hook' cannot fire until it
  ;; returns.  The command then pays for every 32 megabytes it conses, which
  ;; is exactly the case gcmh exists to prevent.
  ;;
  ;; So the collection waits for the command to finish.  Nothing is skipped:
  ;; the timer is simply armed again, and the work is done between commands,
  ;; which is where gcmh always meant to do it.
  (defvar my/gcmh--command-running nil
    "Non-nil between `pre-command-hook' and `post-command-hook'.")

  (defun my/gcmh--command-began () (setq my/gcmh--command-running t))
  (defun my/gcmh--command-ended () (setq my/gcmh--command-running nil))

  (define-advice gcmh-idle-garbage-collect
      (:around (orig) my/between-commands-only)
    "Collect between commands, never in the middle of one."
    (if my/gcmh--command-running
        (gcmh-register-idle-gc)
      (funcall orig)))

  (my/add-hook
   (:hook pre-command-hook  :func #'my/gcmh--command-began)
   (:hook post-command-hook :func #'my/gcmh--command-ended))

  (gcmh-mode 1))

(defvar my/gc--mark nil
  "Collections, seconds and clock as of the last `my/gc-report', or nil.")

(defun my/gc-report (&optional whole-session)
  "Say what garbage collection has cost since this was last asked.

Asked twice around something heavy -- once before, once after -- the
second answer is what that something paid, which is the question worth
asking.  A session total cannot answer it: nine tenths of a second spread
over half an hour is nothing, and the same nine tenths landing inside one
redraw is that redraw being a tenth slower than it looks.

With a prefix argument, or the first time, the whole session instead.

`garbage-collection-messages' is not an instrument for this: a line in the
echo area says a collection happened and nothing about whether they add up
to anything.  A percent or two is the price of not thinking about it; ten
is a reason to look at `gcmh-high-cons-threshold'."
  (interactive "P")
  (let* ((now (current-time))
         (from (if (or whole-session (null my/gc--mark))
                   (list 0 0.0 before-init-time)
                 my/gc--mark))
         (span (max 0.001 (float-time (time-subtract now (nth 2 from)))))
         (count (- gcs-done (nth 0 from)))
         (spent (- gc-elapsed (nth 1 from))))
    (setq my/gc--mark (list gcs-done gc-elapsed now))
    (message "gc: %d collections, %.2fs, %.1f%% of %s"
             count spent (* 100 (/ spent span))
             (if (< span 60)
                 ;; A decimal, because what is being timed here is often a
                 ;; single command and "0s" is not an answer about one.
                 (format "%.1fs" span)
               ;; `%z' drops whatever stands before it when all of it is
               ;; zero, so nine minutes reads as "0h 9m" and not as a year
               ;; and two days of nothing.
               (format-seconds "%Y %D %z%hh %mm" span)))))

(use-package immortal-scratch
  :config
  (setq eval-expression-print-length nil
        eval-expression-print-level nil)

  (immortal-scratch-mode 1))

(provide 'my-emacs-ops)
;;; my-emacs-ops.el ends here
