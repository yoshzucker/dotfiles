;;; profile-init.el --- Rank what a startup spends its time on -*- lexical-binding: t; -*-

;;; Commentary:
;; Not loaded by `init.el'.  Run it in place of a normal startup, from a shell,
;; in a GUI Emacs:
;;
;;     emacs -Q -l ~/dotfiles/emacs.d/profile-init.el
;;
;; It loads `early-init.el' and then `init.el' with `require' and `load' timed,
;; and leaves a report ranking files by the time spent in them *alone*, with
;; children excluded.  That is the column that names a culprit; the inclusive
;; one only names a container.  The alone column sums to the total, so the
;; table accounts for the whole startup rather than a sample of it.
;;
;; `-Q' is what makes this measurable: it skips both init files, because
;; `startup--load-user-init-file' only does its work when `init-file-user' is
;; non-nil, so they can be loaded here instead, under instrumentation.
;;
;; A GUI frame is required, not a convenience.  `:if (display-graphic-p)'
;; decides whether several packages load at all, so `--batch' would profile a
;; different Emacs than the one being complained about.
;;
;; Why not `use-package-report': its Elapsed column is inclusive, so a package
;; configured early absorbs everything loaded beneath it -- evil once showed
;; 23.21s of a 23.90s startup, which names the container and hides the cost.
;; It also sees only packages, while a `require' from a module and a `load'
;; from inside a package cost the same wall clock.
;;
;; `require' must be advised alongside `load'.  It is a subr and reaches
;; `Fload' in C, which never passes through advice on the `load' symbol: with
;; `load' alone, `(require 'calendar)' is invisible.  Autoload resolution goes
;; the same way and stays invisible even so -- its time is charged to whichever
;; frame was open at the time, which is why the CPU sampling profiler runs
;; alongside.  `M-x profiler-report' says what a row was doing.

;;; Code:

(defvar profile-init--stack nil
  "Frames currently open, innermost first.
Each element is (NAME . CHILD-SECONDS), where the cdr accumulates the
time of nested frames so it can be subtracted on the way out.")

(defvar profile-init--files (make-hash-table :test #'equal)
  "NAME to a vector of [ALONE-SECONDS TOTAL-SECONDS CALLS REQUIRER].
REQUIRER is whichever frame was open when this one was first entered, which
is the answer to \"who pulled this in\" -- the question a ranking on its own
always raises and never settles.")

(defun profile-init--time (name thunk)
  "Call THUNK, charging its wall time to NAME."
  (let ((start (float-time))
        (parent (car (car profile-init--stack))))
    (push (cons name 0.0) profile-init--stack)
    (unwind-protect (funcall thunk)
      (let* ((elapsed (- (float-time) start))
             (frame (pop profile-init--stack))
             (row (or (gethash name profile-init--files)
                      (puthash name (vector 0.0 0.0 0 parent)
                               profile-init--files))))
        (aset row 0 (+ (aref row 0) (- elapsed (cdr frame))))
        (aset row 1 (+ (aref row 1) elapsed))
        (aset row 2 (1+ (aref row 2)))
        ;; The caller pays for us in its total but not in its alone.
        (when profile-init--stack
          (setcdr (car profile-init--stack)
                  (+ (cdr (car profile-init--stack)) elapsed)))))))

(defun profile-init--load (orig file &rest args)
  "Time a `load' of FILE.  ORIG and ARGS are the advised call."
  (profile-init--time (if (stringp file) (abbreviate-file-name file)
                        (format "%s" file))
                      (lambda () (apply orig file args))))

(defun profile-init--require (orig feature &rest args)
  "Time a `require' of FEATURE.  ORIG and ARGS are the advised call.
A feature already present costs nothing and would only add a zero row."
  (if (and (symbolp feature) (not (featurep feature)))
      (profile-init--time (symbol-name feature)
                          (lambda () (apply orig feature args)))
    (apply orig feature args)))

(defun profile-init--shorten (name width)
  "Return NAME within WIDTH columns, keeping its tail."
  (let ((short (replace-regexp-in-string
                "\\`.*/straight/build/[^/]+/" "" (or name ""))))
    (if (<= (length short) width) short
      (concat "..." (substring short (- (length short) (- width 3)))))))

(defun profile-init--report (total)
  "Render the collected rows against a startup lasting TOTAL seconds."
  (let (rows)
    (maphash (lambda (name row) (push (cons name row) rows)) profile-init--files)
    (setq rows (sort rows (lambda (a b) (> (aref (cdr a) 0) (aref (cdr b) 0)))))
    (with-current-buffer (get-buffer-create "*startup profile*")
      (erase-buffer)
      (insert (format "Startup profile -- %.2f s over %d files\n%s on %s\n\n"
                      total (length rows) emacs-version system-type))
      (insert "  alone   within  what                            pulled in by\n")
      (insert "  ------  -------  ------------------------------  --------------------\n")
      (let ((shown 0) (rest 0.0))
        (dolist (row rows)
          (if (or (< shown 30) (>= (aref (cdr row) 0) 0.10))
              (progn
                (setq shown (1+ shown))
                (insert (format "  %6.2f  %7.2f  %-30s  %s\n"
                                (aref (cdr row) 0) (aref (cdr row) 1)
                                (profile-init--shorten (car row) 30)
                                (profile-init--shorten (or (aref (cdr row) 3) "-") 20))))
            (setq rest (+ rest (aref (cdr row) 0)))))
        (when (> (length rows) shown)
          (insert (format "  %6.2f           (%d further files)\n"
                          rest (- (length rows) shown)))))
      (insert "\nM-x profiler-report for what a row was doing.\n")
      (if noninteractive
          (princ (buffer-string))
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))))))

;; `early-init.el' is loaded before the instrumentation goes on, not under it.
;; Advising a primitive makes Emacs synthesize a native-compiled trampoline for
;; it, and on Windows the toolchain that compiles one is only reachable through
;; the PATH `early-init.el' sets.  Instrument first and the profiler's own
;; advice fails to compile, which is a report about the profiler.  Its time is
;; taken with a plain clock instead and shown as a row like any other.
(let* ((start (float-time))
       (early (expand-file-name "early-init.el" user-emacs-directory))
       (elapsed nil))
  (when (file-exists-p early)
    (load early nil 'nomessage))
  (setq elapsed (- (float-time) start))
  (puthash "early-init.el" (vector elapsed elapsed 1 nil) profile-init--files)

  (advice-add 'load :around #'profile-init--load)
  (advice-add 'require :around #'profile-init--require)
  (profiler-start 'cpu)
  ;; The total is the two loads added up, not the wall clock across this whole
  ;; form: the setup between them is the profiler's own cost and belongs to no
  ;; startup.  Leaving it out is what keeps the alone column summing to the
  ;; total, which is the property that makes the table exhaustive.
  (setq start (float-time))
  (load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)
  (setq elapsed (+ elapsed (- (float-time) start)))
  (profiler-stop)
  (advice-remove 'load #'profile-init--load)
  (advice-remove 'require #'profile-init--require)

  (profile-init--report elapsed))

;;; profile-init.el ends here
