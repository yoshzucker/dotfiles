;;; profile-ops.el --- Time what the configuration does, not what it loads -*- lexical-binding: t; -*-

;;; Commentary:
;; The companion to `profile-init.el', which ranks a startup.  This ranks the
;; work done afterwards: opening an Org file, building an agenda, running the
;; scans this configuration's own packages do.  Run it from a working Emacs:
;;
;;     M-x load-file RET ~/dotfiles/emacs.d/profile-ops.el RET
;;     M-x profile-ops RET
;;
;; It leaves a plain-text report in a buffer, which is the format it is in
;; because the way back from the machine that has the interesting corpus is a
;; paste into a mail.
;;
;; Two corpora, side by side.  The real one is the only honest answer and the
;; only one that cannot travel: it is read for its shape -- how many files, how
;; many headings, how many of them carry a clock -- and never for its content,
;; so the report can be pasted anywhere.  The synthetic one is built here from
;; that shape, and measuring both says how close the imitation got.  Where the
;; two columns disagree, the generator is missing something the real files do,
;; and `profile-ops-shape' is where to say what.
;;
;; Not a regression test.  A configuration that grows takes longer, and that is
;; not a fault; this is for the occasional look, to find the work nothing asked
;; for.  What it is good at is exactly what a timing has to have to be worth
;; acting on: the same numbers twice, and a name against each of them.
;;
;; It writes nothing but its own corpus, under `temporary-file-directory'.  The
;; real files are opened read-only, the id store is pointed at the corpus, and
;; the agenda is built into a buffer of this profiler's own.
;;
;; Two things it cannot undo, so it does not cause them.  The files it opens
;; would reach `recentf-list' and the entries it walks would reach
;; `org-id-locations', and both of those are written out when Emacs exits --
;; so the corpus it made would be in the list of recent files for weeks.  Both
;; are held off for the duration.

;;; Code:

(require 'benchmark)
(require 'org)
(require 'org-agenda)
(require 'seq)
(require 'cl-lib)
;; For `recentf-exclude', which has to be bound dynamically to be bound at all.
(require 'recentf)

(defvar profile-ops-shape
  '((files             . 60)
    (headings-per-file . 51)
    (max-depth         . 3)
    (scheduled         . 0.60)
    (deadline          . 0.30)
    (clocked           . 0.50)
    (clocks-per-entry  . 3)
    (tagged            . 0.50)
    (bytes-per-heading  . 1000))
  "What a generated corpus should look like.
Measured from a real one by `profile-ops', which prints both columns so
this can be corrected.  The fractions are of all headings.")

(defvar profile-ops-corpus-directory
  (expand-file-name "profile-ops-corpus/" temporary-file-directory)
  "Where the generated corpus is written.  Rebuilt on every run.")

(defvar profile-ops-buffer "*operation profile*"
  "Where the report goes.")

;;; Reading a corpus for its shape, and nothing else
;;
;; Counts only.  A heading's text, a tag's name and a property's value are all
;; capable of naming a person or a client, and this report is written to be
;; pasted into a mail, so none of them is read.  What comes out is arithmetic.

(defun profile-ops--shape-of (files)
  "Return the shape of FILES: how many of what, and nothing about any of it."
  (let ((headings 0) (scheduled 0) (deadline 0) (clocked 0) (clocks 0)
        (tagged 0) (properties 0) (bytes 0) (depths nil) (todo 0) (done 0)
        ;; A keyword is upper case and that is the whole of how it is told
        ;; from a word: folded, `[A-Z]' matches the H of "Heading" and every
        ;; heading in the corpus counts as a task.
        (case-fold-search nil))
    (dolist (file files)
      (setq bytes (+ bytes (or (file-attribute-size (file-attributes file)) 0)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((in-logbook nil))
          (while (not (eobp))
            (cond
             ((looking-at "^\\(\\*+\\) ")
              (setq headings (1+ headings)
                    in-logbook nil)
              (push (length (match-string 1)) depths)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (when (string-match-p ":[[:alnum:]_@#%:]+:[ \t]*$" line)
                  (setq tagged (1+ tagged)))
                (cond ((string-match-p "\\`\\*+ +DONE\\b" line) (setq done (1+ done)))
                      ((string-match-p "\\`\\*+ +[A-Z]\\{2,\\}\\b" line) (setq todo (1+ todo))))))
             ((looking-at "^[ \t]*SCHEDULED:") (setq scheduled (1+ scheduled)))
             ((looking-at "^[ \t]*DEADLINE:") (setq deadline (1+ deadline)))
             ((looking-at "^[ \t]*:LOGBOOK:")
              (setq clocked (1+ clocked) in-logbook t))
             ((and in-logbook (looking-at "^[ \t]*CLOCK:")) (setq clocks (1+ clocks)))
             ((looking-at "^[ \t]*:PROPERTIES:") (setq properties (1+ properties))))
            (forward-line 1)))))
    (let ((h (max headings 1)))
      (list (cons 'files (length files))
            (cons 'headings headings)
            (cons 'bytes bytes)
            (cons 'headings-per-file (/ headings (max (length files) 1)))
            (cons 'max-depth (if depths (apply #'max depths) 1))
            (cons 'mean-depth (if depths (/ (float (apply #'+ depths)) (length depths)) 0.0))
            (cons 'scheduled (/ (float scheduled) h))
            (cons 'deadline (/ (float deadline) h))
            (cons 'clocked (/ (float clocked) h))
            (cons 'clocks-per-entry (if (> clocked 0) (/ (float clocks) clocked) 0.0))
            (cons 'tagged (/ (float tagged) h))
            (cons 'with-properties (/ (float properties) h))
            (cons 'todo-open todo)
            (cons 'todo-done done)
            (cons 'keyworded (/ (float (+ todo done)) h))
            (cons 'bytes-per-heading (/ bytes h))))))

;;; Writing one that looks like it

(defun profile-ops--generate (directory shape)
  "Write a corpus into DIRECTORY shaped like SHAPE.  Return its files.

Deterministic: the same shape gives the same corpus, so two runs measure
the same thing and a difference between them is a difference in the code."
  (when (file-directory-p directory)
    (delete-directory directory t))
  (make-directory directory t)
  (let* ((files (alist-get 'files shape))
         (per (alist-get 'headings-per-file shape))
         (depth (max 1 (alist-get 'max-depth shape)))
         (target-bytes (alist-get 'bytes-per-heading shape))
         ;; Depth drawn so its mean lands where the real one did, rather than
         ;; uniformly over the range, which would sit halfway up it.  Twice the
         ;; mean is the width that has that mean, clamped to the depth seen.
         (mean-depth (or (alist-get 'mean-depth shape) 2.0))
         (depth-span (max 1 (min depth (round (- (* 2 mean-depth) 1)))))
         (today (time-to-days (current-time)))
         ;; A generator of its own, so the corpus does not depend on whatever
         ;; else in the session has drawn from the global one.
         ;; How many of the keyworded headings are still open.  A coin would
         ;; make the todo list twice the work it is here.
         ;; And not every heading carries one: a corpus of notes with a few
         ;; tasks in it is a different amount of work from a corpus of tasks.
         (keyworded (or (alist-get 'keyworded shape) 1.0))
         (open-share (let ((o (or (alist-get 'todo-open shape) 1))
                           (d (or (alist-get 'todo-done shape) 1)))
                       (if (> (+ o d) 0) (/ (float o) (+ o d)) 0.5)))
         (seed 20260921)
         (next (lambda (n) (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
                 (mod (/ seed 65536) n)))
         (chance (lambda (p) (< (funcall next 1000) (* p 1000))))
         (written nil))
    (dotimes (f files)
      (let ((path (expand-file-name (format "ops-%03d.org" f) directory)))
        (with-temp-file path
          (insert (format "#+title: Generated %d\n\n" f))
          (dotimes (h per)
            ;; The entry is built before it is padded: what the structure
            ;; costs counts toward the bytes the real corpus was measured at,
            ;; and guessing at it made the generated column the lighter one.
            (let ((entry-start (point)))
            (insert (format "%s%sHeading %d-%d%s\n"
                            (make-string (1+ (funcall next depth-span)) ?*)
                            (if (funcall chance keyworded)
                                (if (funcall chance open-share) " TODO " " DONE ")
                              " ")
                            f h
                            (if (funcall chance (alist-get 'tagged shape))
                                "  :generated:" "")))
            (when (funcall chance (alist-get 'with-properties shape))
              (insert (format ":PROPERTIES:\n:ID: ops-%d-%d\n:END:\n" f h)))
            (when (funcall chance (alist-get 'scheduled shape))
              (insert (format "SCHEDULED: %s\n"
                              (format-time-string
                               "<%Y-%m-%d %a 10:00-11:30>"
                               (days-to-time (+ today (- (funcall next 60) 30)))))))
            (when (funcall chance (alist-get 'deadline shape))
              (insert (format "DEADLINE: %s\n"
                              (format-time-string
                               "<%Y-%m-%d %a>"
                               (days-to-time (+ today (funcall next 30)))))))
            (when (funcall chance (alist-get 'clocked shape))
              (insert ":LOGBOOK:\n")
              (dotimes (_ (max 1 (round (alist-get 'clocks-per-entry shape))))
                (let ((day (format-time-string
                            "%Y-%m-%d %a"
                            (days-to-time (- today (funcall next 21))))))
                  (insert (format "CLOCK: [%s 09:00]--[%s 10:30] =>  1:30\n" day day))))
              (insert ":END:\n"))
            (insert (make-string (max 0 (- target-bytes (- (point) entry-start))) ?x)
                    "\n\n"))))
        (push path written)))
    (nreverse written)))

;;; Measuring

(defvar profile-ops--rows nil)

(defmacro profile-ops--time (label times &rest body)
  "Run BODY TIMES times under LABEL and record the mean."
  (declare (indent 2))
  `(push (cons ,label (/ (benchmark-elapse (dotimes (_ ,times) (ignore-errors ,@body)))
                         (float ,times)))
         profile-ops--rows))

(defun profile-ops--measure (files &optional first-in-session)
  "Time the operations this configuration does over FILES.
FIRST-IN-SESSION adds the costs a session pays once -- reading the files
cold, and the agenda build that fills the caches every later one uses.
Only the corpus measured first can be charged for either.
Returns an alist of label to seconds.  Nothing here writes to a file: the
agenda is built into a buffer of this profiler's own, the id store is
pointed at the corpus, and the window configuration is put back."
  (let ((profile-ops--rows nil)
        ;; Nothing opened here is a file anybody visited.
        (recentf-exclude '(".*"))
        (org-agenda-files files)
        (org-agenda-sticky nil)
        (org-agenda-buffer-name "*operation profile agenda*")
        (org-agenda-window-setup 'current-window)
        (org-id-track-globally nil)
        (org-id-locations-file
         (expand-file-name "profile-ops-id-locations.el" temporary-file-directory)))
    (save-window-excursion
      ;; Cold, like the first agenda build, is something a session has once.
      ;; The second corpus measured would be read through a warm Emacs and a
      ;; warm page cache, and the number would be one nothing will ever see.
      (when first-in-session
        (profile-ops--time "open every file, cold" 1
          (mapc #'find-file-noselect files)))
      (profile-ops--time "open every file, warm" 10
        (mapc #'find-file-noselect files))
      (profile-ops--time "walk every heading" 3
        (org-map-entries #'ignore nil files))

      ;; The first agenda of a session pays for caches every later one uses.
      ;; It can only be paid once, so it is a row for whichever corpus is
      ;; measured first and absent from the other -- the alternative is a
      ;; second column of the same number measured warm, which says nothing.
      (when first-in-session
        (profile-ops--time "agenda, the first build of a session" 1
          (org-agenda-list nil nil 'day)))
      (profile-ops--time "agenda day" 3 (org-agenda-list nil nil 'day))
      (profile-ops--time "agenda week" 2 (org-agenda-list nil nil 'week))
      (profile-ops--time "todo list" 3 (org-todo-list))

      (when (fboundp 'org-foresight-clock-scan)
        (profile-ops--time "foresight: clocks over a day" 3 (org-foresight-clock-scan 1))
        (profile-ops--time "foresight: clocks over a month" 2 (org-foresight-clock-scan 30)))
      (when (fboundp 'org-foresight-scan)
        (profile-ops--time "foresight: scan a day" 3 (org-foresight-scan 1))
        (profile-ops--time "foresight: scan a week" 2 (org-foresight-scan 7)))

      (when (and (fboundp 'consult--buffer-query)
                 (boundp 'consult-buffer-sources))
        (dolist (source (symbol-value 'consult-buffer-sources))
          (let* ((s (if (symbolp source) (symbol-value source) source))
                 (items (plist-get s :items))
                 (enabled (plist-get s :enabled)))
            (when (and (functionp items) (or (null enabled) (ignore-errors (funcall enabled))))
              (profile-ops--time (format "consult source: %s" (plist-get s :name)) 10
                (funcall items)))))))
    (when-let* ((buffer (get-buffer "*operation profile agenda*")))
      (kill-buffer buffer))
    (nreverse profile-ops--rows)))

;;; Reporting

(defun profile-ops--format-shape (label shape)
  "Render SHAPE under LABEL as lines of arithmetic."
  (concat
   (format "%s\n" label)
   (mapconcat
    (lambda (key)
      (let ((v (alist-get key shape)))
        (format "  %-22s %s" key
                (cond ((null v) "-")
                      ((floatp v) (format "%.2f" v))
                      ((eq key 'bytes) (file-size-human-readable v))
                      (t v)))))
    '(files headings bytes headings-per-file max-depth mean-depth
            scheduled deadline clocked clocks-per-entry tagged
            with-properties keyworded todo-open todo-done bytes-per-heading)
    "\n")
   "\n"))

(defun profile-ops--format-measurements (real synthetic)
  "Render REAL beside SYNTHETIC, longest first."
  (let ((labels (seq-uniq (append (mapcar #'car real) (mapcar #'car synthetic)))))
    (cl-flet ((weight (label)
                (max (or (alist-get label real nil nil #'equal) 0)
                     (or (alist-get label synthetic nil nil #'equal) 0))))
      (setq labels (sort labels (lambda (a b) (> (weight a) (weight b))))))
    (concat
     (format "  %10s  %10s   %s\n" "real" "generated" "operation")
     (format "  %10s  %10s   %s\n" "----------" "----------" "---------")
     (mapconcat
      (lambda (label)
        (let ((r (alist-get label real nil nil #'equal))
              (s (alist-get label synthetic nil nil #'equal)))
          (format "  %10s  %10s   %s"
                  (if r (format "%.0f ms" (* 1000 r)) "-")
                  (if s (format "%.0f ms" (* 1000 s)) "-")
                  label)))
      labels "\n")
     "\n")))

;;;###autoload
(defun profile-ops ()
  "Time what this configuration does, against the real corpus and a copy of it.

Leaves a report in `profile-ops-buffer', as plain text, to be read or
pasted.  Reads the real files for their shape only -- counts, never
content -- so the report carries nothing that cannot be sent."
  (interactive)
  (let* ((real-files (org-agenda-files))
         (_ (unless real-files
              (user-error (concat "`org-agenda-files' is empty, so there is no corpus "
                                  "to measure or to shape one after"))))
         (_ (message "profile-ops: reading the shape of %d files..." (length real-files)))
         (real-shape (profile-ops--shape-of real-files))
         (_ (message "profile-ops: generating a corpus like it..."))
         ;; The measured shape first: the defaults are what to use when there
         ;; is no corpus to read, not what to use instead of one.
         (generated (profile-ops--generate profile-ops-corpus-directory
                                           (append real-shape profile-ops-shape)))
         (generated-shape (profile-ops--shape-of generated))
         (_ (message "profile-ops: measuring the real corpus..."))
         (real (profile-ops--measure real-files 'first))
         (_ (message "profile-ops: measuring the generated one..."))
         (synthetic (profile-ops--measure generated)))
    (with-current-buffer (get-buffer-create profile-ops-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Operation profile -- %s\n%s on %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M")
                        emacs-version system-type))
        (insert (profile-ops--format-shape "Real corpus" real-shape))
        (insert "\n")
        (insert (profile-ops--format-shape "Generated corpus" generated-shape))
        (insert "\nWhere those two disagree, `profile-ops-shape' is not yet saying\n"
                "what the real files do.  Where the timings below disagree by more\n"
                "than the shapes do, something other than size is the difference.\n"
                "\nmax-depth is the exception: the generator aims at the mean and\n"
                "lets the ceiling fall where it will, since what a scan costs\n"
                "follows how deep headings are on average, not how deep one got.\n\n")
        (insert (profile-ops--format-measurements real synthetic))
        (insert "\nThe first agenda build is a row of its own because it fills caches\n"
                "every later build uses; the day and week rows are means of warm ones.\n"))
      (goto-char (point-min))
      (special-mode))
    (message "profile-ops: done")
    (pop-to-buffer profile-ops-buffer)))

(provide 'profile-ops)

;;; profile-ops.el ends here
