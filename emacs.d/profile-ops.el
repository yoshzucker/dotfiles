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
  '((files             . 3)
    (headings-per-file . 315)
    (max-depth         . 6)
    (mean-depth        . 3.03)
    (scheduled         . 0.02)
    (deadline          . 0.00)
    (clocked           . 0.54)
    (clocks-per-entry  . 1.59)
    (tagged            . 0.05)
    (with-properties   . 0.54)
    (keyworded         . 0.53)
    (todo-open         . 161)
    (todo-done         . 343)
    (bytes-per-heading . 328))
  "What a generated corpus should look like.
Measured from a real one by `profile-ops', which prints both columns so
this can be corrected.  The fractions are of all headings.

These are the working corpus on the machine that has one: few files, three
hundred headings in each, half of them carrying a clock and a keyword and
almost none of them scheduled.  A corpus of notes that work has happened
against, in other words, rather than the list of appointments the earlier
guess here described -- which mattered, because what an agenda costs
follows the first shape and not the second.")

(defvar profile-ops-corpus-directory
  (expand-file-name "profile-ops-corpus/" temporary-file-directory)
  "Where the generated corpus is written.  Rebuilt on every run.")

(defvar profile-ops-buffer "*operation profile*"
  "Where the report goes.")

(defvar profile-ops--file (or load-file-name buffer-file-name)
  "Where this file is, which is how it finds a repository to read.")

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
         ;; Dates are made by adding days to now, never by counting them from
         ;; the calendar's own zero.  `time-to-days' answers in absolute
         ;; Gregorian days -- seven hundred and thirty-nine thousand of them
         ;; by now -- and `days-to-time' reads its argument as days since
         ;; 1970, so handing one to the other dated this corpus to the year
         ;; 3995.  Emacs formatted it here without complaint and refused it
         ;; on Windows, which is how it was found; what it cost everywhere
         ;; was an agenda measured over a corpus with nothing in range.
         (in-days (lambda (n) (time-add (current-time) (days-to-time n))))
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
                               (funcall in-days (- (funcall next 60) 30))))))
            (when (funcall chance (alist-get 'deadline shape))
              (insert (format "DEADLINE: %s\n"
                              (format-time-string
                               "<%Y-%m-%d %a>"
                               (funcall in-days (funcall next 30))))))
            (when (funcall chance (alist-get 'clocked shape))
              (insert ":LOGBOOK:\n")
              ;; A rate of 1.59 is one clock on every entry and a second on
              ;; three in five, not two on all of them.  Rounding made the
              ;; generated corpus carry a quarter more clock lines than the
              ;; real one at the same heading count, which is a quarter more
              ;; of the work every clock row of an agenda does.
              (let* ((rate (or (alist-get 'clocks-per-entry shape) 1))
                     (n (+ (floor rate)
                           (if (funcall chance (- rate (floor rate))) 1 0))))
                (dotimes (_ (max 1 n))
                (let ((day (format-time-string
                            "%Y-%m-%d %a"
                            (funcall in-days (- (funcall next 21))))))
                  (insert (format "CLOCK: [%s 09:00]--[%s 10:30] =>  1:30\n" day day)))))
              (insert ":END:\n"))
            (insert (make-string (max 0 (- target-bytes (- (point) entry-start))) ?x)
                    "\n\n"))))
        (push path written)))
    (nreverse written)))

;;; Measuring

(defvar profile-ops--rows nil)

(defvar profile-ops--notes nil
  "Things a measurement learned that are not a number of milliseconds.")

(defvar profile-ops--details nil
  "Alist of a row's label to the parts it broke into, each a label and seconds.
Kept apart from the rows so the parts stay under the whole: sorted in with
them, the pieces of a slow operation scatter through the pieces of a fast
one and the report reads as a list of unrelated facts.")

(defmacro profile-ops--time (label times &rest body)
  "Run BODY TIMES times under LABEL and record the mean.

Collects first, so a row starts from a known heap rather than from
whatever the row before it left."
  (declare (indent 2))
  `(progn
     (garbage-collect)
     (push (cons ,label (/ (benchmark-elapse (dotimes (_ ,times) (ignore-errors ,@body)))
                           (float ,times)))
           profile-ops--rows)))

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

      ;; What `C-c C-w' waits for.  The target list is rebuilt on every refile
      ;; unless `org-refile-use-cache' says otherwise, and the default targets
      ;; here reach every agenda file to six levels -- so this row and the
      ;; heading walk above should be within sight of each other.  Measured
      ;; against the agenda files alone: the `nil' entry in `org-refile-targets'
      ;; means the buffer point happens to be in, which here is the profiler's.
      (let ((org-refile-targets '((org-agenda-files :maxlevel . 6))))
        (profile-ops--time "refile: collect targets" 3 (org-refile-get-targets)))

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

;;; The operations a corpus has nothing to do with
;;
;; Saving, moving, splitting a window, reading a repository.  None of these
;; grows with the number of Org files, so measuring them against both corpora
;; would print the same number twice.  They are measured once, and everything
;; here that writes writes into the profiler's own directory.

;;; What the machine is, rather than what it did
;;
;; The timings say which operation is slow; these say why it could be.  On a
;; machine where starting a process is the expense, the question every row
;; here raises is the same one -- is this the number of processes or the work
;; inside them -- and it cannot be answered from a total.  So: what one
;; trivial process costs, how many a status starts, and the handful of git
;; settings that decide whether a status has to look at every file.

(defun profile-ops--git (&rest args)
  "Run git with ARGS and return its output, trimmed, or nil."
  (with-temp-buffer
    (and (eq 0 (apply #'process-file "git" nil t nil args))
         (string-trim (buffer-string)))))

(defun profile-ops--process-cost (directory)
  "Return what starting the most trivial process costs in DIRECTORY."
  (let* ((default-directory (or directory default-directory))
         (n 20)
         (command (if (eq system-type 'windows-nt)
                      '("cmd" "/c" "exit")
                    '("true")))
         (seconds
          (benchmark-elapse
            (dotimes (_ n)
              (ignore-errors
                (apply #'process-file (car command) nil nil nil (cdr command)))))))
    (format "%.1f ms  (%s)" (/ (* 1000 seconds) (float n))
            (string-join command " "))))

(defun profile-ops--environment (repo)
  "Return what REPO and the git behind it are, as an alist of strings."
  (let ((default-directory (or repo default-directory)))
    (list
     (cons "emacs" (format "%s on %s" emacs-version system-type))
     (cons "git executable"
           (or (and (boundp 'magit-git-executable) (symbol-value 'magit-git-executable))
               (executable-find "git") "not found"))
     (cons "git resolves to" (or (executable-find "git") "not found"))
     (cons "git version" (or (profile-ops--git "--version") "-"))
     (cons "repository" (abbreviate-file-name (or repo "-")))
     (cons "files it tracks"
           (let ((out (profile-ops--git "ls-files")))
             (if out (number-to-string (length (split-string out "\n" t))) "-")))
     (cons "core.fsmonitor" (or (profile-ops--git "config" "core.fsmonitor") "unset"))
     (cons "core.untrackedCache" (or (profile-ops--git "config" "core.untrackedCache") "unset"))
     (cons "core.preloadIndex" (or (profile-ops--git "config" "core.preloadIndex") "unset"))
     (cons "status.showUntrackedFiles"
           (or (profile-ops--git "config" "status.showUntrackedFiles") "unset"))
     (cons "one trivial git process"
           (let ((n 20))
             (format "%.1f ms"
                     (/ (* 1000 (benchmark-elapse
                                 (dotimes (_ n) (profile-ops--git "rev-parse" "--git-dir"))))
                        (float n)))))
     ;; The same question with git taken out of it.  If starting anything at
     ;; all costs what starting git costs, then no amount of choosing a
     ;; different git or configuring the one in hand will move a row: what
     ;; is being measured is the making of a process, and the only answer
     ;; open to a configuration is to ask for fewer of them.
     (cons "one trivial process, not git"
           (profile-ops--process-cost repo))
     ;; The same process started somewhere else.  A working directory the
     ;; system has to resolve through something -- a sync filter, a
     ;; redirected profile, a network drive -- is charged for on every
     ;; process started in it, and every git a status runs is started in the
     ;; repository.  If these two differ, where the repository lives is the
     ;; expense and no amount of asking for fewer sections reaches it.
     (cons "the same, from a plain directory"
           (profile-ops--process-cost temporary-file-directory))
     (cons "where the repository really is"
           (if repo (file-truename repo) "-")))))

(defun profile-ops--measure-editing (directory)
  "Time the operations that do not depend on the corpus.
DIRECTORY is where anything that has to be written goes.  The window
configuration is put back, the buffers made here are killed, and the one
repository read is read and not touched."
  (let ((profile-ops--rows nil)
        (recentf-exclude '(".*")))
    (setq profile-ops--notes nil
          profile-ops--details nil)
    (save-window-excursion
      ;; Saving.  What is being timed is the hooks, not the write: the file is
      ;; one line long and lives beside the corpus.
      ;;
      ;; The first save of a session is a row of its own for the same reason
      ;; the first agenda build is -- it is where whatever `after-save-hook'
      ;; names arrives, and averaging it into the nine after it reports a
      ;; number no save will ever take.  An Elisp buffer, because that is what
      ;; the formatters and checkers here are hooked onto.
      ;; Two rows and a warm-up between them, because there are two numbers.
      ;; The first save of a session is where `after-save-hook' fetches
      ;; whatever formats or checks; the dozen after it are where that
      ;; machinery finishes arriving -- measured here, ten saves cost 857 ms
      ;; and the ten after them 36.  A mean across that reports a figure no
      ;; save takes.  The warm-up is run and not recorded, so the second row
      ;; says what a save costs for the rest of the day.
      (let ((file (expand-file-name "profile-ops-save.el" directory)))
        (with-temp-file file (insert "(defun profile-ops--sample () nil)\n"))
        (with-current-buffer (find-file-noselect file)
          (profile-ops--time "save a file, the first of a session" 1
            (progn (insert "x") (save-buffer)))
          (dotimes (_ 12) (ignore-errors (insert "x") (save-buffer)))
          (profile-ops--time "save a file" 10
            (progn (insert "x") (save-buffer)))
          ;; And which of the things hooked onto a save takes it.  Each is
          ;; timed where it runs rather than called on its own, so whatever
          ;; it reads about the buffer reads true.  `t' in a buffer-local
          ;; hook means the global value as well, which is why both are
          ;; walked.
          (let ((timings nil)
                (hooks (append (remq t after-save-hook)
                               (and (memq t after-save-hook)
                                    (default-value 'after-save-hook)))))
            (let ((after-save-hook
                   (mapcar (lambda (fn)
                             (lambda ()
                               (push (cons fn (benchmark-elapse
                                               (ignore-errors (funcall fn))))
                                     timings)))
                           hooks)))
              (dotimes (_ 3) (insert "x") (save-buffer)))
            (dolist (cell (nreverse timings))
              (when (> (cdr cell) 0.002)
                (push (cons (format "%s" (car cell)) (cdr cell))
                      (alist-get "save a file" profile-ops--details
                                 nil nil #'equal)))))
          (set-buffer-modified-p nil)
          (kill-buffer))
        (ignore-errors (delete-file file)))

      ;; Windows.  A split and a delete is what every `C-x 2' costs, and on a
      ;; frame with a sill it is also a redraw of the sill.
      (profile-ops--time "split a window and close it" 20
        (progn (split-window-below) (other-window 1) (delete-window)))

      ;; Moving.  Per keystroke this is small and it is paid on every one, so
      ;; it is measured in hundreds and divided back down in the reading.
      (with-current-buffer (get-buffer-create " *profile-ops-motion*")
        (erase-buffer)
        (dotimes (i 500)
          (insert (format "line %d with a few words on it for a word motion\n" i)))
        (when (fboundp 'evil-local-mode) (evil-local-mode 1))
        (when (fboundp 'evil-normal-state) (evil-normal-state))
        (when (fboundp 'evil-next-line)
          (goto-char (point-min))
          (profile-ops--time "evil: 100 lines down" 5
            (progn (goto-char (point-min)) (evil-next-line 100)))
          (profile-ops--time "evil: 100 words forward" 5
            (progn (goto-char (point-min)) (evil-forward-word-begin 100))))
        (kill-buffer))

      ;; The repository this file is in, read once.  `magit-status' is the
      ;; slowest thing most days ask for that is not an agenda.
      ;;
      ;; Through `file-truename', because a configuration deployed as symlinks
      ;; is read at its link and lives at its source: the link's directory can
      ;; have a repository of its own -- an empty `git init' nobody meant --
      ;; and reading that one measures a directory with nothing in it.
      (when (fboundp 'magit-status-setup-buffer)
        (let* ((magit-row nil)
               (here (file-truename
                      (or (and (boundp 'profile-ops--file) profile-ops--file)
                          default-directory)))
               (repo (locate-dominating-file here ".git")))
          (when repo
            (profile-ops--time (setq magit-row
                                     (format "magit-status on %s"
                                             (file-name-nondirectory
                                              (directory-file-name repo))))
              3
              (magit-status-setup-buffer repo))
            ;; And which part of it, because on a machine where spawning is
            ;; the expense this row is the largest thing in the report and a
            ;; total names nothing.  magit keeps the arithmetic itself --
            ;; `magit-refresh-verbose' makes it time each section and say so
            ;; -- so this listens rather than measures, and reports the ones
            ;; worth a line.
            (let ((said nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) said)
                           nil)))
                (let ((magit-refresh-verbose t))
                  (ignore-errors (magit-status-setup-buffer repo))))
              (dolist (line (nreverse said))
                ;; Not anchored at the end: magit writes its own marks after
                ;; the number -- one `!' for slow and two for slower -- and
                ;; those are its judgement, which this has no business
                ;; requiring or repeating.
                (when (string-match
                       "\\`  \\([^ ]+\\) +\\([0-9]+\\.[0-9]+\\)" line)
                  (let ((seconds (string-to-number (match-string 2 line))))
                    (when (> seconds 0.01)
                      (push (cons (match-string 1 line) seconds)
                            (alist-get magit-row profile-ops--details
                                       nil nil #'equal)))))))
            ;; And how many processes that was.  With the cost of one in the
            ;; environment block above, this is the whole question: a status
            ;; that starts eighty of them on a machine where each costs a
            ;; tenth of a second is not slow git, it is arithmetic, and what
            ;; to do about it is to ask for fewer sections rather than to
            ;; tune the ones that are asked for.
            ;; And the same status as magit ships it, in the same run.  A
            ;; configuration that cuts sections wants to know what the
            ;; cutting bought, and the machine this matters on moves by half
            ;; again between one run and the next -- so the two have to be
            ;; measured beside each other or not at all.  `standard-value'
            ;; is magit's own list, which is the honest thing to compare
            ;; against: whatever this configuration has done to the hooks,
            ;; that is what it did it to.
            (when-let* ((sections (get 'magit-status-sections-hook 'standard-value))
                        (headers (get 'magit-status-headers-hook 'standard-value))
                        (stock-sections (eval (car sections) t))
                        (stock-headers (eval (car headers) t))
                        ;; Asked before the binding, not after: binding the
                        ;; variable is what makes the two agree.
                        (_ (not (equal stock-sections
                                       (default-value
                                        'magit-status-sections-hook)))))
              (let ((magit-status-sections-hook stock-sections)
                    (magit-status-headers-hook stock-headers))
                (profile-ops--time "the same, with magit's own sections" 3
                  (magit-status-setup-buffer repo))))

            ;; What magit's own memory already saves.  It keeps the answers
            ;; to git calls for the length of one refresh, so a question
            ;; asked twice is asked once -- which means the processes
            ;; counted below are distinct questions and not repetitions,
            ;; and that the way to have fewer of them is to ask for fewer
            ;; sections rather than to look for duplication that is already
            ;; gone.  The counters live in a variable magit binds and
            ;; discards, so it is bound here to be read afterwards.
            (let ((magit--refresh-cache (list (cons 0 0))))
              (ignore-errors (magit-status-setup-buffer repo))
              (push (cons "git calls magit's cache answered"
                          (let ((hits (caar magit--refresh-cache))
                                (misses (cdar magit--refresh-cache)))
                            (format "%d of %d" hits (+ hits misses))))
                    profile-ops--notes))
            (let ((spawns 0))
              (cl-letf* ((process-file-orig (symbol-function 'process-file))
                         (call-process-orig (symbol-function 'call-process))
                         ((symbol-function 'process-file)
                          (lambda (&rest args)
                            (setq spawns (1+ spawns))
                            (apply process-file-orig args)))
                         ((symbol-function 'call-process)
                          (lambda (&rest args)
                            (setq spawns (1+ spawns))
                            (apply call-process-orig args))))
                (ignore-errors (magit-status-setup-buffer repo)))
              (push (cons "processes one magit-status starts"
                          (number-to-string spawns))
                    profile-ops--notes))
            (dolist (buffer (buffer-list))
              (when (string-match-p "\\`magit" (buffer-name buffer))
                (kill-buffer buffer))))))

      ;; Capture, opened and thrown away.  A template of this profiler's own,
      ;; pointing at a file of its own: the user's templates write where they
      ;; are told to, and that is not somewhere a measurement may go.
      (when (fboundp 'org-capture)
        (let* ((file (expand-file-name "profile-ops-capture.org" directory))
               (org-capture-templates
                `(("p" "profile" entry (file+headline ,file "Inbox") "* %?\n"))))
          (profile-ops--time "capture: open a template and abort" 5
            (progn (org-capture nil "p") (org-capture-kill)))
          (dolist (buffer (buffer-list))
            (when (equal (buffer-file-name buffer) file)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer)))
          (ignore-errors (delete-file file)))))
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

(defun profile-ops--format-editing (rows)
  "Render ROWS as one column, longest first."
  (concat
   (format "  %10s   %s\n" "time" "operation")
   (format "  %10s   %s\n" "----------" "---------")
   (mapconcat
    (lambda (row)
      (concat
       (format "  %10s   %s" (format "%.1f ms" (* 1000 (cdr row))) (car row))
       (mapconcat
        (lambda (part) (format "\n  %10s     %s"
                               (format "%.1f ms" (* 1000 (cdr part))) (car part)))
        (sort (copy-sequence
               (alist-get (car row) profile-ops--details nil nil #'equal))
              (lambda (a b) (> (cdr a) (cdr b))))
        "")))
    (sort (copy-sequence rows) (lambda (a b) (> (cdr a) (cdr b))))
    "\n")
   "\n"))

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
         (synthetic (profile-ops--measure generated))
         (_ (message "profile-ops: measuring the operations no corpus reaches..."))
         (editing (profile-ops--measure-editing profile-ops-corpus-directory)))
    (with-current-buffer (get-buffer-create profile-ops-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Operation profile -- %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M")))
        (insert "The machine\n")
        (dolist (cell (profile-ops--environment
                       (and profile-ops--file
                            (locate-dominating-file (file-truename profile-ops--file) ".git"))))
          (insert (format "  %-26s %s\n" (car cell) (cdr cell))))
        (insert "\n")
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
                "every later build uses; the day and week rows are means of warm ones.\n")
        (insert "\nOperations a corpus has nothing to do with\n\n")
        (insert (profile-ops--format-editing editing))
        (insert "\nOne column, because none of these grows with the number of Org\n"
                "files.  The motion rows are a hundred keystrokes each: divide by a\n"
                "hundred for what one costs, and that is what is paid on every key.\n")
        (when profile-ops--notes
          (insert "\nCounted rather than timed\n\n")
          (dolist (cell (nreverse profile-ops--notes))
            (insert (format "  %-34s %s\n" (car cell) (cdr cell))))
          (insert "\nAgainst the cost of one process above, that count says whether an\n"
                  "operation is slow git or arithmetic.\n")))
      (goto-char (point-min))
      (special-mode))
    (message "profile-ops: done")
    (pop-to-buffer profile-ops-buffer)))

(provide 'profile-ops)

;;; profile-ops.el ends here
