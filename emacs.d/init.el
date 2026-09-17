;;; init.el --- Minimal Emacs initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el and load configuration.

;;; Code:

;; Left at the standard nil: Org signals ordinary refusals with `error' -- "the
;; span is already day", "command not allowed in this line" -- and with this on,
;; each of them opens a backtrace over the buffer you were reading.  Debugging
;; wants it, so `M-x toggle-debug-on-error' turns it on for as long as it helps,
;; and `emacs --debug-init' still covers startup.

;; What `load' probes for, on a machine where probing is expensive.
;;
;; Emacs finds a library by walking `load-path' and trying, in each directory,
;; every combination of `load-suffixes' and `load-file-rep-suffixes'.  With Auto
;; Compression mode on -- it is on by default -- that means `.elc' `.elc.gz'
;; `.el' `.el.gz' and the module suffixes doubled the same way: eight probes per
;; directory.  straight puts one build directory per package at the front of the
;; walk, and Emacs's own lisp sits at the end, so every bundled library is found
;; only after several hundred probes for names that were never going to be there.
;;
;; That is most of the startup on Windows, where one probe costs 0.22ms against
;; 0.003ms on macOS.  Measured there: 172 directories, 143ms to look one library
;; up, and `mail-prsvr' -- thirty-three lines -- taking 0.10s of a 39.7s startup,
;; all of it spent arriving.  Without the compressed half the same lookup took
;; 67ms.
;;
;; `jka-compr-load-suffixes' is the setting that adds that half, and setting it
;; through Custom runs `jka-compr-update', so the derived `load-file-rep-suffixes'
;; follows instead of being poked behind its owner's back.  Auto Compression mode
;; stays on: .gz files open and save exactly as before, and only the search for a
;; *library* stops considering them.
;;
;; Sound only where no lisp is shipped gzipped, which is worth testing rather
;; than assuming -- a distribution compresses all of its lisp or none of it, and
;; `subr.el' is the one file all of them have.
(let* ((subr (locate-library "subr"))
       (lisp (and subr (file-name-directory subr))))
  (when (and lisp (not (file-exists-p (expand-file-name "subr.el.gz" lisp))))
    (setopt jka-compr-load-suffixes nil)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; How straight decides a package needs rebuilding.  Must come before the first
;; `straight-use-package' below, and needs nothing earlier than that.
;;
;; The default includes `find-at-startup', and the bulk walk it names does not
;; happen when straight loads -- `straight--modifications' is a `memq' over
;; this variable, consulted per package inside
;; `straight--package-might-be-modified-p', and the walk itself is
;; `straight--make-package-modifications-available', a transaction step that
;; runs once, for the first package checked.  That first package is
;; `use-package', two lines down, which is why setting this after it had no
;; effect: the walk was already done and cached for the rest of the session.
;;
;; It is worth avoiding.  On Windows it walks every file of every cloned
;; repository before anything is loaded: thirty-one seconds to start, against
;; a sixth of a second for `emacs -Q'.  `check-on-save' catches every edit made
;; in this Emacs, which is all of them in ordinary use; `find-when-checking'
;; still walks a repository, but only the one being asked about and only when
;; it is asked about.  What is given up is noticing a repository edited by
;; something else while Emacs was not looking, which `M-x straight-check-all'
;; answers on demand.
(when (eq system-type 'windows-nt)
  (setq straight-check-for-modifications '(check-on-save find-when-checking)))

;; Where Emacs's own lisp sits in `load-path'.
;;
;; A library is found by walking `load-path' in order, probing each directory
;; for each candidate suffix.  straight prepends one build directory per
;; package, so Emacs's own lisp ends up behind all of them, and every bundled
;; library is found only after probing every one: measured on Windows at 67ms a
;; lookup against 0.4ms per directory, which was most of what a startup spent.
;;
;; Only a package that ships a library Emacs also bundles needs to be ahead of
;; Emacs's lisp.  Those few stay in front; the rest go behind it, leaving
;;
;;     [the shadowing packages] [Emacs's own lisp] [everything else]
;;
;; Behind, but at the head of it.  straight prepending is not arbitrary: a
;; package is required immediately after it is registered, so the front is
;; exactly where the directory about to be searched belongs, and appending
;; instead put every package at the back of the queue at the one moment it was
;; wanted -- measured at 57ms to find a package against 0.4ms before.
;;
;; Getting that list wrong is quiet.  The bundled copy wins, the installed
;; package becomes unreachable, and for something like `compat' that looks like
;; nothing at all until a function turns out to be missing.  So the list is
;; checked rather than trusted: `my/straight-warn-if-shadowing' runs on
;; `straight-use-package-post-build-functions', which fires on every install and
;; on every rebuild after a pull -- the only moments the answer can change.

(defconst my/emacs-own-load-path
  (let ((ours (expand-file-name user-emacs-directory)))
    (seq-remove (lambda (dir) (string-prefix-p ours (expand-file-name dir)))
                load-path))
  "Every directory of Emacs's bundled lisp, and nothing else.
Read before straight has added a package, so whatever lives under
`user-emacs-directory' at this point belongs to straight rather than Emacs.")

(defconst my/straight-packages-before-emacs-lisp
  '("bind-key" "compat" "eglot" "eldoc" "external-completion" "flymake"
    "jsonrpc" "let-alist" "map" "org" "peg" "project" "seq" "svg"
    "transient" "use-package" "xref")
  "Packages that must stay ahead of Emacs's own lisp in `load-path'.
Each ships at least one library Emacs also bundles, so behind Emacs's lisp
the bundled copy would win and the installed package would be unreachable.
Every other package straight builds is appended instead.")

(defun my/emacs-bundled-libraries ()
  "Return a hash table whose keys name every library Emacs itself bundles."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (dir my/emacs-own-load-path names)
      (dolist (file (ignore-errors (directory-files dir nil "\\.elc?\\'")))
        (puthash (file-name-base file) t names)))))

(defun my/straight-shadowed-libraries (package &optional bundled)
  "Return the libraries PACKAGE ships that Emacs also bundles.
BUNDLED is a table from `my/emacs-bundled-libraries', built here when
omitted.  Pass one when asking about many packages: building it walks every
directory of Emacs's lisp."
  (let ((bundled (or bundled (my/emacs-bundled-libraries)))
        (dir (straight--build-dir package))
        found)
    (dolist (file (and (file-directory-p dir)
                       (directory-files dir nil "\\.el\\'"))
                  (nreverse found))
      (let ((name (file-name-base file)))
        (when (gethash name bundled)
          (push name found))))))

(defun my/straight-warn-if-shadowing (package &rest _)
  "Warn when PACKAGE shadows Emacs's own lisp without being declared to."
  (unless (member package my/straight-packages-before-emacs-lisp)
    (when-let* ((shadowed (my/straight-shadowed-libraries package)))
      (display-warning
       'straight
       (format (concat "%s ships %s, which Emacs also bundles.  Its build "
                       "directory is appended to `load-path', so Emacs's copy "
                       "wins and the installed %s is unreachable.\n"
                       "Add %S to `my/straight-packages-before-emacs-lisp'.")
               package (string-join shadowed ", ") package package)
       :warning))))

(add-hook 'straight-use-package-post-build-functions
          #'my/straight-warn-if-shadowing)

(defun my/straight-check-load-path-shadows ()
  "Report every built package that shadows Emacs's own lisp undeclared.
The post-build hook covers packages built from here on; this covers the ones
already sitting in straight's build directory, which it has no reason to
rebuild."
  (interactive)
  (let* ((bundled (my/emacs-bundled-libraries))
         (build (straight--build-dir))
         (packages (seq-filter
                    (lambda (name)
                      (file-directory-p (expand-file-name name build)))
                    (directory-files build nil "\\`[^.]")))
         (undeclared
          (seq-filter
           (lambda (package)
             (and (not (member package my/straight-packages-before-emacs-lisp))
                  (my/straight-shadowed-libraries package bundled)))
           packages)))
    (if undeclared
        (message "Shadowing Emacs's own lisp, undeclared: %s"
                 (string-join undeclared " "))
      (message "No package shadows Emacs's own lisp undeclared (%d checked)"
               (length packages)))))

(define-advice straight--add-package-to-load-path
    (:around (orig recipe) my/behind-emacs-own-lisp)
  "Add the package's directory just behind Emacs's own lisp.
A package that has to shadow Emacs's lisp is prepended as straight would.
Anything else lands at the head of what follows Emacs's lisp, so it is still
the first package directory searched when it is required a moment later."
  (let* ((package (plist-get recipe :package))
         (dir (directory-file-name (straight--build-dir package)))
         (last-own (car (last my/emacs-own-load-path)))
         (at (and last-own (seq-position load-path last-own))))
    (cond
     ((member dir load-path) load-path)
     ;; No recognisable block of Emacs's own lisp to sit behind -- leave the
     ;; ordering to straight rather than guess at a position.
     ((or (null at)
          (member package my/straight-packages-before-emacs-lisp))
      (funcall orig recipe))
     (t (setq load-path (append (seq-take load-path (1+ at))
                                (list dir)
                                (seq-drop load-path (1+ at))))))))

;; Updating every package, without waiting for each in turn.
;;
;; `straight-pull-all' is `straight-fetch-all' followed by
;; `straight-merge-all', and the first of those is the whole of the wait: one
;; `git fetch' per repository, one after the next, each paying a network round
;; trip.  Measured at a second apiece over a hundred and ninety repositories,
;; and the same second whether the repository is four megabytes or forty --
;; the cost is the handshake and not the history, which is also why a shallow
;; clone would not help.  Three minutes of it, and the merging that follows
;; never touches the network at all.

(defvar my/straight-fetch-jobs 16
  "How many `git fetch' processes to have in flight at once.

Enough to hide the round trips behind each other, and not so many that a
rate limit or a laptop fan becomes the thing being measured.  The work is
waiting rather than computing, so this is not a count of cores.")

(defun my/straight-fetch-at-once ()
  "Run `git fetch' in every straight repository, several at a time.
Return the names of the repositories git refused, newest first.

Says how far it has got as it goes.  Sixteen seconds here is one machine
on one network, and neither is the slow case: a Windows box walking its
own antivirus for every object it writes takes a multiple of it, and a
command that goes quiet for minutes is one nobody can tell from a hung
one.  `make-progress-reporter' rate-limits the echo area itself, so this
costs a redraw every fifth of a second and not one per repository.

Whatever is still running is killed on the way out, so `C-g' leaves no
git processes behind to finish into a command that has gone."
  (unless (executable-find "git")
    (user-error "No git on PATH"))
  (let* (;; The echo area is this command\='s while it runs.  A progress
         ;; report that something else overwrites four times a second is
         ;; worse than none: the eye reads flicker as a fault.  Garbage
         ;; collection is the one that narrates over it here --
         ;; `garbage-collection-messages\=' is on globally, which is right
         ;; for a session and wrong for a command that has taken the line.
         (garbage-collection-messages nil)
         ;; And fewer of them to narrate.  Collecting does not stop the
         ;; fetching -- git is a separate process and goes on writing while
         ;; Emacs is busy -- it stops Emacs noticing that a fetch finished
         ;; and starting the next.  Measured at one collection of ten
         ;; milliseconds across the whole of a hundred and ninety
         ;; repositories, so this is for the quiet rather than the speed.
         ;;
         ;; Only until something lowers it again: `gcmh-mode\=' drops the
         ;; threshold from its own idle timer, and a wait this long looks
         ;; idle.  What that costs is collections, not messages, and they
         ;; are silent now.
         (gc-cons-threshold (max gc-cons-threshold (* 256 1024 1024)))
         (queue (seq-filter
                 (lambda (dir)
                   (and (file-directory-p dir)
                        ;; A worktree keeps a file there rather than a
                        ;; directory, and a repository of mine is a symlink
                        ;; to one I edit -- both are repositories to fetch.
                        (file-exists-p (expand-file-name ".git" dir))))
                 (directory-files (straight--repos-dir) t
                                  directory-files-no-dot-files-regexp)))
         (total (length queue))
         (began (float-time))
         (reporter (make-progress-reporter
                    (format "straight: fetching %d repositories..." total)
                    0 total))
         (procs nil)
         (live 0) (done 0) (failed nil))
    (unwind-protect
        (progn
          (while (or queue (> live 0))
            (while (and queue (< live my/straight-fetch-jobs))
              (let* ((dir (pop queue))
                     (name (file-name-nondirectory (directory-file-name dir)))
                     (default-directory dir))
                (setq live (1+ live))
                (push
                 (make-process
                  :name (concat "straight-fetch-" name)
                  :command '("git" "fetch" "--quiet")
                  :noquery t
                  :connection-type 'pipe
                  :buffer nil
                  ;; Nothing reads it, and a process whose output nobody
                  ;; drains can block on a full pipe.
                  :filter #'ignore
                  :sentinel
                  (lambda (proc _event)
                    (unless (process-live-p proc)
                      (setq live (1- live)
                            done (1+ done))
                      (unless (eq 0 (process-exit-status proc))
                        (push name failed))
                      (progress-reporter-update reporter done))))
                 procs)))
            ;; Short, because this loop is also what starts the next process
            ;; as each one finishes.
            (accept-process-output nil 0.05))
          (progress-reporter-done reporter)
          (message "straight: fetched %d repositories in %.0fs%s"
                   done (- (float-time) began)
                   (if failed
                       (format "; %d refused: %s" (length failed)
                               (string-join (reverse failed) ", "))
                     ""))
          failed)
      (dolist (proc procs)
        (when (process-live-p proc)
          (set-process-sentinel proc #'ignore)
          (delete-process proc))))))

(defun my/straight-pull-all (&optional from-upstream predicate)
  "Pull all packages, fetching them all at once rather than one after another.

A drop-in for `straight-pull-all', and the same two halves in the same
order: everything is fetched, then everything is merged.  Only the fetching
is done differently, by several git processes at once, which is what turns
three minutes of round trips into a quarter of one.

Nothing about what gets asked changes.  Every question the merge puts up --
a dirty worktree, a branch that has diverged, a merge left half-done --
comes from `straight-vc-git--ensure-local', which is local by design and
runs on the merge side.  They are asked after the waiting instead of
scattered through it, which is the only difference a reader will notice.

PREDICATE filters by package name as it does there.  Given one, this hands
the whole job to `straight-pull-all': a subset is a handful of round trips,
and there is nothing in a handful for parallelism to hide."
  (interactive "P")
  (if predicate
      (straight-pull-all from-upstream predicate)
    (my/straight-fetch-at-once)
    (straight-merge-all from-upstream)))

;; Install and use use-package via straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq use-package-verbose t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0)

;; Add core and module directories to load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load essential core modules
(require 'my-core-encoding)
(require 'my-core-env)
(require 'my-core-keys)
(require 'my-core-utils)

;; Load all module files in "modules/" directory
(dolist (file (directory-files (expand-file-name "modules" user-emacs-directory) t "\\.el$"))
  (let ((feature (intern (file-name-sans-extension (file-name-nondirectory file)))))
    (require feature)))

;; Load personal configuration if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;;; init.el ends here
