;;; init.el --- Minimal Emacs initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el and load configuration.

;;; Code:

;; Left at the standard nil: Org signals ordinary refusals with `error' -- "the
;; span is already day", "command not allowed in this line" -- and with this on,
;; each of them opens a backtrace over the buffer you were reading.  Debugging
;; wants it, so `M-x toggle-debug-on-error' turns it on for as long as it helps,
;; and `emacs --debug-init' still covers startup.

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
