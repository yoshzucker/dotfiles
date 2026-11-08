;;; my-core-env.el --- Environment detection and locale setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Sets up essential environment configurations.

;;; Code:

;; Where packages keep their state: var/ and etc/ under `user-emacs-directory'.
;;
;; First, because no-littering sets those paths when it loads and a package
;; reads its path when it starts.  savehist and saveplace start in the
;; modules and read their files at once; behind no-littering they read the
;; files no-littering has moved, and ahead of it they read the old ones and
;; write the new, so what one session writes the next never reads.
(use-package no-littering
  :config
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backup/")))
	make-backup-files t
	backup-by-copying t
	version-control t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2))

(setq default-directory "~/"
      command-line-default-directory "~/"
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; The shell's PATH, for an Emacs started from the Dock rather than from a
;; shell: the one list, built by ~/.zshenv and its modules, rather than a
;; second copy of it written here.  A login shell and not an interactive
;; one -- ~/.zshenv has finished PATH before ~/.zshrc would start, and the
;; prompt and completion it would add are read by nothing here.  A few
;; milliseconds, since ~/.zshenv reads none of the system's startup files.
;; Not on Windows, where Emacs does not start a shell to learn a PATH.
(defun my/path-from-login-shell ()
  "Return PATH as a login shell sets it, or nil when the shell fails."
  (with-temp-buffer
    (and (eql 0 (ignore-errors
                  (call-process shell-file-name nil '(t nil) nil
                                "-l" "-c" "printf %s \"$PATH\"")))
         (> (buffer-size) 0)
         (buffer-string))))

(unless (eq system-type 'windows-nt)
  (when-let* (((or (display-graphic-p) (daemonp)))
              (path (my/path-from-login-shell)))
    (setenv "PATH" path)
    (setq exec-path (append (parse-colon-path path) (list exec-directory)))))

;; Git for Windows' own tools on Windows: `diff' for ediff, which is how
;; claude-code-ide shows a change, and the find and grep that xref and
;; `find-name-dired' run.  Windows has none of them, and Emacs there does
;; not start a shell to learn a PATH.  Ahead of the rest, so its find is
;; the one Emacs runs rather than Windows's own.  Where git is not there,
;; nothing is added, and those commands do not work.
(when (eq system-type 'windows-nt)
  (let ((usr (expand-file-name "~/scoop/apps/git/current/usr/bin")))
    (when (file-directory-p usr)
      (add-to-list 'exec-path usr)
      (setenv "PATH" (concat usr ";" (getenv "PATH"))))))

(when (native-comp-available-p)
  (add-hook 'kill-emacs-hook #'native-compile-prune-cache))

(provide 'my-core-env)
;;; my-core-env.el ends here
