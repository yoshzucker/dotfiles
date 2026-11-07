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

;; Load shell environment variables (for GUI Emacs)
;;
;; A login shell, not an interactive one.  The package's default asks for both,
;; which runs the whole of ~/.zshrc to read a PATH that ~/.zshenv has already
;; finished setting -- the same twenty-four entries either way, checked against
;; each other.  What the interactive half adds is the prompt and the
;; completion system, which nothing here reads.  Measured at 74 ms against
;; 29 ms.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

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

(defconst my/wsl-p
  (and (eq system-type 'gnu/linux)
       (file-readable-p "/proc/version")
       (let ((case-fold-search t))
         (string-match-p "microsoft"
                         (with-temp-buffer
                           (insert-file-contents "/proc/version")
                           (buffer-string)))))
  "Non-nil if running under Windows Subsystem for Linux.")

(when (native-comp-available-p)
  (add-hook 'kill-emacs-hook #'native-compile-prune-cache))

(provide 'my-core-env)
;;; my-core-env.el ends here
