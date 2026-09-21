;;; my-editor-search.el --- Search and navigation enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; Migemo integration and Evil-compatible search adjustments.

;;; Code:

(use-package migemo
  :if (executable-find "cmigemo")
  ;; Reached by a search and by nothing else: the advice below hands evil
  ;; `migemo-forward' in place of `search-forward', and calling it is what
  ;; brings the package.  Starting here rather than at startup keeps two
  ;; subprocesses out of the start -- `brew --prefix' to find the dictionary,
  ;; and cmigemo itself -- and a session that searches nothing starts neither.
  ;;
  ;; `:commands' because migemo autoloads nothing of its own, and an advice
  ;; that returns the name of a function nobody has defined returns nothing.
  :defer t
  :commands (migemo-forward migemo-backward)
  :init
  ;; In `:init' so that the first `/' is already a migemo search.  Left in
  ;; `:config' it would take a search to install the advice that a search is
  ;; supposed to go through.
  (with-eval-after-load 'evil-search
    (setq evil-regexp-search nil)
    (defun my/evil-migemo-search-function (arg)
      (car (cl-sublis '((search-forward . migemo-forward)
                        (search-backward . migemo-backward))
                      (list arg))))
    (advice-add 'evil-search-function :filter-return #'my/evil-migemo-search-function))
  :config
  (setq migemo-command "cmigemo"
        migemo-options '("--quiet" "--emacs")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil)

  ;; OS-specific dictionary and encoding
  (pcase system-type
    ((or 'darwin 'gnu/linux)
     (when (executable-find "brew")
       (setq migemo-coding-system 'utf-8-unix
             migemo-dictionary
             (expand-file-name
              "share/migemo/utf-8/migemo-dict"
              (string-trim
               (shell-command-to-string "brew --prefix cmigemo"))))))
    ('windows-nt
     (setq migemo-coding-system 'cp932-unix
           migemo-dictionary (expand-file-name
                              "~/.local/share/cmigemo/dict/cp932/migemo-dict")
	       migemo-directory (expand-file-name "~/.local/share/cmigemo"))))
  
  (migemo-init))

(use-package deadgrep
  ;; A search, started by `:memex', `:reference' or `:project' on the ex line
  ;; and by nothing else.  Those three have to be typeable from the start, so
  ;; they and the functions behind them are in `:init'; how the search runs is
  ;; in `:config', which the search itself brings on.
  :after evil
  :defer t
  :init
  (defcustom my/deadgrep-reference-directory
    (file-name-as-directory "~/Documents/reference/")
    "Directory searched by `my/deadgrep-reference'."
    :type 'directory
    :group 'deadgrep)

  (defcustom my/deadgrep-project-directory
    (file-name-as-directory "~/Documents/project/")
    "Directory searched by `my/deadgrep-project'."
    :type 'directory
    :group 'deadgrep)

  (defun my/deadgrep-in (dir)
    "Run `deadgrep' rooted at DIR, ignoring any enclosing VC/project root.
`deadgrep--project-root' otherwise walks up to a parent `.git' (e.g. ~/.git),
so pin the search root to DIR explicitly.

The `require' is not ceremony.  `deadgrep-project-root-function' is only a
special variable once deadgrep has defined it, and a `let' over a name that
is not special yet binds it lexically -- which deadgrep would never see, and
which would fail by searching the wrong tree rather than by complaining."
    (require 'deadgrep)
    (let* ((dir (expand-file-name dir))
           (default-directory dir)
           (deadgrep-project-root-function (lambda () dir)))
      (call-interactively #'deadgrep)))

  (defun my/deadgrep-memex ()     (interactive) (my/deadgrep-in org-directory))
  (defun my/deadgrep-reference () (interactive) (my/deadgrep-in my/deadgrep-reference-directory))
  (defun my/deadgrep-project ()   (interactive) (my/deadgrep-in my/deadgrep-project-directory))

  (evil-ex-define-cmd "memex"       #'my/deadgrep-memex)
  (evil-ex-define-cmd "ref[erence]" #'my/deadgrep-reference)
  (evil-ex-define-cmd "pro[ject]"   #'my/deadgrep-project)

  :config
  (setq deadgrep-executable "rga")
  (setq deadgrep-extra-arguments
        (list "--no-config"
              (format "--rga-config-file=%s"
                      (expand-file-name "~/.config/ripgrep-all/config.jsonc"))))

  (when (eq system-type 'windows-nt)
    (add-to-list 'process-coding-system-alist
                 '("rg" utf-8-dos . cp932-dos))))

(provide 'my-editor-search)
;;; my-editor-search.el ends here
