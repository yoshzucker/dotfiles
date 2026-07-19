;;; my-editor-search.el --- Search and navigation enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; Migemo integration and Evil-compatible search adjustments.

;;; Code:

(use-package migemo
  :after evil
  :if (executable-find "cmigemo")
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
  
  (with-eval-after-load 'evil-search
    (setq evil-regexp-search nil)
    (defun my/evil-migemo-search-function (arg)
      (car (cl-sublis '((search-forward . migemo-forward)
                        (search-backward . migemo-backward))
                      (list arg))))
    (advice-add 'evil-search-function :filter-return #'my/evil-migemo-search-function))

  (migemo-init))

(use-package deadgrep
  :after evil
  :config
  (setq deadgrep-executable "rga")
  (setq deadgrep-extra-arguments
        (list "--no-config"
              (format "--rga-config-file=%s"
                      (expand-file-name "~/.config/ripgrep-all/config.jsonc"))))

  (when (eq system-type 'windows-nt)
    (add-to-list 'process-coding-system-alist
                 '("rg" utf-8-dos . cp932-dos)))

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
so pin the search root to DIR explicitly."
    (let* ((dir (expand-file-name dir))
           (default-directory dir)
           (deadgrep-project-root-function (lambda () dir)))
      (call-interactively #'deadgrep)))

  (defun my/deadgrep-memex ()     (interactive) (my/deadgrep-in org-directory))
  (defun my/deadgrep-reference () (interactive) (my/deadgrep-in my/deadgrep-reference-directory))
  (defun my/deadgrep-project ()   (interactive) (my/deadgrep-in my/deadgrep-project-directory))

  (evil-ex-define-cmd "memex"       #'my/deadgrep-memex)
  (evil-ex-define-cmd "ref[erence]" #'my/deadgrep-reference)
  (evil-ex-define-cmd "pro[ject]"   #'my/deadgrep-project))

(provide 'my-editor-search)
;;; my-editor-search.el ends here
