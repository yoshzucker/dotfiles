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
                              "~/.local/bin/cmigemo/dict/cp932/migemo-dict")
	       migemo-directory (expand-file-name "~/.local/bin/cmigemo"))))
  
  (with-eval-after-load 'evil-search
    (setq evil-regexp-search nil)
    (defun my/evil-migemo-search-function (arg)
      (car (cl-sublis '((search-forward . migemo-forward)
                        (search-backward . migemo-backward))
                      (list arg))))
    (advice-add 'evil-search-function :filter-return #'my/evil-migemo-search-function))

  (migemo-init))

(use-package deadgrep
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'process-coding-system-alist
                 '("rg" utf-8-dos . cp932-dos))))

(provide 'my-editor-search)
;;; my-editor-search.el ends here
