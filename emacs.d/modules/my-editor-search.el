;;; my-editor-search.el --- Search and navigation enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; Migemo integration and Evil-compatible search adjustments.

;;; Code:

(use-package migemo
  :if (executable-find "cmigemo")
  ;; Started with evil, deliberately, after deferring it was tried and
  ;; measured.  Waiting costs more than it saves and costs it in a worse
  ;; place: cmigemo loads its dictionary in the background once `migemo-init'
  ;; has started it, so a session that starts it at the beginning answers its
  ;; first query in 7 ms.  Started on demand instead, the first query waits
  ;; for that dictionary -- about 250 ms with the package load -- and the
  ;; thing that asks first is not a search but the first prompt of the
  ;; session, because the orderless style in my-completion-minibuffer.el
  ;; goes through `migemo-get-pattern'.  84 ms at the start buys that.
  :after evil
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
    ;; The dictionary sits beside the program, in the prefix it was installed
    ;; into: share/ next to bin/.  Found from where cmigemo is rather than by
    ;; asking `brew --prefix', which costs a shell and a brew at every startup
    ;; to say the same thing.
    ('darwin
     (let ((dict (expand-file-name
                  "../share/migemo/utf-8/migemo-dict"
                  (file-name-directory (executable-find "cmigemo")))))
       (when (file-exists-p dict)
         (setq migemo-coding-system 'utf-8-unix
               migemo-dictionary dict))))
    ('windows-nt
     (setq migemo-coding-system 'cp932-unix
           migemo-dictionary (expand-file-name
                              "~/.local/share/cmigemo/dict/cp932/migemo-dict")
	       migemo-directory (expand-file-name "~/.local/share/cmigemo"))))
  
  (migemo-init))

(use-package deadgrep
  ;; Reached by `:dg' on the ex line (my-editor-evil.el), which searches the
  ;; project it is typed in.
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'process-coding-system-alist
                 '("rg" utf-8-dos . cp932-dos))))

(provide 'my-editor-search)
;;; my-editor-search.el ends here
