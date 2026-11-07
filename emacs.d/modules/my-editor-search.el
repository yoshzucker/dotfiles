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
  ;; Reached by `g/' (my-completion-minibuffer.el), which searches the
  ;; project, and by `:source' on the ex line, which searches org-moor's
  ;; sources -- left out of the notes' project only by living apart.
  ;; `:source' has to be typeable from the start, so it and the function
  ;; behind it are in `:init'.
  :after evil
  :defer t
  :init
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

  (defun my/deadgrep-source ()
    "Search org-moor's sources with deadgrep."
    (interactive)
    (my/deadgrep-in (if (boundp 'org-moor-sources-directory)
                        org-moor-sources-directory
                      (expand-file-name "sources/" org-directory))))

  (evil-ex-define-cmd "source" #'my/deadgrep-source)

  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'process-coding-system-alist
                 '("rg" utf-8-dos . cp932-dos))))

(provide 'my-editor-search)
;;; my-editor-search.el ends here
