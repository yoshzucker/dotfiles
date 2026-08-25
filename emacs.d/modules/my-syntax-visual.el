;;; my-syntax-visual.el --- Syntax behavior and language parsing setup -*- lexical-binding: t -*-

;;; Commentary:
;; Configures syntax-related behavior for editing across multiple modes.
;; Includes word boundary tweaks, Tree-sitter grammar integration,
;; and support for Flyspell, Flycheck, and other syntax-aware tools.

;;; Code:

(use-package eldoc
  :straight nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p t
        eldoc-idle-delay 0.25))

(use-package flyspell
  :if (executable-find "aspell")
  :diminish flyspell-mode
  :init
  (setq-default ispell-program-name "aspell")
  :config
  ;; Skip non-ASCII regions when checking spelling
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

  ;; `text-mode-hook\=' alone: org-mode derives from text-mode, so an Org buffer
  ;; runs it already and naming `org-mode-hook\=' beside it only calls
  ;; `flyspell-mode\=' a second time on the buffers where it costs most.
  (my/add-hook
   (:hook text-mode-hook
          :func #'flyspell-mode)))

(use-package flycheck
  :defer t
  :diminish (flycheck-mode "flyc")
  :init
  (my/add-hook
   (:hook python-mode-hook slime-lisp-mode-hook ess-mode-hook c-mode-common-hook
          :func #'flycheck-mode)
   (:hook org-mode-hook
          :func (lambda () (flycheck-mode -1)))))

(use-package flymake
  :defer t)

(use-package treesit
  :straight nil
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :init
  ;; Define language grammar sources
  (setq treesit-language-source-alist
        (append
         '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
           (c . ("https://github.com/tree-sitter/tree-sitter-c"))
           (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
           (python . ("https://github.com/tree-sitter/tree-sitter-python"))
           (json . ("https://github.com/tree-sitter/tree-sitter-json"))
           (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
           (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
           (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
           (yaml . ("https://github.com/ikatyang/tree-sitter-yaml")))
         (let ((path (expand-file-name "var/treesit/tree-sitter-swift" user-emacs-directory)))
           ;; https://github.com/alex-pinkus/tree-sitter-swift#where-is-your-parserc
           (when (file-directory-p path)
             `((swift . (,path)))))))

  ;; Remap major modes to Tree-sitter equivalents
  (setq major-mode-remap-alist
	    '((sh-mode     . bash-ts-mode)
          (c-mode      . c-ts-mode)
          (c++-mode    . c++-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode   . json-ts-mode)
          (js-mode     . js-ts-mode)
          ;; .ts/.tsx are registered in `auto-mode-alist' by typescript-ts-mode
          ;; itself once the grammars are available -- no remap needed.
          (yaml-mode   . yaml-ts-mode)))

  ;; The compiler grammars are built with is put on PATH in early-init.el,
  ;; where it has to be chosen anyway: native compilation needs the driver
  ;; that matches the libgccjit Emacs loaded, and two compilers at the front
  ;; of one PATH is one compiler too many.  See the comment there.
  (when (eq system-type 'windows-nt)
    ;; kill treesit remaps for these modes (Emacs 30.2)
    (dolist (src '(bash c cpp javascript python))
      (setq treesit-language-source-alist
            (assq-delete-all src treesit-language-source-alist)))
    (dolist (src '(sh-mode c-mode c++-mode js-mode python-mode))
      (setq major-mode-remap-alist
            (assq-delete-all src major-mode-remap-alist))))

  (defun my/treesit-install-grammars (&optional force)
    "Install the Tree-sitter grammars named above that are not there yet.
With FORCE (a prefix argument), install all of them again --
`treesit-install-language-grammar\=' has no force flag of its own, but a plain
install overwrites the library it finds.

A command, and deliberately not something startup does.  Finding out whether
a grammar is present means loading its shared library; finding out that it is
absent means cloning a repository and running a C compiler.  Neither belongs
in the way of a frame appearing, and both were there: nine `dlopen\='s measured
at 22ms on macOS, and a missing grammar turned a startup into a download.

What is given up is that a grammar can now be missing.  Emacs says so rather
than failing -- `treesit-ready-p\=' warns once and the mode falls back to its
non-tree-sitter self -- and this is the command that answers it."
    (interactive "P")
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (when (or force (not (treesit-language-available-p lang)))
        (treesit-install-language-grammar lang)))))

(provide 'my-syntax-visual)
;;; my-syntax-visual.el ends here
