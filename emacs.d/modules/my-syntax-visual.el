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

  (my/add-hook
   (:hook text-mode-hook org-mode-hook
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

  ;; Install grammars if not already available.  Skip entirely without a C
  ;; compiler, otherwise every startup warns once per language.  Same probe
  ;; list `treesit--install-language-grammar-1' uses.
  (when (seq-find #'executable-find '("cc" "gcc" "c99"))
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))

  (defun my/treesit-reinstall-all-grammars ()
    "Force reinstall all Tree-sitter language grammars."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      ;; No force flag exists: the 2nd arg is OUT-DIR, and a plain reinstall
      ;; already overwrites the existing library.
      (treesit-install-language-grammar lang))))

(provide 'my-syntax-visual)
;;; my-syntax-visual.el ends here
