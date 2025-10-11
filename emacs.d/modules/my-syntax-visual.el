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
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  ;; Remap major modes to Tree-sitter equivalents
  (setq major-mode-remap-alist
	'((sh-mode     . bash-ts-mode)
      (c-mode      . c-ts-mode)
      (c++-mode    . c++-ts-mode)
      (python-mode . python-ts-mode)
      (json-mode   . json-ts-mode)
      (js-mode     . js-ts-mode)
      (tsx-mode    . tsx-ts-mode)
      (yaml-mode   . yaml-ts-mode)))

  ;; Add gcc
  (when (eq system-type 'windows-nt)
    (let ((gcc-dir (expand-file-name "~/scoop/apps/gcc/current/bin/")))
      (when (file-exists-p (expand-file-name "gcc.exe" gcc-dir))
        (add-to-list 'exec-path gcc-dir)
        (setenv "PATH" (concat gcc-dir ";" (getenv "PATH")))))

    ;; kill treesit remaps for these modes (Emacs 30.2)
    (dolist (src '(bash c cpp javascript python))
      (setq treesit-language-source-alist
            (assq-delete-all src treesit-language-source-alist)))
    (dolist (src '(sh-mode c-mode c++-mode js-mode python-mode))
      (setq major-mode-remap-alist
            (assq-delete-all src major-mode-remap-alist))))

  ;; Install grammars if not already available
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang)))

  ;; Optional: Define a helper to reinstall all grammars
  (defun my/treesit-reinstall-all-grammars ()
    "Force reinstall all Tree-sitter language grammars."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (treesit-install-language-grammar lang t))))

(provide 'my-syntax-visual)
;;; my-syntax-visual.el ends here
