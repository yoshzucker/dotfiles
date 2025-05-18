;;; my-lang-shell.el --- Shell and terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhances shell and REPL environments in Emacs.
;; Provides keybindings for `comint-mode` and indentation settings for shell scripts.
;; Supports various shell interfaces such as `sh-mode`, `comint`, and `powershell`.
;; 
;; This file is focused on traditional shell support.
;; For vterm/eshell or REPL frameworks, consider creating separate modules if needed.

;;; Code:

(use-package comint
  :straight nil
  :defer t
  :config
  ;; Shell-like input navigation in insert state
  (my/define-key
   (:map comint-mode-map
         :state insert
         :key
         "C-j" #'comint-next-input
         "C-k" #'comint-previous-input)))

(use-package sh-script
  :straight nil
  :mode ("\\.sh\\'" . sh-mode)
  :config
  (defun my/setup-sh-indentation ()
    "Set consistent indentation for shell scripts and Tree-sitter bash."
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil)
    (setq-local sh-basic-offset 2)
    (setq-local sh-indentation 2)
    ;; Tree-sitter specific (bash-ts-mode)
    (when (boundp 'bash-ts-mode-indent-offset)
      (setq-local bash-ts-mode-indent-offset 2)))
  
  (my/add-hook
   (:hook sh-mode-hook bash-ts-mode-hook
          :func #'my/setup-sh-indentation)))

(use-package powershell
  :defer t)

(provide 'my-lang-shell)
;;; my-lang-shell.el ends here
