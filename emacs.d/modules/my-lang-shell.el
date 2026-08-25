;;; my-lang-shell.el --- Shell and terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing shell scripts: how they are indented, and the modes that read them.
;;
;; Not the shells themselves.  A process buffer is comint's, and comint is
;; configured in my-syntax-repl.el beside everything else that runs code and
;; reads back what it said.

;;; Code:

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
