;;; my-lang-shell.el --- Shell and terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing shell scripts: how they are indented, and the modes that read them.
;;
;; Not the shells themselves.  A process buffer is comint's, and comint is
;; configured in my-syntax-repl.el.

;;; Code:

(use-package sh-script
  :straight nil
  :mode ("\\.sh\\'" . sh-mode)
  :config
  (defun my/setup-sh-indentation ()
    "Set consistent indentation for shell scripts."
    (setq-local tab-width 2)
    (setq-local indent-tabs-mode nil)
    (setq-local sh-basic-offset 2)
    (setq-local sh-indentation 2))
  
  (my/add-hook
   (:hook sh-mode-hook
          :func #'my/setup-sh-indentation)))

(provide 'my-lang-shell)
;;; my-lang-shell.el ends here
