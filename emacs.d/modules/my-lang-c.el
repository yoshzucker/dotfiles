;;; my-lang-c.el --- C/C++ development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for C and C++ development in Emacs.
;; Includes indentation, Eglot LSP setup, and syntax tweaks.

;;; Code:

(use-package cc-mode
  :straight nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)

  (add-hook 'c-mode-common-hook
	        (lambda ()
	          (c-set-offset 'innamespace 0)
	          (c-set-offset 'arglist-intro '+))))

(use-package preproc-font-lock
  :config
  (preproc-font-lock-global-mode 1))

(use-package eglot
  :after cc-mode
  :config
  (my/add-hook
   (:hook c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
          :func #'eglot-ensure))

  ;; Language server association
  (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (add-to-list 'eglot-server-programs
                 `(,mode . ("clangd")))))

(provide 'my-lang-c)
;;; my-lang-c.el ends here
