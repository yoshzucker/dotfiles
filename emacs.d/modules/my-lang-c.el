;;; my-lang-c.el --- C/C++ development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for C and C++ development in Emacs.
;; Includes indentation and syntax tweaks.  The language server is started
;; from my-syntax-lsp.el, with the other languages', and only when clangd is
;; installed.

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
  ;; The global mode checks `preproc-font-lock-modes' in every buffer opened;
  ;; the hook asks the same question once, of the buffers it can be true of.
  :defer t
  :init
  (my/add-hook
   (:hook c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
          :func #'preproc-font-lock-mode)))

(provide 'my-lang-c)
;;; my-lang-c.el ends here
