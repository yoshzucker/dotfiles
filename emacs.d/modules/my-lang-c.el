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

  (defcustom my/use-ggtags-for-c t
    "Whether to enable ggtags-mode in C/C++ modes."
    :type 'boolean)
  
  (add-hook 'c-mode-common-hook
	        (lambda ()
	          (c-set-offset 'innamespace 0)
	          (c-set-offset 'arglist-intro '+)
	          (when my/use-ggtags-for-c
		        (ggtags-mode 1)))))

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

(use-package ggtags
  :if (eq system-type 'windows-nt)
  :config
  (defun my/xref-ggtags-windows-patch ()
    "Workarounds for ggtags on Windows shell and encoding issues."
    ;; Disable shell-quote-argument
    (defun my/no-shell-quote (f &rest args)
      (cl-letf (((symbol-function 'shell-quote-argument) (lambda (x) x)))
        (apply f args)))
    (advice-add 'ggtags-global-build-command :around #'my/no-shell-quote)
    (advice-add 'ggtags--xref-find-tags :around #'my/no-shell-quote)
    
    (defun my/gtags-fix-process-coding (f &rest args)
      (let* ((c buffer-file-coding-system)
             (i (cond ((string-match-p "cp932" (symbol-name c)) 'cp932-mac)
		      ((string-match-p "utf-8" (symbol-name c)) 'utf-8-mac)
		      (t c)))
             (process-coding-system-alist `(("global" ,i . ,i))))
        (apply f args)))
    (advice-add 'ggtags--xref-find-tags :around #'my/gtags-fix-process-coding))

  (my/xref-ggtags-windows-patch))

(provide 'my-lang-c)
;;; my-lang-c.el ends here
