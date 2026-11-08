;;; my-lang-c.el --- C/C++ development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading C and C++: indentation and syntax.  No language server -- jumps
;; go through etags (my-syntax-jump.el).

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
	          (c-set-offset 'arglist-intro '+)
	          (c-toggle-hungry-state 1))))

(provide 'my-lang-c)
;;; my-lang-c.el ends here
