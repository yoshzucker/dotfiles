;;; my-lang-misc.el --- Minor languages and filetype modes -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures support for small or auxiliary file formats
;; such as Dockerfile, YAML, etc., that do not warrant a dedicated file.

;;; Code:

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :config
  (setq dockerfile-indent-offset 2))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (setq yaml-indent-offset 2))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown"))

(provide 'my-lang-misc)
;;; my-lang-misc.el ends here
