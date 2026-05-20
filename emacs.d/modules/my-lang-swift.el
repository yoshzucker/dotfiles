;;; my-lang-swift.el --- Swift configration -*- lexical-binding: t -*-

;;; Commentary:
;; Configures for Swift development in Emacs.
;; Includes word boundary tweaks, Tree-sitter grammar integration,
;; and support for Flyspell, Flycheck, and other syntax-aware tools.

;;; Code:
(use-package swift-ts-mode
  :mode ("\\.swift\\'" . swift-ts-mode))

(provide 'my-lang-swift)
;;; my-lang-swift.el ends here
