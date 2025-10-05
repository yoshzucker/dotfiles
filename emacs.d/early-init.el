;;; early-init.el --- Minimal early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Disable package.el in favor of straight.el.

;;; Code:

(setq package-enable-at-startup nil)
(setq straight-built-in-pseudo-packages
      '(project xref jsonrpc flymake external-completion eglot))

;;; early-init.el ends here
