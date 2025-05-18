;;; my-editor-syntax.el --- Syntax editing behavior and enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Core syntax-related configurations.

;;; Code:

;; Parentheses and electric pair handling
(show-paren-mode t)
(electric-pair-mode t)

;; Indentation settings
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Treat `_` and `-` as word constituents globally
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))

;; In org-mode, restore `-` as punctuation for timestamp compatibility
(defun my/org-syntax-reset-dash ()
  (modify-syntax-entry ?- "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'my/org-syntax-reset-dash)

;; Apply word syntax to selected Lisp-like modes
(dolist (table (list emacs-lisp-mode-syntax-table
                     lisp-mode-syntax-table))
  (dolist (pair '((?- . "w")
                  (?: . "w")
                  (?/ . "w")))
    (modify-syntax-entry (car pair) (cdr pair) table)))

(provide 'my-editor-syntax)
;;; my-editor-syntax.el ends here
