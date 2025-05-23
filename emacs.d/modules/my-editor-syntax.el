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
(dolist (pair '((?_ . "w")
                (?- . "w")))
  (modify-syntax-entry (car pair) (cdr pair) (standard-syntax-table)))

;; In org-mode, restore `-` as punctuation for timestamp compatibility
(defun my/org-syntax-reset-dash ()
  (modify-syntax-entry ?- "." org-mode-syntax-table))
(add-hook 'org-mode-hook #'my/org-syntax-reset-dash)

;; Apply word syntax to selected Lisp-like modes
(dolist (table (list emacs-lisp-mode-syntax-table
                     lisp-mode-syntax-table))
  (dolist (pair '((?- . "_")
                  (?: . "_")
                  (?/ . "_")))
    (modify-syntax-entry (car pair) (cdr pair) table)))

;; Apply word syntax to minibuffer
(defun my/minibuffer-syntax ()
  "Make symbols like '.' and '/' word constituents in minibuffer."
  (let ((table (syntax-table)))
    (dolist (pair '((?. . "w")))
      (modify-syntax-entry (car pair) (cdr pair) table))))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-syntax)

(provide 'my-editor-syntax)
;;; my-editor-syntax.el ends here
