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
  (dolist (pair '((?- . "w")
                  (?: . "w")
                  (?/ . "w")))
    (modify-syntax-entry (car pair) (cdr pair) table)))

;; Apply word syntax to minibuffer
(defun my/minibuffer-syntax ()
  "In the minibuffer, make symbols like '.' part of words,
and disable script-based word boundaries (e.g., between kanji and kana).
This enables word operations like `forward-word` and `backward-kill-word`
to treat sequences without breaking at script boundaries."
  (let ((table (copy-syntax-table (syntax-table))))
    (dolist (pair '((?. . "w")
                    (?\s . "w")
                    (?\( . "w")
                    (?\) . "w")))
      (modify-syntax-entry (car pair) (cdr pair) table))
    (set-syntax-table table))
  (setq-local char-script-table (make-char-table nil)))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-syntax)

(provide 'my-editor-syntax)
;;; my-editor-syntax.el ends here
