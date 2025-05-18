;;; my-syntax-jump.el --- Syntax-aware navigation and cross-referencing -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides semantic navigation support using xref, dumb-jump, eglot, etc.
;; Enables jumping to definitions, references, and symbol discovery with Vim-style bindings.

;;; Code:

(use-package xref
  :after evil
  :init
  ;; Vim-style keybindings for navigation
  (my/define-key
   (:map evil-motion-state-map
         :key
         "gd" #'xref-find-definitions
         "gD" #'xref-find-definitions-other-window
         "gr" #'xref-find-references
         "gR" #'my/xref-find-apropos
         "C-]" #'xref-find-definitions
         "C-t" #'xref-pop-marker-stack))
  :config
  ;; 
  (defun my/xref-backend-slime ()
    (when (bound-and-true-p slime-mode) 'slime))
  
  (defun my/xref-backend-eglot ()
    (when (bound-and-true-p eglot--managed-mode) 'eglot))
  
  (defun my/xref-backend-ggtags ()
    (when (bound-and-true-p ggtags-mode) 'ggtags))
  
  (defun my/xref-backend-dumb-jump ()
    (when (require 'dumb-jump nil t)
      (dumb-jump-xref-activate)
      'dumb-jump))
  
  (defun my/setup-xref-backends ()
    "Set up prioritized xref backends for the current buffer."
    (setq-local xref-backend-functions nil)
    (dolist (backend '(my/xref-backend-slime
                       my/xref-backend-eglot
                       my/xref-backend-ggtags
                       my/xref-backend-dumb-jump))
      (add-hook 'xref-backend-functions backend nil t)))
  
  (add-hook 'prog-mode-hook #'my/setup-xref-backends)

  ;; Avoid prompts when identifier is at point
  (setq xref-prompt-for-identifier
        '(not xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references
	      xref-find-apropos))

  (defun my/xref-find-apropos (pattern)
    "Find symbols matching PATTERN using xref and apropos."
    (interactive
     (list (read-string "Search for pattern (word list or regexp): "
                        (or (symbol-name (symbol-at-point)) "")
                        'xref--read-pattern-history)))
    (require 'apropos)
    (xref--find-xrefs pattern 'apropos
                      (apropos-parse-pattern
                       (if (string-equal (regexp-quote pattern) pattern)
                           (or (split-string pattern "[ \t]+" t)
                               (user-error "No word list given"))
                         pattern))
                      nil)))

(provide 'my-syntax-jump)
;;; my-syntax-jump.el ends here
