;;; my-syntax-jump.el --- Syntax-aware navigation and cross-referencing -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides semantic navigation support using xref, dumb-jump, eglot, etc.
;; Enables jumping to definitions, references, and symbol discovery with Vim-style bindings.

;;; Code:

(use-package xref
  ;; Reached by the keys below, all of which name commands Emacs autoloads.
  :after evil
  :defer t
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

  ;; The backends and the hooks that register them, in `:init', because the
  ;; hook that would register one has run long before `gd' is pressed: in
  ;; `:config' the buffer in hand goes without its backend, silently, and
  ;; only buffers opened after the first jump get one.
  ;;
  ;; None of this needs xref loaded.  Each backend asks whether its mode is
  ;; on, with a guard, and `my/xref-add' adds buffer-locally -- which leaves
  ;; the global list alone, and that list is `etags--xref-backend' put there
  ;; by etags itself when etags loads.  Nothing of xref's is touched, so
  ;; nothing of xref's is lost by it not being here yet.
  (defun my/xref-backend-slime ()
    (when (bound-and-true-p slime-mode) 'slime))
  
  (defun my/xref-backend-eglot ()
    (when (bound-and-true-p eglot--managed-mode) 'eglot))

  (defun my/xref-backend-dumb-jump ()
    (when (require 'dumb-jump nil t)
      (dumb-jump-xref-activate)
      'dumb-jump))

  (defun my/xref-add (backend &optional append)
    (add-hook 'xref-backend-functions backend append t))

  (my/add-hook
   (:hook lisp-mode-hook
          :func
          (lambda () (my/xref-add #'my/xref-backend-slime t)))
   (:hook python-mode-hook python-ts-mode
          :func
          (lambda () (my/xref-add #'my/xref-backend-eglot t)))
   (:hook prog-mode-hook
          :func
          (lambda () (my/xref-add #'my/xref-backend-dumb-jump t))))
  :config
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
