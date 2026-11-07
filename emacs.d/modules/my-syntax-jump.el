;;; my-syntax-jump.el --- Syntax-aware navigation and cross-referencing -*- lexical-binding: t; -*-

;;; Commentary:
;; Jumping to definitions and references through xref, with Vim-style keys.
;; Emacs Lisp answers from the running Emacs; C and Common Lisp fall to
;; etags, whose tags file `etags-regen-mode' keeps current.

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

(use-package etags-regen
  ;; Tag jumps with no language server: the first `gd' in a project runs
  ;; Emacs's own etags over its files, and the tags are brought up to date
  ;; as files are saved.  Turned on by the first C or Lisp buffer, the
  ;; languages it is for, so that a jump in an Org file never sets it to
  ;; reading the notes.
  :straight nil
  :defer t
  :init
  (my/add-hook
   (:hook c-mode-common-hook lisp-mode-hook
          :func (lambda () (etags-regen-mode 1))))
  :config
  ;; etags is what puts its backend on `xref-backend-functions', and
  ;; nothing else loads it: without this `gd' finds the tags regenerated and
  ;; no backend to read them.
  (require 'etags))

(provide 'my-syntax-jump)
;;; my-syntax-jump.el ends here
