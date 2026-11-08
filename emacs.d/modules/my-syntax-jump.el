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

  ;; The search behind `project-find-regexp' -- `:rg' on the ex line
  ;; (my-editor-evil.el) -- run by ripgrep rather than grep.
  (setq xref-search-program 'ripgrep)

  ;; On Windows the search is a shell line, `xargs -0 rg', fed the file
  ;; names on its input, and Emacs talks to a shell there in cp932.  Git's
  ;; xargs and rg read and write UTF-8, so Japanese file names arrive
  ;; mangled and the matching lines come back unreadable.
  ;;
  ;; The same coding system encodes the shell line itself, which Windows
  ;; then reads as cp932: a Japanese pattern reached rg as mojibake and
  ;; matched nothing.  So the pattern goes as ASCII alone, each other
  ;; character written as rg's `\x{...}', which reads the same in any
  ;; coding system.
  (defun my/xref-ascii-regexp (regexp)
    "Write every non-ASCII character of REGEXP as ripgrep's \\x{HEX}."
    (replace-regexp-in-string
     "[^[:ascii:]]"
     (lambda (char) (format "\\x{%X}" (string-to-char char)))
     regexp t t))

  (when (eq system-type 'windows-nt)
    (define-advice xref-matches-in-files (:around (orig &rest args) my/utf-8)
      "Talk to the search in UTF-8."
      (let ((coding-system-for-read 'utf-8-dos)
            (coding-system-for-write 'utf-8-unix))
        (apply orig args)))
    (advice-add 'xref--regexp-to-extended :filter-return #'my/xref-ascii-regexp))

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
