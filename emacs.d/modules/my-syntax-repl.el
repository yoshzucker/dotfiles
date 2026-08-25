;;; my-syntax-repl.el --- Context-aware REPL integration and code evaluation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides REPL integration and evaluation behavior for various programming languages.
;; Automatically binds keys (e.g., <C-return>) to appropriate `eval-in-repl` functions
;; based on `major-mode`, including support for tree-sitter variants.
;;
;; Supports Common Lisp (SLIME), Python, Scheme, Emacs Lisp, and more.
;; Evaluation dispatch is dynamic and extensible per mode.
;;
;; Also includes common keybindings and behavior improvements for `comint`-based shells.

;;; Code:

(use-package comint
  :straight nil
  :config
  (my/define-key
   (:map comint-mode-map
         :key
         "C-p" #'comint-previous-matching-input-from-input
         "C-n" #'comint-next-matching-input-from-input))

  (defun clear-shell ()
    "Clear the shell output buffer."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(use-package eval-in-repl
  ;; Loaded by the first evaluation, not by opening a file.  Resolving a
  ;; backend means `require\='ing it, and a `require\=' that finds nothing
  ;; searches every directory of `load-path\=' -- the most expensive lookup
  ;; there is, and one Emacs caches nothing about.  Done from `prog-mode-hook\='
  ;; that was paid on every programming buffer opened, for every language with
  ;; no backend, forever.
  :defer t
  :init
  (defvar my/eir-backends
    '((emacs-lisp-mode       . (eval-in-repl-ielm       . eir-eval-in-ielm))
      (ielm-mode             . (eval-in-repl-ielm       . eir-eval-in-ielm))
      (lisp-interaction-mode . (eval-in-repl-ielm       . eir-eval-in-ielm))
      (scheme-mode           . (eval-in-repl-scheme     . eir-eval-in-scheme))
      ;; Named for the REPL or for the language's full name rather than for
      ;; the mode, so the derived name misses them -- and these are the ones
      ;; actually used here.
      (lisp-mode             . (eval-in-repl-sly        . eir-eval-in-sly))
      (sh-mode               . (eval-in-repl-shell      . eir-eval-in-shell))
      (bash-ts-mode          . (eval-in-repl-shell      . eir-eval-in-shell))
      (js-mode               . (eval-in-repl-javascript . eir-eval-in-javascript))
      (js-ts-mode            . (eval-in-repl-javascript . eir-eval-in-javascript)))
    "Major mode to (LIBRARY . FUNCTION) for `eval-in-repl\='.
Only where the library cannot be derived from the mode name.  Everything
else -- python, ruby, racket -- follows the `eval-in-repl-LANGUAGE\=' /
`eir-eval-in-LANGUAGE\=' pattern and needs no entry.")

  (defvar my/eir--resolved (make-hash-table :test 'eq)
    "Modes already looked up, and what the lookup found, or `none\='.
Asked once per mode per session.  What is being cached is the failure: a
library that is not there costs a walk of the whole `load-path\=', and Emacs
remembers nothing about a `require\=' that did not succeed.")

  (defun my/eir--function (mode)
    "Return the `eval-in-repl\=' command for MODE, or nil when there is none."
    (let* ((entry (cdr (assq mode my/eir-backends)))
           (pair (or entry
                     (let ((base (replace-regexp-in-string
                                  "-ts\\|-mode\\'" "" (symbol-name mode))))
                       (cons (intern (format "eval-in-repl-%s" base))
                             (intern (format "eir-eval-in-%s" base)))))))
      (when (require (car pair) nil 'noerror)
        (and (fboundp (cdr pair)) (cdr pair)))))

  (defun my/eir-eval ()
    "Evaluate at point in this language\='s REPL.
Which command that is, is worked out on the first use in a session and
remembered."
    (interactive)
    (let ((known (gethash major-mode my/eir--resolved 'unknown)))
      (when (eq known 'unknown)
        (setq known (or (my/eir--function major-mode) 'none))
        (puthash major-mode known my/eir--resolved))
      (if (eq known 'none)
          (user-error "No eval-in-repl backend for %s" major-mode)
        (call-interactively known))))

  ;; Bound once in the mode map every programming mode inherits, rather than
  ;; per buffer from a hook: `local-set-key\=' writes into whichever map the
  ;; buffer happens to share, which for most modes is the major mode\='s own.
  (my/define-key
   (:map prog-mode-map :key "C-RET" #'my/eir-eval))
  :config
  (setq eir-repl-placement 'below))

(provide 'my-syntax-repl)
;;; my-syntax-repl.el ends here
