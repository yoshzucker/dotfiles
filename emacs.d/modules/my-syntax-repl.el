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
  :defer t
  :init
  (defvar my/eir-special-mode-alist
    '((emacs-lisp-mode     . (eval-in-repl-ielm   . eir-eval-in-ielm))
      (ielm-mode           . (eval-in-repl-ielm   . eir-eval-in-ielm))
      (lisp-interaction-mode . (eval-in-repl-ielm . eir--in-ielm))
      (scheme-mode         . (eval-in-repl-scheme . eir-eval-in-geiser)))
    "Alist of major modes and their corresponding eval-in-repl package and function.")

  (defun my/eir-dispatch-key (key)
    "Bind KEY to suitable `eval-in-repl` function for `major-mode`."
    (let* ((entry (assoc major-mode my/eir-special-mode-alist))
           (pkg-fn (or entry
                       (let* ((mode-name (symbol-name major-mode))
                              (base (replace-regexp-in-string "-ts\\|-mode\\'" "" mode-name)))
			 (cons (intern (format "eval-in-repl-%s" base))
                               (intern (format "eir-eval-in-%s" base)))))))
      (when (require (car pkg-fn) nil 'noerror)
	(let ((fn (cdr pkg-fn)))
          (when (fboundp fn)
            (local-set-key key fn))))))

  (add-hook 'prog-mode-hook
            (lambda () (my/eir-dispatch-key (kbd "C-RET"))))
  :config
  (setq eir-repl-placement 'below))

(provide 'my-syntax-repl)
;;; my-syntax-repl.el ends here
