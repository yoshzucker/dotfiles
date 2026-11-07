;;; my-syntax-repl.el --- Process buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybindings and behaviour for `comint', the buffer under every REPL and
;; inferior process.  Evaluating code into one belongs to the language's own
;; mode: Emacs Lisp evaluates in place and sly sends to its live image.

;;; Code:

(use-package comint
  ;; The substrate under every process buffer -- a REPL, a shell, an inferior
  ;; anything -- which is why it has a module of its own rather than sitting
  ;; beside the editing of shell scripts, with which it shares a word and
  ;; nothing else.
  ;;
  ;; Deferred, because nothing wants comint until a process buffer exists.
  :straight nil
  :defer t
  :config
  (my/define-key
   (:map comint-mode-map
         :key
         "C-p" #'comint-previous-matching-input-from-input
         "C-n" #'comint-next-matching-input-from-input)
   ;; Plain history in insert state, where the matching search above would
   ;; take the half-typed line as a pattern.
   (:map comint-mode-map
         :state insert
         :key
         "C-j" #'comint-next-input
         "C-k" #'comint-previous-input))

  (defun my/comint-clear ()
    "Clear this process buffer's output."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(provide 'my-syntax-repl)
;;; my-syntax-repl.el ends here
