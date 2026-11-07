;;; my-syntax-visual.el --- What the code says back while it is edited -*- lexical-binding: t -*-

;;; Commentary:
;; eldoc and flymake: the signature at point and the diagnostics, across
;; the modes that have them.

;;; Code:

(use-package eldoc
  :straight nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p t
        eldoc-idle-delay 0.25))

(defun my/elisp-flymake-setup ()
  "Turn flymake on for Emacs Lisp, minus the backend a process costs too much.

`emacs-lisp-mode' offers two: `elisp-flymake-checkdoc', which reads the
buffer in this process, and `elisp-flymake-byte-compile', which starts an
Emacs of its own for every check -- `make-process' against
`invocation-name'.  That is nothing here and a fifth of a second on the
Windows machine, paid over and over while typing, so there it keeps
checkdoc alone.

The unbalanced paren and the compiler's warnings come from the
byte-compile one, which is the half worth having and the half that costs."
  (when (eq system-type 'windows-nt)
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile t))
  (flymake-mode 1))

(use-package flymake
  ;; Reached by the hook below, which names an autoloaded mode.
  :defer t
  :custom
  ;; `ewn[3 0 3]': three errors, no warnings, three notes.  The lighter
  ;; carries the legend instead of a name, because which of flymake and
  ;; flycheck is underneath is not a thing worth knowing while reading a
  ;; buffer, and three columns is what every other lighter here costs.
  ;;
  ;; A legend once rather than a letter per field (`fly[3e 0w 3n]'): the
  ;; counts are taken in as a shape and not read as prose, so the key
  ;; wants to be out of the figure.  Once it is known it is skipped.
  (flymake-mode-line-lighter "ewn")

  ;; Which only holds while the positions are fixed.  Stock hides a zero
  ;; note count, so `[0 0]' is errors and warnings where `[0 0 0]' is all
  ;; three, and the third slot means whatever happens to be in it.
  (flymake-suppress-zero-counters nil)
  :init
  (my/add-hook
   (:hook emacs-lisp-mode-hook :func #'my/elisp-flymake-setup)))

(provide 'my-syntax-visual)
;;; my-syntax-visual.el ends here
