;;; my-lang-python.el --- Python editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python's own mode does the editing.  What is here is the one thing it does
;; not do on its own: put a virtualenv in front of it.

;;; Code:

(use-package pyvenv
  ;; Loaded by `pyvenv-activate' or `pyvenv-workon', and nothing more is
  ;; needed from it.  pyvenv sets `python-shell-virtualenv-root', and python.el
  ;; reads that to put the environment's bin directory -- `Scripts' on Windows,
  ;; `bin' everywhere else -- at the front of the interpreter's `exec-path'.
  ;;
  ;; Setting `python-shell-interpreter' to an absolute path instead would say
  ;; the same thing in a way that cannot be undone: it outlives
  ;; `pyvenv-deactivate' and goes on naming an interpreter that is no longer
  ;; the one in use.
  :defer t)

(provide 'my-lang-python)
;;; my-lang-python.el ends here
