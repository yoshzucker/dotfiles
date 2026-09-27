;;; my-lang-python.el --- Python editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python's own mode does the editing.  What is here is the one thing it does
;; not do: find the project's virtualenv and point the rest of Emacs at the
;; executables inside it.
;;
;; Which matters for the project's own dependencies and not for the tools
;; around them.  pyright and ruff are installed once, on PATH, because
;; nothing imports them -- see `install_python_tools' in bootstrap.  The
;; interpreter and the libraries it can see are the other kind: those belong
;; to the checkout, and a REPL started against the wrong ones is a REPL that
;; cannot import the project.

;;; Code:

(use-package pet
  :straight (pet :host github :repo "wyuenho/emacs-pet")
  ;; Reached by the hook below and nothing else.
  ;;
  ;; Instead of pyvenv, which asked to be told where the environment was and
  ;; has not been touched upstream since 2024.  pet finds it: a `.venv' in
  ;; the project, or whatever poetry, pipenv, conda, pdm, hatch, pyenv or uv
  ;; made, and then sets the interpreter and the executables eglot, apheleia
  ;; and flymake will look for.  uv is the one this configuration installs
  ;; with, and a uv project is a plain `.venv' in the root.
  :defer t
  :init
  ;; A plain `add-hook' because the depth is load-bearing and `my/add-hook'
  ;; has no argument for it: pet decides which executables the hooks after
  ;; it will find, so it goes first.  `python-base-mode-hook' is the parent
  ;; of both python-mode and python-ts-mode, which is why neither is named.
  (add-hook 'python-base-mode-hook #'pet-mode -10))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
