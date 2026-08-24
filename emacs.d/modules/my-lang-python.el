;;; my-lang-python.el --- Python editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for Python development in Emacs.

;;; Code:

(use-package pyvenv
  :defer t
  :config
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              ;; Windows virtualenvs put the interpreter in Scripts/, every
              ;; other platform in bin/.
              (setq python-shell-interpreter
                    (expand-file-name (if (eq system-type 'windows-nt)
                                          "Scripts/python"
                                        "bin/python")
                                      pyvenv-virtual-env)))))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
