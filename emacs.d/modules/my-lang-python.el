;;; my-lang-python.el --- Python editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for Python development in Emacs.

;;; Code:

(use-package pyenv-mode
  :config
  (my/define-key
   (:map pyenv-mode-map
         :key
         "C-c C-s" nil
         "C-c C-u" nil))

  (my/add-hook
   (:hook python-mode python-ts-mode
          :func #'pyenv-mode)))

(use-package pyvenv
  :config
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (setq python-shell-interpreter
                    (expand-file-name "Scripts/python" pyvenv-virtual-env)))))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
