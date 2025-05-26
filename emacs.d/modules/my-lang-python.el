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

(use-package eglot
  :after python
  :config
  (my/add-hook
   (:hook python-mode-hook python-ts-mode-hook
          :func #'eglot-ensure))

  (defun my/ensure-pyright-available ()
    "Check current pyright usage and show guidance for reproducibility."
    (interactive)
    (let* ((pyright-path (executable-find "pyright-langserver"))
           (project-root (or (project-root (project-current)) default-directory))
           (version-file (locate-dominating-file project-root ".python-version"))
           (venv-dir (locate-dominating-file project-root ".venv"))
           (venv-bin (when venv-dir (expand-file-name "bin/pyright-langserver" venv-dir))))
      (cond
       ((null pyright-path)
        (message "pyright not found. Install with: npm install -g pyright"))
       
       ((and venv-bin (file-exists-p venv-bin)
             (file-equal-p pyright-path venv-bin))
        (message "pyright is project-local: %s" pyright-path))
       
       ((string-match-p "\\.npm" pyright-path)
        (let ((base-msg (format "Using global pyright: %s." pyright-path))
              (advice
               (cond
                (version-file "Consider: pip install pyright in your venv")
                (venv-dir "Consider: poetry add --group dev pyright")
                (t "Consider using a virtualenv or poetry"))))
          (message "%s %s" base-msg advice)))
       
       (t
        (message "pyright in use: %s" pyright-path)))))

  (add-hook 'eglot-connect-hook
            (lambda ()
              (when (memq major-mode '(python-mode python-ts-mode))
                (my/ensure-pyright-available))))

  ;; Prevent eglot from hijacking imenu or other features
  (setq eglot-stay-out-of '(imenu))

  (setq-default eglot-workspace-configuration
                '((:pyright . (:useLibraryCodeForTypes t
                               :useTypeCheckingMode "strict"
                               :reportMissingImports t
                               :reportMissingTypeStubs t))))

  (dolist (mode '(python-mode python-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("pyright-langserver" "--stdio")))))

(use-package reformatter
  :config
  (reformatter-define black-format
                      :program (or (executable-find "black")
                                   (user-error "black not found in PATH"))
                      :args '("-"))
  
  (reformatter-define isort-format
                      :program (or (executable-find "isort")
                                   (user-error "isort not found in PATH"))
                      :args '("-"))

  (define-minor-mode my/python-autoformat-mode
    "Autoformat Python buffers on save."
    :lighter " mpf"
    (if my/python-autoformat-mode
        (add-hook 'before-save-hook #'my/python-auto-format nil t)
      (remove-hook 'before-save-hook #'my/python-auto-format t)))
  
  (defun my/python-auto-format ()
    "Format with isort and black."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (isort-format-buffer)
      (black-format-buffer)))
  
  (my/add-hook
   (:hook python-mode-hook python-ts-mode-hook
          :func #'my/python-autoformat-mode)))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
