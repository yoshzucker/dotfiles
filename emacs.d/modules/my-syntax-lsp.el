;;; my-syntax-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides LSP configuration for various languages development in Emacs.

;;; Code:
(use-package eglot
  :after (python swift-ts-mode)
  :config
  ;; Prevent eglot from hijacking imenu or other features
  (setq eglot-stay-out-of '(imenu))

  ;; Swift
  (my/add-hook
   (:hook swift-mode-hook swift-ts-mode-hook :func #'eglot-ensure))

  (dolist (mode '(swift-mode swift-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("xcrun" "sourcekit-lsp"))))

  ;; Python
  (my/add-hook
   (:hook python-mode-hook python-ts-mode-hook
          :func #'eglot-ensure #'my/ensure-pyright-available))

  (dolist (mode '(python-mode python-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("pyright-langserver" "--stdio"))))

  (setq-default eglot-workspace-configuration
                '((:pyright . (:useLibraryCodeForTypes t
                                                       :useTypeCheckingMode "strict"
                                                       :reportMissingImports t
                                                       :reportMissingTypeStubs t))))
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
        (message "pyright in use: %s" pyright-path))))))

(use-package apheleia
  :diminish apheleia-mode
  :if (memq system-type '(darwin gnu/linux))
  :config
  (apheleia-global-mode +1))

(provide 'my-syntax-lsp)
;;; my-syntax-lsp.el ends here
