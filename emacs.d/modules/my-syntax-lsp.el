;;; my-syntax-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides LSP configuration for various languages development in Emacs.

;;; Code:
;; Named by a hook that is registered before eglot loads, so it is defined
;; here rather than in eglot's `:config': the hook would otherwise call a
;; function that does not exist yet.  Nothing in it is eglot's anyway.
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

(use-package eglot
  ;; Reached by the hooks below, which name `eglot-ensure' -- autoloaded, so
  ;; opening a file in one of these modes is what loads eglot.
  ;;
  ;; They are in `:init' because `:config' runs only once eglot is loaded,
  ;; and nothing else loads it.  Waiting on `(python swift-ts-mode)' waited
  ;; on both: a Python file loads `python' and never `swift-ts-mode', so
  ;; eglot stayed unloaded, these hooks unregistered, and no buffer got a
  ;; language server unless a Swift file had been opened first.
  ;;
  ;; `eglot-server-programs' stays in `:config' and is still in time: an
  ;; autoloaded function loads its file and runs the after-load forms before
  ;; its own body, so the table is filled before `eglot-ensure' reads it.
  :defer t
  :init
  (my/add-hook
   (:hook swift-mode-hook swift-ts-mode-hook :func #'eglot-ensure)
   (:hook python-mode-hook python-ts-mode-hook
          :func #'eglot-ensure #'my/ensure-pyright-available))
  :config
  ;; Prevent eglot from hijacking imenu or other features
  (setq eglot-stay-out-of '(imenu))

  ;; Swift
  (dolist (mode '(swift-mode swift-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("xcrun" "sourcekit-lsp"))))

  ;; Python
  (dolist (mode '(python-mode python-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("pyright-langserver" "--stdio"))))

  (setq-default eglot-workspace-configuration
                '((:pyright . (:useLibraryCodeForTypes t
                                                       :useTypeCheckingMode "strict"
                                                       :reportMissingImports t
                                                       :reportMissingTypeStubs t)))))
(use-package apheleia
  ;; Where code is written, and not where anything else is.  The global mode
  ;; turns it on in every buffer there is and leaves it to each one's major
  ;; mode to have no formatter; a hook says the same thing without loading a
  ;; formatter to open a text file.
  :diminish apheleia-mode
  :if (memq system-type '(darwin gnu/linux))
  :defer t
  :init
  (my/add-hook
   (:hook prog-mode-hook
          :func #'apheleia-mode)))

(provide 'my-syntax-lsp)
;;; my-syntax-lsp.el ends here
