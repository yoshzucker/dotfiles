;;; my-lang-python.el --- Python editing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for Python development in Emacs.
;; Integrates LSP (eglot), syntax checking, REPL evaluation, formatting, poetry, etc.

;;; Code:

(use-package python
  :straight nil
  :after evil
  :mode (("\\.py\\'" . python-mode))
  :config
  (diminish-major-mode 'python-mode-hook "Py")
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        python-shell-completion-native-enable nil)
  (add-to-list 'evil-normal-state-modes 'inferior-python-mode))

(use-package eglot
  :after python
  :config
  (my/add-hook
   (:hook python-mode-hook python-ts-mode-hook
          :func #'eglot-ensure))

  ;; Prevent eglot from hijacking imenu or other features
  (setq eglot-stay-out-of '(imenu))

  ;; Use pyright language server
  (dolist (mode '(python-mode python-ts-mode))
    (add-to-list 'eglot-server-programs
		 `(,mode . ("pyright-langserver" "--stdio"))))

  (defun eglot-howto-py ()
    "Show help message for setting up Pyright with poetry."
    (interactive)
    (let ((buf (get-buffer-create "*eglot-pyright-setup*")))
      (with-current-buffer buf
	(erase-buffer)
	(insert "📌 How to install Pyright with Poetry\n\n")
	(insert "Run the following command in your project root:\n\n")
	(insert "    poetry add --group dev pyright\n\n")
	(insert "This ensures `pyright-langserver` is available for `eglot` to use.\n")
	(insert "\nYou may also need to ensure your environment is activated:\n")
	(insert "    poetry shell\n\n")
	(insert "or let `eglot` run in the virtualenv directly.\n"))
      (display-buffer buf))))

(use-package blacken
  :defer t
  :config
  (setq blacken-line-length 98))

(use-package pyenv-mode
  :defer t
  :config
  (my/define-key
   (:map pyenv-mode-map
         :key
         "C-c C-s" nil
         "C-c C-u" nil))

  (pyenv-mode)

  (add-hook 'projectile-after-switch-project-hook #'my/projectile-pyenv-set)
  (defun my/projectile-pyenv-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(use-package poetry
  :config
  (poetry-tracking-mode))

(use-package poetry
  :if (eq system-type 'windows-nt)
  :after poetry
  :config
  ;; Advice: Fix CRLF and env on Windows for poetry
  ;; Override for CRLF-tolerant poetry root detection
  (defun my/poetry-find-project-root ()
    "Find poetry project root, tolerant of CRLF line endings."
    (or poetry-project-root
	(when-let* ((root (locate-dominating-file default-directory "pyproject.toml"))
                    (file (expand-file-name "pyproject.toml" root))
                    (contents (with-temp-buffer
				(insert-file-contents-literally file)
				(buffer-string))))
          (when (string-match "^\\[tool\\.poetry]\\(\r\\)?$" contents)
            (setq poetry-project-root root)))))
  
  (advice-add 'poetry-find-project-root :override #'my/poetry-find-project-root)
  
  ;; Around advice to replace env prefix with direct call on Windows
  (defun my/poetry-do-call--around (orig-fun command &optional args project output blocking)
    "Advice around `poetry-do-call` to avoid `env` on Windows."
    (cl-letf* (((symbol-function #'executable-find)
		(lambda (cmd)
                  (if (and (eq system-type 'windows-nt) (string= cmd "poetry"))
		      ;; Direct poetry call on Windows
		      (executable-find "poetry")
                    ;; Fallback to original
                    (funcall (symbol-function 'executable-find) cmd)))))
      (funcall orig-fun command args project output blocking)))
  
  (advice-add 'poetry-do-call :around #'my/poetry-do-call--around))

(provide 'my-lang-python)
;;; my-lang-python.el ends here
