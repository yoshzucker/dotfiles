;;; my-lang-r.el --- R and ESS configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Emacs Speaks Statistics (ESS) for R development.
;; Includes support for smart piping, REPL integration, help buffers,
;; and enhancements like `ess-smart-equals` and handy commands.

;;; Code:

(use-package ess
  :after (evil comint)
  :defer t
  :init
  (setq ess-default-style 'DEFAULT)
  :config
  (my/define-key
   (:map ess-r-mode-map
         :after ess-r-mode
         :key
         "C-RET" #'ess-eval-region-or-line-visibly-and-step
         "C-c ," #'ess-handy-commands)
   (:map ess-help-mode-map
         :after ess-help-mode
         :key
         "C-RET" #'ess-eval-region-or-line-visibly-and-step
         "C-c ," #'ess-handy-commands))

  (setq inferior-R-args "--no-save"
	ess-use-tracebug t
        ess-use-eldoc t
        ess-eldoc-show-on-symbol t
        ess-eldoc-abbreviation-style 'mild)

  (defun my/ess-sayoonara (&optional arg)
    "Quit R session and kill the current ESS buffer."
    (interactive "P")
    (when (derived-mode-p 'inferior-ess-mode)
      (ess-quit arg)
      (kill-this-buffer)))
  
  (add-to-list 'ess-handy-commands
               '("sayoonara" . my/ess-sayoonara))

  ;; Hook for ESS REPL configuration
  (defun my/comint-ess-setup ()
    "Setup `comint-mode` behavior in `inferior-ess-mode`."
    (setq-local comint-use-prompt-regexp nil)
    (setq-local inhibit-field-text-motion nil)
    (setq-local comint-prompt-read-only t))
  
  (add-hook 'inferior-ess-mode-hook #'my/comint-ess-setup)

  ;; Use normal state in ESS help buffers
  (setq evil-emacs-state-modes
        (remove 'ess-help-mode evil-emacs-state-modes)))

(use-package ess-smart-equals
  :after ess-r-mode
  :init
  ;; Additional operators for smart equals behavior
  (setq ess-smart-equals-extra-ops '(brace percent))
  :config
  (ess-smart-equals-activate)
  ;; Extend percent-based operators
  (let ((percent-ops '("%>%" "%in%" "%*%" "%%" "%/%" "%<>%" "%o%" "%x%")))
    (setcdr (assoc '% (cdar ess-smart-equals-contexts)) percent-ops)))

(provide 'my-lang-r)
;;; my-lang-r.el ends here
