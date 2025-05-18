;;; my-lang-jupyter.el --- Jupyter (EIN) integration for Python -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides configuration for working with Jupyter notebooks in Emacs via `ein`.
;; Integrates smartrep, Evil keybindings, and enhanced UI for notebook interaction.

;;; Code:

(use-package ein
  :defer t
  :after (evil ein smartrep)
  :config
  (my/define-key
   (:map ein:notebook-mode-map
         :key
         my/c-: nil
         "C-;" #'ein:shared-output-eval-string
         "C-RET" #'ein:worksheet-execute-cell-and-goto-next))

  (smartrep-define-key ein:notebook-mode-map "C-c"
    '(("C-t" . ein:worksheet-toggle-cell-type)
      ("C-l" . ein:worksheet-clear-output)
      ("C-k" . ein:worksheet-kill-cell)
      ("C-y" . ein:worksheet-yank-cell)
      ("C-a" . ein:worksheet-insert-cell-above)
      ("C-b" . ein:worksheet-insert-cell-below)
      ("C-n" . ein:worksheet-goto-next-input)
      ("C-p" . ein:worksheet-goto-prev-input)
      ("C-m" . ein:worksheet-merge-cell)
      ("<up>" . ein:worksheet-move-cell-up)
      ("<down>" . ein:worksheet-move-cell-down)))

  (my/add-hook
   (:hook ein:notebook-mode-hook
          :func
          #'anaconda-eldoc-mode
          (lambda ()
            (my/evil-ex-define-cmd-local "w[rite]" #'ein:notebook-save-notebook-command)
            (my/evil-ex-define-cmd-local "q[uit]"   #'ein:notebook-close)))
   (:hook ein:notebooklist-mode-hook
          :func (lambda () (setq truncate-lines t)))))

(provide 'my-lang-jupyter)
;;; my-lang-jupyter.el ends here
