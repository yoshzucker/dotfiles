;;; my-completion-buffer.el --- In-buffer completion setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides in-buffer completion using Corfu, Cape, and Tempel.

;;; Code:

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :after evil evil-collection
  :config
  (setq corfu-auto t
	    corfu-auto-delay 0
	    corfu-auto-prefix 2
	    corfu-cycle t)
  (global-corfu-mode 1)

  ;; (advice-add 'evil-collection-corfu-quit-and-escape :override #'corfu-quit)
  (my/define-key
   (:map corfu-map
         :state insert
         :key
         "C-y" #'corfu-insert
         "C-e" #'corfu-quit)))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :config
  (corfu-popupinfo-mode))

(use-package emacs
  :straight nil
  :init
  (setq completion-cycle-threshold 3
        read-extended-command-predicate #'command-completion-default-include-p
        tab-always-indent 'complete))

(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal" :branch "master" :files ("*.el" "out"))
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package tempel
  :config
  (my/define-key
   (:map tempel-map
         :after evil
         :state insert
         :key
         "TAB" #'tempel-next
         "C-e" #'tempel-done))

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))

  (my/add-hook
   (:hook conf-mode-hook prog-mode-hook text-mode-hook
          :func #'tempel-setup-capf)
   (:hook prog-mode-hook
          :func #'tempel-abbrev-mode))

  (global-tempel-abbrev-mode 1))

(use-package tempel-collection)

(provide 'my-completion-buffer)
;;; my-completion-buffer.el ends here
