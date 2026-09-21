;;; my-completion-buffer.el --- In-buffer completion setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides in-buffer completion using Corfu, Cape, and Tempel.

;;; Code:

;; In-buffer completion happens while typing, and typing here means insert
;; state.  Armed on the first entry into it rather than at startup, which
;; takes corfu, its popup info and tempel off the start of every session and
;; off a session spent reading altogether.  The hook stands down once it has
;; fired: what these turn on are global modes, and they stay on.
;;
;; `with-eval-after-load' because the modules load in name order, so evil is
;; not here yet when this file is read and the hook it is added to is evil's.
(defun my/arm-in-buffer-completion ()
  "Turn on in-buffer completion, once, and stand down."
  (remove-hook 'evil-insert-state-entry-hook #'my/arm-in-buffer-completion)
  (global-corfu-mode 1)
  (global-tempel-abbrev-mode 1))

(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my/arm-in-buffer-completion))

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  ;; `global-corfu-mode' is called by the arming above, and calling it is what
  ;; loads this.  It used to sit in `:config' behind `:after evil', which put
  ;; the whole of corfu in the start for a popup nobody had typed at yet.
  :defer t
  :config
  (setq corfu-auto t
	    corfu-auto-delay 0
	    corfu-auto-prefix 2
	    corfu-cycle t)
  (global-corfu-mode 1)

  ;; corfu draws its two margins as fringes -- the right one carries the
  ;; scrollbar -- and a fringe is painted with the `fringe' face wherever it
  ;; is, which inside the popup is the page colour standing in a strip down
  ;; either edge.  corfu already remaps `default' in that buffer; `fringe'
  ;; wants the same treatment and there is no setting that reaches it.
  (defun my/corfu-margins-read-as-popup (buffer)
    "Point BUFFER's `fringe' face at `corfu-default'.  Return BUFFER."
    (with-current-buffer buffer
      (setf (alist-get 'fringe face-remapping-alist)
            (cons 'corfu-default (alist-get 'fringe face-remapping-alist))))
    buffer)

  (advice-add 'corfu--make-buffer :filter-return
              #'my/corfu-margins-read-as-popup)

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
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package cape
  ;; Nothing here but two entries on a list of functions, and cape autoloads
  ;; both -- so the package arrives at the first completion that reaches them.
  ;; Without a deferring keyword use-package loads a package outright, which
  ;; is what was happening: `:init' alone does not defer.
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package tempel
  ;; Reached two ways, and neither is the start of a session: the capf below
  ;; names `tempel-complete', which tempel autoloads, so the package arrives
  ;; at the first completion in a buffer that has it; and
  ;; `global-tempel-abbrev-mode' is called by the arming at the top of this
  ;; file.  The `prog-mode-hook' that turned `tempel-abbrev-mode' on buffer by
  ;; buffer is gone, because the global mode is the same thing for more
  ;; buffers and is now what puts it on.
  :defer t
  :init
  (setq tempel-path (expand-file-name "etc/tempel/*.eld" user-emacs-directory))

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))

  (my/add-hook
   (:hook conf-mode-hook prog-mode-hook text-mode-hook
          :func #'tempel-setup-capf))
  :config
  (my/define-key
   (:map tempel-map
         :after evil
         :state insert
         :key
         "TAB" #'tempel-next
         "C-e" #'tempel-done)))

(use-package tempel-collection
  ;; Templates for tempel to find, and nothing without it.
  :after tempel)

(provide 'my-completion-buffer)
;;; my-completion-buffer.el ends here
