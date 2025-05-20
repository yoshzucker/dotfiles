;;; my-editor-evil.el --- Evil mode key customizations for iTerm2 -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure iTerm2 key mappings:
;;   (Preferences -> Profiles -> Keys -> Key Mappings -> Send Hex Code)
;;
;;   C-,   -> "18 40 63 2c"
;;   C-.   -> "18 40 63 2e"
;;   C-:   -> "18 40 63 3a"
;;   C-;   -> "18 40 63 3b"
;;   C-<   -> "18 40 63 3c"  (actually C-S-<)
;;   C->   -> "18 40 63 3e"  (actually C-S->)
;;   C-(   -> "18 40 63 28"
;;   C-)   -> "18 40 63 29"
;;   C-RET -> "18 40 63 0d"

;;; Code:

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq evil-undo-system 'undo-tree
        undo-tree-auto-save-history nil)
  (add-hook 'evil-local-mode-hook #'turn-on-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-integration t         ; for evil-collection
        evil-want-keybinding nil)       ; for evil-collection
  :config
  (evil-mode 1)

  (my/define-key
   (:map global-map
         :key
         (my/c-: "M-:") #'execute-extended-command
         "C-w" #'evil-window-map)
   (:map evil-motion-state-map
         :key
         "g:" #'execute-extended-command
         "gf" #'find-file-at-point
         "zd" #'narrow-to-defun
         "zn" #'narrow-to-region
         "zw" #'widen)
   (:map evil-window-map
         :key
         "w" #'ace-window
         "a" #'ace-swap-window)
   (:map evil-inner-text-objects-map
         :key "d" #'my/evil-inner-defun)
   (:map evil-outer-text-objects-map
         :key "d" #'my/evil-a-defun))

  (setq evil-esc-delay 0
	    evil-symbol-word-search t
        evil-jumps-max-length 500
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-mode-line-format 'before
        evil-echo-state nil)

  (defun my/evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))

  (my/evil-swap-key evil-motion-state-map "j" "gj")
  (my/evil-swap-key evil-motion-state-map "k" "gk")

  (evil-define-command my/evil-vim-quit (&optional force)
    "Quit buffer/window like Vim. Use FORCE for :q! or :wq!."
    :repeat nil
    (interactive "<!>")
    (condition-case nil
	(progn (kill-buffer) (delete-window))
      (error
       (when (and (fboundp 'server-edit)
                  (bound-and-true-p server-buffer-clients))
	 (if force
             (server-buffer-done (current-buffer))
           (server-edit))))))
  
  (evil-define-command my/evil-vim-wq (&optional force)
    "Save buffer and quit, optionally forcing like :wq!."
    :repeat nil
    (interactive "<!>")
    (save-buffer)
    (my/evil-vim-quit force))
  
  (dolist (pair '(("q[ite]" . my/evil-vim-quit)
                  ("wq"     . my/evil-vim-wq)
                  ("ls"     . ibuffer)
                  ("etags"  . create-etags)
                  ("dg"     . deadgrep)))
    (evil-ex-define-cmd (car pair) (cdr pair)))

  (defun my/evil-ex-define-cmd-local (cmd function)
    "Locally binds the function FUNCTION to the command CMD."
    (unless (local-variable-p 'evil-ex-commands)
      (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
    (evil-ex-define-cmd cmd function))

  (evil-define-text-object my/evil-inner-defun (count &optional beg end type)
    (evil-select-inner-object 'evil-defun beg end type count))

  (evil-define-text-object my/evil-a-defun (count &optional beg end type)
    (pcase-let ((`(,beg ,past-end)
                 (evil-select-inner-object 'evil-defun beg end type count)))
      (goto-char past-end)
      (if-let ((lst-space-addr (re-search-forward "[^[:space:]\n]" nil t)))
          `(,beg ,(1- lst-space-addr))
        `(,beg ,(point-max)))))

  (defun my/evil-text-objects ()
    "Fuzzy search Evil text objects with key and command."
    (interactive)
    (let ((flatten-keymap
           (lambda (keymap)
             (let (pairs)
               (map-keymap
                (lambda (event binding)
                  (when (commandp binding)
                    (push (cons (single-key-description event) binding) pairs)))
                keymap)
               pairs))))
      (let* ((inner (funcall flatten-keymap evil-inner-text-objects-map))
             (outer (funcall flatten-keymap evil-outer-text-objects-map))
             (all (append inner outer))
             (candidates
              (mapcar (lambda (pair)
                        (cons (format "%s : %s" (car pair)
                                      (symbol-name (cdr pair)))
                              (cdr pair)))
                      all)))
        (consult--read (mapcar #'car candidates)
                       :require-match t)))))

(use-package winner
  :after evil
  :init
  (winner-mode 1)
  :config
  (my/define-key
   (:map evil-window-map
         :key
         "," #'winner-undo
         "." #'winner-redo)))

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :after evil
  :config
  (setq etcc-term-type-override (or etcc-term-type-override 'xterm))
  (evil-terminal-cursor-changer-activate))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :init
  :config
  (evil-collection-init)

  (my/define-key
   (:map evil-collection-magit-toggle-text-minor-mode-map
         :after evil-collection-magit
         :state normal
         :key
         "\\" nil)
   (:map comint-mode-map
         :after evil-collection-comint
         :state insert
         :key
         "C-p" #'comint-previous-input
         "C-n" #'comint-next-input)))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode 1))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-textobj-anyblock
  :after evil
  :config
  (my/define-key
   (:map evil-inner-text-objects-map
         :key
         "b" #'evil-textobj-anyblock-inner-block)
   (:map evil-outer-text-objects-map
         :key "b" #'evil-textobj-anyblock-a-block)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)

  (defun my/evil-surround-function ()
    "Surround with a function call using minibuffer input."
    (let ((fname (evil-surround-read-from-minibuffer "function: " "")))
      (cons (format "%s(" (or fname "")) ")")))

  (advice-add 'evil-surround-function :override #'my/evil-surround-function))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-cx-install))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t))

(use-package evil-iedit-state
  :after evil
  :config
  (my/define-key
   (:map evil-normal-state-map :key "g C-n" #'iedit-mode)
   (:map evil-visual-state-map :key "C-n" #'iedit-mode)))

(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-additional-movement-keys nil)
  :config
  (my/add-hook
   (:hook lisp-mode-hook slime-lisp-mode-hook emacs-lisp-mode-hook
          :func #'evil-cleverparens-mode))

  (when evil-move-cursor-back
    (setq evil-move-beyond-eol t)))

(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (defcustom evil-snipe-emurate-feature 'clever-f
    "clever-f affects to f/F/t/T. vim-sneak affects s/S in addition."
    :type '(choice
            (const :tag "clever-f" clever-f)
            (const :tag "vim-sneak" vim-sneak)))

  (when (eq evil-snipe-emurate-feature 'vim-sneak)
    (evil-snipe-mode 1))

  (evil-snipe-override-mode 1)

  (setq evil-snipe-smart-case t
        evil-snipe-repeat-keys nil
        evil-snipe-show-prompt nil
        evil-snipe-enable-incremental-highlight nil
        evil-snipe-scope 'whole-buffer)

  (my/add-hook
   (:hook magit-mode-hook dired-mode-hook
          :func #'turn-off-evil-snipe-override-mode)))

(use-package avy
  :after evil
  :config
  (my/define-key
   (:map evil-motion-state-map evil-operator-state-map
         :key
         my/backslash #'evil-avy-goto-char-timer))
  (setq avy-timeout-seconds 0.25))

(use-package avy-migemo
  :after avy
  :config
  (avy-migemo-mode 1))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode 1))

(use-package origami
  :after evil
  :config
  (global-origami-mode)
  (my/define-key
   (:map evil-normal-state-map
         :key
         "za" #'origami-toggle-node
         "zc" #'origami-close-node
         "zm" #'origami-close-all-nodes
         "zo" #'origami-open-node
         "zr" #'origami-open-all-nodes
         "zO" #'origami-open-node-recursively)))

(use-package smartrep
  :after evil
  :config
  (smartrep-define-key global-map "C-w"
    '(("+" . evil-window-increase-height)
      ("-" . evil-window-decrease-height)
      (">" . evil-window-increase-width)
      ("<" . evil-window-decrease-width)
      ("C-w" . evil-window-next)
      ("C-h" . evil-window-left)
      ("C-j" . evil-window-down)
      ("C-k" . evil-window-up)
      ("C-l" . evil-window-right)))
  
  (smartrep-define-key evil-motion-state-map "g"
    '(("t" . evil-tab-next)
      ("T" . evil-tab-previous)))
  
  (smartrep-define-key evil-motion-state-map "["
    '(("b" . evil-prev-buffer)))
  
  (smartrep-define-key evil-motion-state-map "]"
    '(("b" . evil-next-buffer))))

(provide 'my-editor-evil)
;;; my-editor-evil.el ends here
