;;; my-editor-evil.el --- Evil, and the keys a terminal cannot send -*- lexical-binding: t; -*-

;;; Commentary:
;; Some of the keys bound below have no encoding of their own in a terminal.
;; They arrive instead as Emacs\='s own control escape -- `C-x @ c\=' followed by
;; the plain character, 18 40 63 in hex -- which the terminal has to be told
;; to send:
;;
;;   C-,   18 40 63 2c      C-<   18 40 63 3c   (C-S-<)
;;   C-.   18 40 63 2e      C->   18 40 63 3e   (C-S->)
;;   C-:   18 40 63 3a      C-(   18 40 63 28
;;   C-;   18 40 63 3b      C-)   18 40 63 29
;;   C-RET 18 40 63 0d
;;
;; Where that is said depends on the terminal -- mintty has KeyFunctions,
;; ghostty has `keybind ... text:\=' -- so what is recorded here is the hex,
;; which is the part that does not change.  A graphical Emacs needs none of it.

;;; Code:

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
   (:map evil-normal-state-map
         :key
         "C-e" #'my/pixel-scroll-line-down
         "C-y" #'my/pixel-scroll-line-up)
   (:map evil-motion-state-map
         :key
         "g:" #'execute-extended-command
         "gf" #'find-file-at-point
         "gh" #'my/find-file-from-base
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

  (setopt evil-esc-delay 0
          evil-undo-system 'undo-redo
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

  (defun my/line-px ()
    "Get line height."
    ;; Basic line height = character cell height + line spacing
    (+ (frame-char-height) (or line-spacing 0)))
  
  (defun my/pixel-scroll-line-down (&optional count)
    "Pixel-precise version of Evil's line scroll (downwards), supporting COUNT."
    (interactive "p")
    (let* ((n (max 1 (or count 1)))
           (px (* n (my/line-px))))
      (pixel-scroll-precision-scroll-down px)))

  (defun my/pixel-scroll-line-up (&optional count)
    "Pixel-precise version of Evil's line scroll (upwards), supporting COUNT."
    (interactive "p")
    (let* ((n (max 1 (or count 1)))
           (px (* n (my/line-px))))
      (pixel-scroll-precision-scroll-up px)))

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
  
  (dolist (pair '(("q[uit]"  . my/evil-vim-quit)
                  ("wq"      . my/evil-vim-wq)
                  ("bk[ill]" . kill-current-buffer)
                  ("ls"      . ibuffer)
                  ("etags"   . create-etags)
                  ("dg"      . deadgrep)
                  ("fd"      . my/fd-dired)))
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
  :config
  (setopt evil-collection-key-blacklist '("gs" "gr"))
  ;; org-agenda: keep evil's built-in emacs state; the agenda keymap in
  ;; my-app-org.el is built for emacs state, so don't let evil-collection
  ;; take it over (newer evil-collection moves it to normal state).
  (let ((excludes '(agent-shell comint org-agenda)))
    (dolist (mode excludes)
      (setq evil-collection-mode-list (remove mode evil-collection-mode-list))))
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

;; The editing extensions -- the operators, the text objects, a better `f' --
;; each install themselves as a global minor mode over evil's own keymaps, and
;; not one of them is reachable before there is a file open: a splash screen
;; takes no operator.  Armed at the first visit instead of at the start, and
;; the hook stands down once it has fired.
;;
;; Turned on by calling the same functions the packages' `:config' used to
;; call, which is also what loads them -- every one of these is autoloaded.
;; Nothing of their keymaps is written out here: a configuration that copied
;; which key each package binds would be a copy going quietly out of date.
(defun my/arm-evil-extensions ()
  "Turn on the evil editing extensions, once, and stand down."
  (remove-hook 'find-file-hook #'my/arm-evil-extensions)
  (evil-lion-mode 1)
  (evil-commentary-mode 1)
  (global-evil-surround-mode 1)
  (evil-exchange-cx-install)
  (global-evil-visualstar-mode 1)
  (evil-snipe-override-mode 1))

(add-hook 'find-file-hook #'my/arm-evil-extensions)

(use-package evil-lion
  :defer t)

(use-package evil-commentary
  :defer t
  :diminish evil-commentary-mode)

(use-package evil-textobj-anyblock
  ;; No mode to turn on: two text objects, both autoloaded, so the bindings
  ;; below reach them and pressing one is what brings the package.
  :after evil
  :defer t
  :init
  (my/define-key
   (:map evil-inner-text-objects-map
         :key
         "b" #'evil-textobj-anyblock-inner-block)
   (:map evil-outer-text-objects-map
         :key "b" #'evil-textobj-anyblock-a-block)))

(use-package evil-surround
  :defer t
  :config
  (defun my/evil-surround-function ()
    "Surround with a function call using minibuffer input."
    (let ((fname (evil-surround-read-from-minibuffer "function: " "")))
      (cons (format "%s(" (or fname "")) ")")))

  (advice-add 'evil-surround-function :override #'my/evil-surround-function))

(use-package evil-exchange
  :defer t)

(use-package evil-visualstar
  :defer t
  :config
  (setq evil-visualstar/persistent t))

(use-package evil-iedit-state
  ;; Entered through the package's own command, not through `iedit-mode'.
  ;; What this package is for is the state around iedit -- `<E>', where the
  ;; occurrences can be moved over and changed with normal-state keys -- and
  ;; that state is put on by `evil-iedit-state/iedit-mode' and by nothing
  ;; else: the package hangs no hook on iedit.  Calling iedit directly gets
  ;; the editing without the state, which is the package not running.
  ;;
  ;; It autoloads nothing of its own, so the commands are named here.
  :after evil
  :defer t
  :commands (evil-iedit-state/iedit-mode)
  :init
  (my/define-key
   (:map evil-normal-state-map :key "g C-n" #'evil-iedit-state/iedit-mode)
   (:map evil-visual-state-map :key "C-n" #'evil-iedit-state/iedit-mode)))

(use-package evil-snipe
  :defer t
  :diminish evil-snipe-local-mode
  :init
  ;; In `:init' so that a dired or magit buffer opened before any file still
  ;; turns the override off -- and does it by reaching the package, which is
  ;; a trigger of its own.
  (my/add-hook
   (:hook magit-mode-hook dired-mode-hook
          :func #'turn-off-evil-snipe-override-mode))
  :config
  (defcustom evil-snipe-emurate-feature 'clever-f
    "clever-f affects to f/F/t/T. vim-sneak affects s/S in addition."
    :type '(choice
            (const :tag "clever-f" clever-f)
            (const :tag "vim-sneak" vim-sneak)))

  (when (eq evil-snipe-emurate-feature 'vim-sneak)
    (evil-snipe-mode 1))

  (setq evil-snipe-smart-case t
        evil-snipe-repeat-keys nil
        evil-snipe-show-prompt nil
        evil-snipe-enable-incremental-highlight nil
        evil-snipe-scope 'whole-buffer))

(use-package avy
  ;; One key, and `evil-avy-goto-char-timer' is evil-integration's rather than
  ;; avy's -- so the binding stands without avy, and pressing it reaches
  ;; `avy-goto-char-timer', which avy autoloads.
  :after evil
  :defer t
  :init
  (my/define-key
   (:map evil-motion-state-map evil-normal-state-map evil-operator-state-map
         :key
         my/backslash #'evil-avy-goto-char-timer))
  :config
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
  ;; Folding, reached by the `z' keys and by nothing else, so it waits for one
  ;; of them.  `:commands' is what makes that possible: origami autoloads its
  ;; modes and not the commands underneath, so without this the first `za'
  ;; would find nothing to call.  `global-origami-mode' then comes on in
  ;; `:config', which reaches the buffer already open as well as the next.
  :after evil
  :defer t
  :commands (origami-toggle-node origami-close-node origami-close-all-nodes
             origami-open-node origami-open-all-nodes
             origami-open-node-recursively)
  :init
  (my/define-key
   (:map evil-normal-state-map
         :key
         "za" #'origami-toggle-node
         "zc" #'origami-close-node
         "zm" #'origami-close-all-nodes
         "zo" #'origami-open-node
         "zr" #'origami-open-all-nodes
         "zO" #'origami-open-node-recursively))
  :config
  (global-origami-mode))

(use-package smartrep
  :after evil
  :config
  (smartrep-define-key global-map "C-w"
    '(("i" . transwin-inc)
      ("d" . transwin-dec)
      ("+" . evil-window-increase-height)
      ("-" . evil-window-decrease-height)
      (">" . evil-window-increase-width)
      ("<" . evil-window-decrease-width)
      ("C-w" . evil-window-next)
      ("C-h" . evil-window-left)
      ("C-j" . evil-window-down)
      ("C-k" . evil-window-up)
      ("C-l" . evil-window-right)
      ("e" . my/cycle-frame-size)))
  
  (smartrep-define-key evil-motion-state-map "g"
    '(("t" . evil-tab-next)
      ("T" . evil-tab-previous)))
  
  (smartrep-define-key evil-motion-state-map "["
    '(("b" . evil-prev-buffer)))
  
  (smartrep-define-key evil-motion-state-map "]"
    '(("b" . evil-next-buffer))))

(provide 'my-editor-evil)
;;; my-editor-evil.el ends here
