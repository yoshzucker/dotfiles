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
        evil-want-keybinding nil        ; for evil-collection
        ;; `/' as evil's own search rather than isearch, because `gn' --
        ;; and with it `cgn' and `.' over each match -- works only there.
        ;; Read when evil loads, so it is set before.
        evil-search-module 'evil-search)
  :config
  (evil-mode 1)

  (my/define-key
   (:map global-map
         :key
         ("C-:" "M-:") #'execute-extended-command
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

  ;; `:rg foo' searches the project for foo at once, as `:grep foo' does in
  ;; a Vim whose grepprg is rg; a bare `:rg' asks.  The pattern is an Emacs
  ;; regexp, which xref turns into rg's.
  (evil-define-command my/evil-project-find-regexp (regexp)
    "Search the project for REGEXP, asking for it when none is given."
    (interactive "<a>")
    (if (and regexp (not (string-empty-p regexp)))
        (project-find-regexp regexp)
      (call-interactively #'project-find-regexp)))

  (dolist (pair '(("bk[ill]" . kill-current-buffer)
                  ("ls"      . ibuffer)
                  ("etags"   . create-etags)
                  ("rg"      . my/evil-project-find-regexp)
                  ("fd"      . find-name-dired)))
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

;; C-[ as a key of its own, on a graphical frame.  A terminal sends C-[ and
;; ESC as the same byte, so there is nothing to tell apart there.  A
;; graphical frame reports the ESC key as `<escape>' and only C-[ as the
;; character ESC, so decoding that character to `<C-[>' separates the two,
;; and a mode can then give each its own binding -- as evil-ghostel does in
;; my-app-terminal.el, where ESC goes to the terminal and C-[ to evil.
;;
;; The decoding takes the place of evil's own, by which C-[ -- the character
;; ESC -- arrives as `<escape>' (`evil-intercept-esc').  So where nothing
;; binds `<C-[>' it is given back as evil gave it: `<escape>' where that is
;; bound -- leaving insert state, quitting the minibuffer -- and else the
;; character ESC, a meta prefix.  Both maps are per terminal, hence per
;; frame.
(defun my/control-bracket-as-escape (_prompt)
  "Return what C-[ is where nothing binds it: the ESC key's binding."
  (if (key-binding [escape]) [escape] [?\e]))

(defun my/distinguish-control-bracket (&optional frame)
  "Decode C-[ on FRAME's terminal as `<C-[>' when FRAME is graphical."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (define-key input-decode-map [?\C-\[] [C-\[])
      (define-key local-function-key-map [C-\[] #'my/control-bracket-as-escape))))

(add-hook 'after-init-hook #'my/distinguish-control-bracket)
(add-hook 'after-make-frame-functions #'my/distinguish-control-bracket)

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

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (setopt evil-collection-key-blacklist '("gs" "gr"))
  ;; org-agenda: keep evil's built-in emacs state; the agenda keymap in
  ;; my-app-org.el is built for emacs state, so don't let evil-collection
  ;; take it over (newer evil-collection moves it to normal state).
  (let ((excludes '(comint org-agenda)))
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

;; The editing extensions -- the operators and the text objects -- each
;; install themselves as a global minor mode over evil's own keymaps, and not
;; one of them is reachable before there is a file open: a splash screen takes
;; no operator.  Armed at the first visit instead of at the start, and
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
  (global-evil-visualstar-mode 1))

(add-hook 'find-file-hook #'my/arm-evil-extensions)

(use-package evil-lion
  :defer t)

(use-package evil-commentary
  :defer t
  :diminish evil-commentary-mode)

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

;; Backspace takes the whole run of whitespace behind point, newlines
;; included, and one character otherwise -- Emacs's own command under its
;; `all' method.  Insert state remaps DEL's command to its own, which calls
;; `delete-backward-char' and so is never hungry; the remap goes in insert
;; state's map, ahead of that one and of Org's, which remaps the same
;; command.  A mode that binds DEL to `backward-delete-char-untabify'
;; outright, as the Lisp modes do, needs nothing.  cc-mode has a backspace
;; of its own, which does the same once its hungry state is on.
(setq backward-delete-char-untabify-method 'all)
(my/define-key
 (:map evil-insert-state-map
       :after evil
       :key "<remap> <delete-backward-char>" #'backward-delete-char-untabify))

;; Folding is evil's own: the `z' keys drive whichever of outline,
;; hideshow and the rest the buffer has on, so hideshow on in code is all it
;; needs.
(use-package hideshow
  :straight nil
  :diminish hs-minor-mode
  :defer t
  :init
  (my/add-hook
   (:hook prog-mode-hook
          :func #'hs-minor-mode)))

;; Keys that go on repeating without their prefix: after `C-w +', a bare `+'
;; again, until any other key.  Each map below names the commands it repeats,
;; and `:repeat t' marks every one of them, which is all `repeat-mode' reads.
(use-package repeat
  :straight nil
  :after evil
  :config
  (defvar-keymap my/window-repeat-map
    :doc "Window keys that repeat after `C-w'."
    :repeat t
    "+" #'evil-window-increase-height
    "-" #'evil-window-decrease-height
    ">" #'evil-window-increase-width
    "<" #'evil-window-decrease-width
    "C-w" #'evil-window-next
    "C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right
    "e" #'my/cycle-frame-size)

  (defvar-keymap my/tab-repeat-map
    :doc "Tab keys that repeat after `gt' or `gT'."
    :repeat t
    "t" #'my/evil-tab-next
    "T" #'my/evil-tab-previous)

  ;; One map each: the repeating key is `b' both ways, and which way it goes
  ;; is the bracket that started it.
  (defvar-keymap my/prev-buffer-repeat-map
    :repeat t
    "b" #'evil-prev-buffer)
  (defvar-keymap my/next-buffer-repeat-map
    :repeat t
    "b" #'evil-next-buffer)

  (my/define-key
   (:map evil-window-map
         :key
         "e" #'my/cycle-frame-size)
   (:map evil-motion-state-map
         :key
         "[b" #'evil-prev-buffer
         "]b" #'evil-next-buffer))

  (repeat-mode 1))

(provide 'my-editor-evil)
;;; my-editor-evil.el ends here
