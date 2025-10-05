;;; my-ui-cursor.el --- Cursor appearance and behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up cursor appearance, scrolling behavior, mouse support,
;; and terminal cursor shape using ANSI escape sequences.

;;; Code:

;; Cursor appearance
(setq-default cursor-in-non-selected-windows nil)
(setq blink-cursor-blinks 1)

;; Scroll behavior
(setq scroll-conservatively 5
      scroll-margin 0)

;; Mouse support
(when (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode 1))
(when (and (not (display-graphic-p)) (fboundp 'xterm-mouse-mode))
  (xterm-mouse-mode 1))

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum t
      pixel-scroll-precision-interpolate-page t
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Context menu for mouse
(context-menu-mode 1)

(defun my/context-open-containing-dir ()
  "Open Dired for the current buffer's file (or `default-directory`)."
  (interactive)
  (cond
   (buffer-file-name (dired-jump nil buffer-file-name))
   (t (dired default-directory))))

(defun my/context-open-parent-dir ()
  "Open Dired at the parent directory of the current buffer/file."
  (interactive)
  (let* ((dir (or (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  default-directory))
         (parent (file-name-directory (directory-file-name dir))))
    (if parent
        (dired parent)
      (user-error "No parent directory"))))

(defun my/global-context-menu (menu click)
  "Add handy items everywhere. Always append to and return MENU (a keymap)."
  (ignore click)  ;; we don't need the click object here
  (let ((submenu
         (easy-menu-create-menu
          "Navigate"
          (cond
           ((derived-mode-p 'dired-mode)
            '(["Open"                 dired-find-file t]
              ["Open in other window" dired-find-file-other-window t]
              "---"
              ["Parent Directory"     dired-up-directory t]
              "---"
              ["Close Buffer"         kill-this-buffer t]
              ["Close Window"         quit-window t]))
           (t
            '(["Containing Directory" my/context-open-containing-dir t]
              ["Parent Directory"     my/context-open-parent-dir t]
              "---"
              ["Close Buffer"         kill-this-buffer t]
              ["Close Window"         quit-window t]))))))
    (easy-menu-add-item menu nil submenu)
    menu))

(add-hook 'context-menu-functions #'my/global-context-menu)

;; Terminal-specific cursor settings
(unless (display-graphic-p)

  ;; Cursor shape escape sequences
  (defconst my/cursor-shape-table
    '((iterm . ((line  . "\e]50;CursorShape=1\x7")
                (block . "\e]50;CursorShape=0\x7")))
      (ansi  . ((line  . "\e[5 q")
                (block . "\e[1 q")))))

  ;; Tmux escape sequence wrapping
  (defconst my/tmux-prefix "\ePtmux;\e")
  (defconst my/tmux-suffix "\e\\")

  (defun my/escape-seq (shape)
    "Return appropriate escape sequence string for cursor SHAPE (:line or :block)."
    (let* ((term (if (equal (getenv "TERM_PROGRAM") "iTerm.app") 'iterm 'ansi))
           (seq  (cdr (assoc shape (alist-get term my/cursor-shape-table))))
           (tmux (getenv "TMUX")))
      (concat (if tmux my/tmux-prefix "")
              seq
              (if tmux my/tmux-suffix ""))))

  (defun my/set-cursor-line ()
    (send-string-to-terminal (my/escape-seq 'line)))

  (defun my/set-cursor-block ()
    (send-string-to-terminal (my/escape-seq 'block)))

  ;; Hook into Evil insert state transitions
  (my/add-hook
   (:hook evil-insert-state-entry-hook :func #'my/set-cursor-line)
   (:hook evil-insert-state-exit-hook :func #'my/set-cursor-block)))

(provide 'my-ui-cursor)
;;; my-ui-cursor.el ends here
