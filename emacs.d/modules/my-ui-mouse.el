;;; my-ui-mouse.el --- Mouse support and context menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Mouse wheel / xterm mouse / pixel-scroll setup, plus the global
;; right-click context menu and its helpers.

;;; Code:

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

(provide 'my-ui-mouse)
;;; my-ui-mouse.el ends here
