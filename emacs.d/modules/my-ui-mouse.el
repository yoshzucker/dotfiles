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

(defun my/context-menu-frame (menu _click)
  "Add a \"Frame\" submenu to MENU."
  (easy-menu-add-item
   menu nil
   (easy-menu-create-menu
    "Frame"
    '(["New Frame"         make-frame-command       t]
      ["Delete Frame"      delete-frame             t]
      "---"
      ["Toggle Fullscreen" toggle-frame-fullscreen  t]
      ["Toggle Maximized"  toggle-frame-maximized   t])))
  menu)

(defun my/context-menu-window (menu _click)
  "Add a \"Window\" submenu to MENU."
  (easy-menu-add-item
   menu nil
   (easy-menu-create-menu
    "Window"
    '(["Split Below"          split-window-below    t]
      ["Split Right"          split-window-right    t]
      "---"
      ["Balance Windows"      balance-windows       t]
      "---"
      ["Quit Window"          quit-window           t]
      ["Delete Window"        delete-window         t]
      ["Delete Other Windows" delete-other-windows  t])))
  menu)

(defun my/context-menu-buffer (menu _click)
  "Add a \"Buffer\" submenu to MENU."
  (easy-menu-add-item
   menu nil
   (easy-menu-create-menu
    "Buffer"
    '(["Switch Buffer"   switch-to-buffer  t]
      ["Previous Buffer" previous-buffer   t]
      ["Next Buffer"     next-buffer       t]
      "---"
      ["Kill Buffer"     kill-this-buffer  t])))
  menu)

(defun my/context-menu-dired (menu _click)
  "Add a \"Dired\" submenu to MENU outside of Dired buffers."
  (unless (derived-mode-p 'dired-mode)
    (easy-menu-add-item
     menu nil
     (easy-menu-create-menu
      "Dired"
      '(["Open"                 dired-jump                   t]
        ["Open in Other Window" dired-jump-other-window      t]
        "---"
        ["Toggle Sidebar"       dired-sidebar-toggle-sidebar t]))))
  menu)

;; Register so the visual order top-to-bottom is Frame, Window, Buffer, Dired.
;; `add-hook' prepends by default and hooks run in list order; each function
;; appends its submenu, so registering in reverse gives the desired order.
(dolist (fn '(my/context-menu-dired
              my/context-menu-buffer
              my/context-menu-window
              my/context-menu-frame))
  (add-hook 'context-menu-functions fn))

(provide 'my-ui-mouse)
;;; my-ui-mouse.el ends here
