;;; my-app-eww.el --- EWW configuration for browsing and search -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up EWW with buffer naming, image toggle, and jump-to-section behavior.
;; Also supports optional WSL integration for external browser handling.

;;; Code:

(use-package eww
  :defer t
  :init
  (when my/wsl-p
    (defun my/browse-url-wsl-browser (url &optional _new-window)
      "Open URL in the WSL host's default browser."
      (interactive (browse-url-interactive-arg "URL: "))
      (start-process "wsl-open" nil "wsl-open" url))

    (setq browse-url-secondary-browser-function #'my/browse-url-wsl-browser))
  :config
  (setq eww-search-prefix "https:/duckduckgo.com/q=")

  ;; Rename EWW buffers for multi-tab use
  (defun my/eww-rename-buffer ()
    "Give EWW buffer a unique name."
    (rename-buffer "eww" t))

  (add-hook 'eww-mode-hook #'my/eww-rename-buffer)

  ;; Use fixed-pitch font in terminal
  (unless (display-graphic-p)
    (setq shr-use-fonts nil))

  ;; Optional image toggle
  (setq eww-images t)

  (defun my/shr-put-image (_spec alt &optional _flags)
    "Insert ALT text instead of image."
    (insert alt))

  (defun my/eww-toggle-images (&optional arg)
    "Toggle inline image display in EWW. With ARG, enable if positive."
    (interactive "P")
    (setq eww-images (if arg (> (prefix-numeric-value arg) 0) (not eww-images)))
    (setq-local shr-put-image-function
                (if eww-images #'shr-put-image #'my/shr-put-image))
    (message "EWW images %s" (if eww-images "enabled" "disabled"))
    (eww-reload))

  ;; Auto-scroll to search result section
  (defcustom my/eww-start-position-alist nil
    "Alist of site-specific regex rules for positioning EWW buffers.

Each entry should be a cons cell of the form (DOMAIN . REGEXP), where:
- DOMAIN is a string representing the host name (e.g., \"www.example.com\")
- REGEXP is a regular expression string to match a specific position
  within the rendered EWW buffer.

If nil, no automatic positioning will be performed."
  :type '(alist :key-type string :value-type string)
  :group 'my/app)

  (defun my/eww-render-jump ()
    "Auto-jump to a specific section after rendering."
    (let ((url (eww-current-url)))
      (cl-loop for (pattern . regexp) in my/eww-start-position-alist
               when (string-match pattern url)
               do (goto-char (point-min))
                  (when (re-search-forward regexp nil t)
                    (recenter 0)
                    (cl-return t)))))

  (add-hook 'eww-after-render-hook #'my/eww-render-jump))

(provide 'my-app-eww)
;;; my-app-eww.el ends here
