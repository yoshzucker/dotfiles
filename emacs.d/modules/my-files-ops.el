;;; my-files-ops.el --- File management and navigation settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Provides file and directory navigation enhancements like dired and ibuffer.

;;; Code:
;;; my-files.el --- File management and navigation settings -*- lexical-binding: t; -*-
;;; Commentary:
;; File navigation, dired enhancements, and external file handling.

;;; Code:

(use-package unify-opening)

(use-package dired
  :straight nil
  :defer t
  :after evil
  :config
  (setq dired-dwim-target t
        dired-auto-revert-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Keybindings
  (my/define-key
   (:map dired-mode-map
         :key
         "V" #'my/dired-view-marked-files
         "RET" #'my/dired-find-marked-files
         "C-c C-o" #'my/dired-open-file-with-system
         "C-c C-d" #'my/dired-open-dir-with-system))

  ;; Evil integration
  (dolist (key '("n" "N" "g" "G"))
    (define-key dired-mode-map (kbd key)
                (lookup-key evil-motion-state-map (kbd key))))
  (my/define-key
   (:map dired-mode-map :state motion :key "gf" #'find-file))
  
  ;; Hooks
  (my/add-hook
   (:hook dired-mode-hook
          :func 
          #'dired-hide-details-mode
          #'my/dired-buffer-append-slash))

  (defun my/dired-buffer-append-slash ()
    "Append a slash to the dired buffer name for easier identification."
    (when (eq major-mode 'dired-mode)
      (rename-buffer (concat (buffer-name) "/") t)))

  ;; Sort .. and . at top
  (defun my/dired-keep-dot-top ()
    "Keep '.' and '..' entries at the top in dired listings."
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t))
	(save-excursion
          (goto-char (point-min))
          (forward-line 2)
          (sort-regexp-fields t "^.*$" "[ ]*[.]*$" (point) (point-max))))))
  (advice-add 'dired-readin :after #'my/dired-keep-dot-top)

  ;; Alist of (program . path-transform) per system
  (defconst my/open-program-alist
    (cond
     (my/wsl-p
      '("wsl-open" . identity))
     ((eq system-type 'darwin)
      '("open" . identity))
     ((eq system-type 'gnu/linux)
      '("xdg-open" . identity))
     ((eq system-type 'windows-nt)
      '("open" . (lambda (path)
                   (replace-regexp-in-string "/" "\\" path t t))))))
  
  ;; Apply FUNC to marked files in Dired (or to file at point if none marked)
  (defun my/dired-mapc-marked-files (func &optional arg)
    "Open marked files using MODE ('find-file or 'view-file)."
    (let ((files (dired-get-marked-files nil arg)))
      (mapc func files)))
  
  (defun my/dired-find-marked-files (&optional arg)
    (interactive "P")
    (my/dired-mapc-marked-files #'find-file arg))
  
  (defun my/dired-view-marked-files (&optional arg)
    (interactive "P")
    (my/dired-mapc-marked-files #'view-file arg))

  ;; Start process depending on system
  (defun my/start-process (name buffer program &rest program-args)
    (pcase system-type
      ('windows-nt (w32-shell-execute program (car program-args)))
      (_ (apply #'start-process name buffer program program-args))))
  
  ;; Open file or directory with system default app
  (defun my/open-with-system (&optional path)
    "Open PATH with the system's default application."
    (let ((path (expand-file-name (or path (buffer-file-name)))))
      (cond
       ((not (file-exists-p path))
	(message "Path %s does not exist" path))
       (t
	(let ((program (car my/open-program-alist))
              (arg-fn (cdr my/open-program-alist)))
          (my/start-process program nil program (funcall arg-fn path)))))))
  
  (defun my/dired-open-file-with-system ()
    "Open file at point in dired with the system's default application."
    (interactive)
    (my/open-with-system (dired-get-filename nil t)))
  
  (defun my/dired-open-dir-with-system ()
    "Open directory in dired with the system's default application."
    (interactive)
    (my/open-with-system (dired-current-directory)))

  (defvar my/open-with-system-ext
    '("app" "exe" "png" "svg" "lnk" "url" "docx" "xlsx" "pptx")
    "List of file extensions to open with system apps.")
  
  (defun my/open-with-system-ext-p (filename)
    "Return non-nil if FILE-OR-DIR should be opened externally based on its extension."
    (member (downcase (or (file-name-extension filename) "")) my/open-with-system-ext))
  
  (defun my/find-file-maybe-external (f filename &optional wildcards)
    "Open FILENAME externally if extension matches, otherwise call F."
    (if (my/open-with-system-ext-p filename)
	(progn (recentf-push filename) (my/open-with-system filename))
      (funcall f filename wildcards)))
  
  (advice-add 'find-file :around #'my/find-file-maybe-external)

  ;; Drag and drop support in dired
  (setq dired-dnd-protocol-alist
        '(("^file:///" . dired-dnd-handle-local-file-move)
          ("^file://"  . dired-dnd-handle-file-move)
          ("^file:"    . dired-dnd-handle-local-file-move)))

  (defun dired-dnd-handle-local-file-move (uri action)
    (dired-dnd-handle-local-file uri 'move))

  (defun dired-dnd-handle-file-move (uri action)
    (dired-dnd-handle-file uri 'move))

  ;; Rename (R) uses visible dired as default target unless given C-u
  (advice-add 'dired-do-rename :override #'my/dired-do-rename)
  (defun my/dired-do-rename (&optional arg)
    (let ((dired-dwim-target (not arg)))
      (dired-do-create-files 'move #'dired-rename-file "Move" nil dired-keep-marker-rename "Rename"))))

(use-package wdired
  :defer t
  :after (evil dired)
  :config
  (my/define-key
   (:map wdired-mode-map :key "C-c C-k" #'wdired-abort-changes))
  
  ;; Always start in normal state (avoid insert-mode confusion)
  (advice-add 'wdired-change-to-wdired-mode :after
              (lambda () (evil-normal-state)))

  ;; Restore dired hide-details after abort
  (advice-add 'wdired-abort-changes :after
              (lambda (&rest _) (dired-hide-details-mode)))

  ;; Change background while in wdired
  (advice-add 'wdired-change-to-wdired-mode :after
              (lambda ()
                (face-remap-add-relative
                 'default
                 :background (if (eq frame-background-mode 'light)
                                 my/white
                                 my/black))))

  ;; Restore background after leaving wdired
  (advice-add 'wdired-change-to-dired-mode :after
              (lambda ()
                (face-remap-add-relative 'default :background my/background))))

(use-package dired-subtree
  :defer t
  :after (evil dired)
  :init
  (my/define-key
   (:map dired-mode-map :state normal :key "TAB" #'dired-subtree-cycle)))

(use-package dired-filter
  :after (dired evil)
  :config
  ;; Use dired-filter with C-c /, keep / for evil motion
  (my/define-key
   (:map dired-mode-map
         :key
         "C-c /" #'dired-filter-map
         "/" (lookup-key evil-motion-state-map "/"))))

(use-package ibuffer
  :config
  (my/define-key
   (:map global-map :key "C-x C-b" #'ibuffer)))

(provide 'my-files-ops)
;;; my-files-ops.el ends here

