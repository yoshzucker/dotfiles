;;; my-files-ops.el --- File management and navigation settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Provides file and directory navigation enhancements like dired and ibuffer.

;;; Code:
;;; my-files.el --- File management and navigation settings -*- lexical-binding: t; -*-
;;; Commentary:
;; File navigation, dired enhancements, and external file handling.

;;; Code:

(defcustom my/base-directory "~/"
  "Base directory used as the starting point for `my/find-file-from-base`."
  :type 'directory
  :group 'my-config)

(defun my/find-file-from-base ()
  "Start `find-file` from `my/base-directory`."
  (interactive)
  (let ((default-directory (expand-file-name my/base-directory)))
    (call-interactively #'find-file)))

(use-package unify-opening)

(use-package dired
  :straight nil
  :defer t
  :after evil
  :config
  (when (memq system-type '(darwin gnu/linux))
    (setq dired-listing-switches "-alh")
    (when (executable-find "gls")
      (setq insert-directory-program (executable-find "gls")
            ls-lisp-use-insert-directory-program t
            dired-use-ls-dired t
            dired-listing-switches "-alh --group-directories-first")))
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
  
  (my/define-key
   (:map dired-mode-map
         :after evil-collection
         :key "<mouse-2>" #'dired-find-file)
   (:map dired-mode-map
         :state normal
         :after evil-collection
         :key "<mouse-2>" #'dired-find-file))
  
  ;; Evil integration
  (dolist (key '("n" "N" "g" "G"))
    (define-key dired-mode-map (kbd key)
                (lookup-key evil-motion-state-map (kbd key))))
  (my/define-key
   (:map dired-mode-map
         :state normal
         :key
         "r" #'revert-buffer
         "gf" #'find-file-at-point
         "gh" #'my/find-file-from-base))
  
  ;; Hooks
  (my/add-hook
   (:hook dired-mode-hook
          :func
          #'dired-hide-details-mode
          #'my/dired-buffer-append-slash
          #'my/dired-enable-auto-revert))
  
  (defun my/dired-buffer-append-slash ()
    "Append a slash to the dired buffer name for easier identification."
    (when (eq major-mode 'dired-mode)
      (rename-buffer (concat (buffer-name) "/") t)))

  (defun my/dired-enable-auto-revert ()
    "Enable `auto-revert-mode' for Dired buffers, except for remote directories."
    (unless (file-remote-p default-directory)
      (auto-revert-mode)))

  (setq dired-clean-up-buffers-too t)
  
  ;; Sort .. and . at top
  (defun my/dired-keep-dot-top ()
    "Keep '.' and '..' entries at the top in dired listings."
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
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
              (arg-fn (cdr my/open-program-alist))
              (truepath (file-truename path)))
          (my/start-process program nil program (funcall arg-fn truepath)))))))
  
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
  
  ;; Omit
  (when (eq system-type 'windows-nt)
    (setq dired-omit-files
          (rx (or (seq bol "desktop.ini")
                  (seq bol "NTUSER" (* any) eol)
                  (seq bol "Thumbs.db")
                  (seq bol "My Documents")
                  (seq bol "My Music")
                  (seq bol "My Pictures")
                  (seq bol "My Videos"))))
    
    (my/add-hook (:hook dired-mode-hook :func #'dired-omit-mode)))
  
  ;; Rename (R) uses visible dired as default target unless given C-u
  (advice-add 'dired-do-rename :override #'my/dired-do-rename)
  (defun my/dired-do-rename (&optional arg)
    (let ((dired-dwim-target (not arg)))
      (dired-do-create-files 'move #'dired-rename-file "Move" nil dired-keep-marker-rename "Rename")))

  (defvar my/fd-dired-history nil
    "History list for `my/fd-dired' patterns.")

  (defun my/fd-dired-sentinel (proc state)
    "Sentinel for `my/fd-dired' processes.
Appends a completion summary line in the result buffer and echoes a
\"fd-dired … finished.\" message, mirroring `find-dired-sentinel'."
    (let ((buf (process-buffer proc)))
      (when (buffer-name buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (save-restriction
                (widen)
                (let ((point (point-max)))
                  (goto-char point)
                  (insert "\n  fd "
                          (substring state 0 -1)
                          " at " (substring (current-time-string) 0 19))
                  (dired-insert-set-properties point (point))))
              (setq mode-line-process (format ":%s" (process-status proc)))
              (delete-process proc)
              (force-mode-line-update))))
        (message "fd-dired %s finished." buf))))

  (defun my/fd-dired (dir pattern)
    "Run `fd' asynchronously and display results in a `find-dired'-style buffer.
DIR is the search root.  PATTERN is passed to `fd' as the search pattern.
Buffer name format: *fd <pattern> <root-dir>/*

The function mirrors built-in `find-dired' for buffer layout, prompts and
completion messages, while running the search via `make-process' through
a shell pipeline `fd … -0 | xargs -0 ls -ld 2>/dev/null'.  Routing
through `ls -ld' avoids the \"gls: cannot access …\" failure that
`(dired (cons DIR FILES))' produces when any collected entry has gone
away between scan and listing.  On Windows the process coding is set to
cp932 so that mojibake doesn't occur in file names."
    (interactive
     (let* ((dir (read-directory-name "Run fd in directory: " nil default-directory t))
            (pattern (read-string
                      (format "Run fd in %s (with args): " (abbreviate-file-name dir))
                      nil 'my/fd-dired-history)))
       (list dir pattern)))
    (require 'find-dired)
    (let* ((search-dir (file-name-as-directory (expand-file-name dir)))
           (root-name (file-name-nondirectory (directory-file-name search-dir)))
           (result-buffer-name (format "*fd %s %s/*" pattern root-name))
           (fd-executable (or (executable-find "fd")
                              (executable-find "fdfind")))
           (ls-program (or (executable-find "gls")
                           (executable-find "ls")))
           (ls-switches (if (and ls-program
                                 (string-match-p "gls\\'" ls-program))
                            "-ldh --group-directories-first"
                          "-ldh"))
           (coding (if (eq system-type 'windows-nt)
                       '(cp932-dos . cp932-dos)
                     'utf-8-unix))
           command)
      (unless fd-executable
        (user-error "fd command not found"))
      (unless ls-program
        (user-error "ls/gls command not found"))
      (unless (file-directory-p search-dir)
        (error "my/fd-dired needs a directory: %s" search-dir))
      (setq command
            (format "%s --color=never -0%s | xargs -0 %s %s 2>/dev/null"
                    (shell-quote-argument fd-executable)
                    (if (string= pattern "")
                        ""
                      (concat " " (shell-quote-argument pattern)))
                    (shell-quote-argument ls-program)
                    ls-switches))
      (pop-to-buffer-same-window (get-buffer-create result-buffer-name))
      (let ((running (get-buffer-process (current-buffer))))
        (when running
          (if (or (not (eq (process-status running) 'run))
                  (yes-or-no-p (format-message "A `fd' process is running; kill it? ")))
              (condition-case nil
                  (progn (interrupt-process running)
                         (sit-for 1)
                         (delete-process running))
                (error nil))
            (error "Cannot have two processes in `%s' at once" (buffer-name)))))
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory search-dir)
      (let ((proc (make-process
                   :name "fd-dired"
                   :buffer (current-buffer)
                   :command (list shell-file-name shell-command-switch command)
                   :coding coding
                   :connection-type 'pipe
                   :filter #'find-dired-filter
                   :sentinel #'my/fd-dired-sentinel)))
        (move-marker (process-mark proc) (point) (current-buffer)))
      (dired-mode search-dir (cdr find-ls-option))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" 'kill-find)
        (use-local-map map))
      (setq-local dired-sort-inhibit t)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (my/fd-dired search-dir pattern)))
      (if (fboundp 'dired-simple-subdir-alist)
          (dired-simple-subdir-alist)
        (setq dired-subdir-alist
              (list (cons default-directory (point-min-marker)))))
      (setq-local dired-subdir-switches find-ls-subdir-switches)
      (setq buffer-read-only nil)
      (insert "  " search-dir ":\n")
      (when (bound-and-true-p dired-make-directory-clickable)
        (dired--make-directory-clickable))
      (let ((point (point)))
        (insert "  " command "\n")
        (dired-insert-set-properties point (point)))
      (setq buffer-read-only t)
      (setq mode-line-process '(":%s")))))

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
              (lambda (&rest _) (dired-hide-details-mode))))

(use-package dired-subtree
  :defer t
  :after (evil dired)
  :init
  (my/define-key
   (:map dired-mode-map :state normal :key "TAB" #'dired-subtree-cycle))
  :config
  (my/define-key
   (:map dired-mode-map
         :state normal
         :key
         "gh" #'my/find-file-from-base
         "gH" #'dired-subtree-up)))

(use-package dired-filter
  :after (dired evil)
  :config
  ;; Use dired-filter with C-c /, keep / for evil motion
  (my/define-key
   (:map dired-mode-map
         :key
         "C-c /" #'dired-filter-map
         "/" (lookup-key evil-motion-state-map "/"))))

(use-package dired-preview
  :config
  (setq dired-preview-delay 0.25
        dired-preview-max-size (expt 2 20)
        dired-preview-display-action-alist
        '((display-buffer-in-side-window)
          (side . bottom)
          (window-height . 0.4)
          (preserve-size . (t . t))
          (window-parameters . ((no-delete-other-windows . t))))))

(use-package dired-sidebar
  :after dired
  :config
  (my/define-key (:map global-map :key "C-x C-n" #'dired-sidebar-toggle-sidebar))
  (my/add-hook (:hook dired-sidebar-mode-hook :func #'my/dired-enable-auto-revert))

  (setq dired-sidebar-width 20
        dired-sidebar-no-delete-other-windows t)

  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(use-package ibuffer
  :config
  (my/define-key
   (:map global-map :key "C-x C-b" #'ibuffer)))

(provide 'my-files-ops)
;;; my-files-ops.el ends here
