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

;; ---- File open policy (system / emacs / pandoc text) ----

(defcustom my/open-with-system-extensions
  '("app" "exe" "png" "svg" "lnk" "url" "docx" "xlsx" "pptx")
  "File extensions opened with the OS default app under method `auto'.

Used when FORCE is nil.  Plain C-u force prefers in-Emacs open instead
\(see `my/open-as-text-extensions')."
  :type '(repeat string)
  :group 'my-config)

(defcustom my/open-as-text-extensions
  '("docx" "xlsx" "pptx")
  "File extensions opened as pandoc-converted text under method `text'.

Also used under method `auto' when FORCE is a plain C-u."
  :type '(repeat string)
  :group 'my-config)

(defcustom my/open-as-text-pandoc-format
  "markdown"
  "Pandoc output format for `my/open--text' (e.g. \"markdown\", \"plain\")."
  :type 'string
  :group 'my-config)

(defun my/open--extension (filename)
  "Return the lower-case extension of FILENAME, or \"\"."
  (downcase (or (file-name-extension filename) "")))

(defun my/open-with-system-extension-p (filename)
  "Return non-nil if FILENAME should open with the OS default app.

Membership is tested against `my/open-with-system-extensions'."
  (member (my/open--extension filename) my/open-with-system-extensions))

(defun my/open-as-text-extension-p (filename)
  "Return non-nil if FILENAME should open as pandoc text.

Membership is tested against `my/open-as-text-extensions'."
  (member (my/open--extension filename) my/open-as-text-extensions))

(defun my/open--force-p (arg)
  "Return non-nil if ARG is a plain C-u (raw prefix \\='(4)).

Integer prefixes such as C-u 2 are not treated as force."
  (equal arg '(4)))

(defun my/open--resolve-method (path method force)
  "Resolve open METHOD for PATH with FORCE flag.

METHOD is `auto' (default), `emacs', `system', or `text'.
FORCE is meaningful only for `auto' (plain C-u via `my/open--force-p')."
  (let ((method (or method 'auto))
        (force (my/open--force-p force)))
    (pcase method
      ((or 'emacs 'system 'text) method)
      ('auto
       (cond
        ((and force (my/open-as-text-extension-p path)) 'text)
        (force 'emacs)
        ((my/open-with-system-extension-p path) 'system)
        (t 'emacs)))
      (_ (error "Unknown open method: %S" method)))))

(defun my/open-system-command ()
  "Return (PROGRAM . PATH-TRANSFORM) for the OS default open command."
  (cond
   (my/wsl-p
    '("wsl-open" . identity))
   ((eq system-type 'darwin)
    '("open" . identity))
   ((eq system-type 'gnu/linux)
    '("xdg-open" . identity))
   ((eq system-type 'windows-nt)
    '("open" . (lambda (path)
                 (replace-regexp-in-string "/" "\\" path t t))))
   (t
    (error "No system open command for %S" system-type))))

(defun my/start-process (name buffer program &rest program-args)
  "Start PROGRAM like `start-process', with a Windows shell-execute fallback."
  (pcase system-type
    ('windows-nt (w32-shell-execute program (car program-args)))
    (_ (apply #'start-process name buffer program program-args))))

(defun my/open--system (path)
  "Open PATH with the OS default application.  Return nil."
  (let ((path (expand-file-name path)))
    (unless (file-exists-p path)
      (user-error "Path %s does not exist" path))
    (let* ((cmd (my/open-system-command))
           (program (car cmd))
           (arg-fn (cdr cmd))
           (truepath (file-truename path)))
      (my/start-process program nil program (funcall arg-fn truepath)))
    nil))

(defun my/open-system (path)
  "Open PATH with the OS default application.

Interactive and Embark-friendly (prompts for a file)."
  (interactive
   (list (read-file-name "Open with system app: " nil
                         (or (buffer-file-name) default-directory) t)))
  (my/open--system path))

(defvar-local my/open-as-text-source-file nil
  "Absolute path of the office file that produced this text buffer.")

(defvar-local my/open-as-text-source-mtime nil
  "Modification time of `my/open-as-text-source-file' at conversion.")

(defun my/open--text-buffer-name (file)
  "Buffer name for the pandoc text view of FILE."
  (format "*office: %s*" (file-name-nondirectory file)))

(defun my/open--text-reusable-buffer (file mtime)
  "Return an existing text buffer for FILE if still valid for MTIME."
  (let ((buf (get-buffer (my/open--text-buffer-name file))))
    (when (and buf
               (buffer-live-p buf)
               (with-current-buffer buf
                 (and (equal my/open-as-text-source-file file)
                      (equal my/open-as-text-source-mtime mtime))))
      buf)))

(defun my/open--text (file &optional force-reload)
  "Convert FILE with pandoc into a read-only buffer and return it.

Does not set `buffer-file-name' to FILE (avoids overwriting the binary).
Reuse an existing buffer when source mtime is unchanged, unless
FORCE-RELOAD is non-nil."
  (let* ((file (expand-file-name file))
         (attrs (file-attributes file))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (ext (my/open--extension file))
         existing)
    (unless (and attrs (null (file-attribute-type attrs)))
      (user-error "Not a regular file: %s" file))
    (unless (executable-find "pandoc")
      (user-error "pandoc not found; install pandoc to open office files as text"))
    (setq existing (and (not force-reload)
                        (my/open--text-reusable-buffer file mtime)))
    (if existing
        (progn
          (pop-to-buffer-same-window existing)
          existing)
      (let ((buf (get-buffer-create (my/open--text-buffer-name file)))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8)
            exit)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (setq exit
                  (call-process
                   "pandoc" nil t nil
                   file
                   (concat "--from=" ext)
                   (concat "--to=" my/open-as-text-pandoc-format)
                   "--wrap=none"))
            (unless (eq exit 0)
              (let ((err (string-trim (buffer-string))))
                (erase-buffer)
                (user-error "pandoc failed for %s (exit %s)%s"
                            file exit
                            (if (string-empty-p err) "" (concat ": " err)))))
            (goto-char (point-min))
            (insert (format "<!-- source: %s -->\n" file)
                    (format "<!-- converted: %s -->\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")))
            (if (fboundp 'markdown-mode)
                (markdown-mode)
              (text-mode))
            (setq-local my/open-as-text-source-file file
                        my/open-as-text-source-mtime mtime
                        buffer-read-only t
                        buffer-undo-list t)
            ;; Never bind buffer-file-name to the office path.
            (setq buffer-file-name nil)
            (set-buffer-modified-p nil)))
        (pop-to-buffer-same-window buf)
        buf))))

(defun my/open-as-text (path &optional force-reload)
  "Open PATH as pandoc text (interactive wrapper for `my/open--text').

Interactive and Embark-friendly (prompts for a file)."
  (interactive
   (list (read-file-name "Open as text: " nil
                         (or (buffer-file-name) default-directory) t)
         current-prefix-arg))
  (my/open--text path force-reload))

(defun my/open-file (path &optional method force)
  "Open PATH using METHOD (`auto', `emacs', `system', or `text').

METHOD defaults to `auto'.  FORCE (plain C-u) affects `auto' only:
prefer pandoc text for `my/open-as-text-extensions', else in-Emacs
`find-file', instead of the OS default app.

Return a buffer for `emacs'/`text', or nil for `system'.
Interactive and Embark-friendly (prompts for a file)."
  (interactive
   (list (read-file-name "Open file: " nil
                         (or (buffer-file-name) default-directory) t)
         'auto
         current-prefix-arg))
  (let* ((path (expand-file-name path))
         (resolved (my/open--resolve-method path method force)))
    (pcase resolved
      ('system
       (when (fboundp 'recentf-push)
         (recentf-push path))
       (my/open--system path))
      ('text
       (when (fboundp 'recentf-push)
         (recentf-push path))
       (my/open--text path))
      ('emacs
       (find-file path)))))

(defun my/open-file-in-emacs (path)
  "Open PATH inside Emacs (same as plain C-u force open).

For extensions in `my/open-as-text-extensions', convert with pandoc.
Otherwise use normal `find-file'.  Interactive and Embark-friendly."
  (interactive
   (list (read-file-name "Open in Emacs: " nil
                         (or (buffer-file-name) default-directory) t)))
  (my/open-file path 'auto '(4)))

(defun my/find-file-open-advice (orig filename &optional wildcards)
  "Around advice for `find-file': honor `my/open-with-system-extensions'.

Force (plain C-u) is not taken from `current-prefix-arg' here, because
`find-file' already uses C-u for wildcards.  Use dired C-u RET or
`my/open-file' for pandoc text open."
  (pcase (my/open--resolve-method filename 'auto nil)
    ('emacs (funcall orig filename wildcards))
    ('system
     (when (fboundp 'recentf-push)
       (recentf-push filename))
     (my/open--system filename)
     nil)
    ('text
     (when (fboundp 'recentf-push)
       (recentf-push filename))
     (my/open--text filename))))

(advice-add 'find-file :around #'my/find-file-open-advice)

(defun my/embark-bind-file-open-actions ()
  "Expose open-policy commands on `embark-file-map'.

Bindings (also listed by `embark-completing-read-prompter'):
  x  `my/open-system'        — OS default app (same path as dired C-c C-o)
  t  `my/open-as-text'       — pandoc text buffer
  E  `my/open-file-in-emacs' — force in-Emacs (C-u RET equivalent)

Default RET/f remain `find-file', which already honors
`my/open-with-system-extensions' via `my/find-file-open-advice'."
  (when (boundp 'embark-file-map)
    (define-key embark-file-map (kbd "x") #'my/open-system)
    (define-key embark-file-map (kbd "t") #'my/open-as-text)
    (define-key embark-file-map (kbd "E") #'my/open-file-in-emacs)))

(with-eval-after-load 'embark
  (my/embark-bind-file-open-actions))

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
         "C-c C-d" #'my/dired-open-dir-with-system
         "C-c =" #'my/dired-ediff-dwim))
  
  (my/define-key
   (:map dired-mode-map
         :after evil-collection
         :key "<mouse-2>" #'dired-find-file)
   (:map dired-mode-map
         :state normal
         :after evil-collection
         :key
         "<mouse-2>" #'dired-find-file
         "RET" #'my/dired-find-marked-files))
  
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

  ;; Dired entry points for `my/open-file' policy
  (defun my/dired-mapc-marked-files (func &optional arg)
    "Call FUNC on each marked file (or file at point).

ARG is passed to `dired-get-marked-files'."
    (mapc func (dired-get-marked-files nil arg)))

  (defun my/dired-find-marked-files (&optional arg)
    "Open marked files via `my/open-file'.

ARG is passed to `dired-get-marked-files' (plain C-u: current file only).
A plain C-u also forces in-Emacs open: pandoc text for
`my/open-as-text-extensions', otherwise `find-file'."
    (interactive "P")
    (let ((force current-prefix-arg))
      (dolist (f (dired-get-marked-files nil arg))
        (my/open-file f 'auto force))))

  (defun my/dired-view-marked-files (&optional arg)
    "View marked files; Office + plain C-u opens pandoc text instead.

ARG is passed to `dired-get-marked-files'."
    (interactive "P")
    (let ((force current-prefix-arg))
      (dolist (f (dired-get-marked-files nil arg))
        (pcase (my/open--resolve-method f 'auto force)
          ('text (my/open--text f))
          ('system
           (when (fboundp 'recentf-push)
             (recentf-push f))
           (my/open--system f))
          ('emacs (view-file f))))))

  (defun my/dired-open-file-with-system ()
    "Open file at point in dired with the OS default application."
    (interactive)
    (my/open--system (dired-get-filename nil t)))

  (defun my/dired-open-dir-with-system ()
    "Open current dired directory with the OS default application."
    (interactive)
    (my/open--system (dired-current-directory)))

  (defun my/dired-ediff-dwim ()
    "Ediff two files from Dired.

If both are in `my/open-as-text-extensions', convert with pandoc and
run `ediff-buffers'.  Otherwise run `ediff-files'.

With one marked file (or only file at point), prompt for the other."
    (interactive)
    (let* ((marked (dired-get-marked-files))
           (files
            (cond
             ((= (length marked) 2) marked)
             ((= (length marked) 1)
              (list (car marked)
                    (read-file-name
                     (format "Ediff %s with: "
                             (file-name-nondirectory (car marked)))
                     (dired-dwim-target-directory)
                     nil t)))
             (t (user-error "Mark 1 or 2 files for ediff")))))
      (let ((a (expand-file-name (car files)))
            (b (expand-file-name (cadr files))))
        (if (and (my/open-as-text-extension-p a)
                 (my/open-as-text-extension-p b))
            (let ((buf-a (my/open--text a))
                  (buf-b (my/open--text b)))
              (ediff-buffers buf-a buf-b))
          (ediff-files a b)))))

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
                       '(utf-8-unix . cp932-dos)
                     'utf-8-unix))
           (shell (if (eq system-type 'windows-nt)
                      (or (executable-find "bash") shell-file-name)
                    shell-file-name))
           (switch (if (eq system-type 'windows-nt) "-c" shell-command-switch))
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
                   :command (list shell switch command)
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
