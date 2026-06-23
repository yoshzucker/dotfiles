;;; my-app-marp.el --- Marp slide helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; Drive the Marp CLI from markdown buffers: a live-preview server (start/stop)
;; and one-shot exports.  All commands pass the global config (local-file
;; embedding, raw HTML).  YAML config is used because the Scoop standalone
;; binary cannot load JS/ESM config.
;;
;; Theming is theme-agnostic plumbing: the whole themes dir is registered with
;; --theme-set and a theme is then chosen by its `/* @theme NAME */` name with
;; --theme.  Names are discovered by scanning the dir, so adding a CSS makes it
;; selectable with no code change; theme *colors* live only in those CSS files.
;; The default names are the sole knob here (`my/marp-preview-theme',
;; `my/marp-export-theme').  Preview defaults to the dark, editor-matched
;; gensho-light; export defaults to the white/formal `custom'.  Because --theme
;; overrides the deck's `theme:' directive, the preview override never touches
;; the exported artifact.
;;; Code:

(defgroup my-marp nil
  "Marp slide preview and export helpers."
  :group 'tools)

(defcustom my/marp-server-open-browser t
  "Non-nil means automatically open the preview URL in the default browser when server starts."
  :type 'boolean
  :group 'my-marp)

(defcustom my/marp-preview-theme "gensho-light"
  "Default theme name for the live preview server.
A `/* @theme NAME */' name registered under the themes dir.  Defaults to
the dark, editor-matched theme so the preview does not glare white."
  :type 'string
  :group 'my-marp)

(defcustom my/marp-export-theme "custom"
  "Default theme name for exports.
A `/* @theme NAME */' name registered under the themes dir.  Defaults to
the white/formal theme suited to a real presentation."
  :type 'string
  :group 'my-marp)

(defvar my/marp-config-dir
  (expand-file-name "marp"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name ".config" "~")))
  "Directory holding the global Marp config and themes.")

(defun my/marp--themes-dir ()
  "Return the directory holding theme CSS files."
  (expand-file-name "themes" my/marp-config-dir))

(defun my/marp--available-themes ()
  "Return registered theme names by scanning the themes dir.
Each name is the `/* @theme NAME */' declaration, falling back to the
file's base name when no declaration is found."
  (let ((dir (my/marp--themes-dir)))
    (when (file-directory-p dir)
      (delete-dups
       (mapcar
        (lambda (file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (if (re-search-forward "@theme[ \t]+\\([^ \t*]+\\)" nil t)
                (match-string 1)
              (file-name-base file))))
        (directory-files dir t "\\.css\\'"))))))

(defun my/marp--read-theme (prompt default)
  "Read a theme name with PROMPT, completing on available themes, DEFAULT preselected."
  (completing-read prompt (my/marp--available-themes) nil nil nil nil default))

(defconst my/marp--server-name "marp-server"
  "Process name of the Marp live-preview server.")

(defconst my/marp--server-buffer "*marp-server*"
  "Buffer name for the Marp live-preview server output.")

(defconst my/marp--server-port 8080
  "Port number used by the Marp live server (Marp CLI default).")

(defun my/marp--preview-url ()
  "Return the full preview URL for the Marp live server."
  (format "http://localhost:%d" my/marp--server-port))

(defun my/marp--browser-opening-filter (url)
  "Return a process filter that opens URL in browser once when Marp is ready."
  (let ((opened nil))
    (lambda (proc output)
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              (goto-char (process-mark proc))
              (insert output)
              (set-marker (process-mark proc) (point)))
            (when moving (goto-char (process-mark proc))))))
      (when (and (not opened)
                 (string-match-p (regexp-quote url) output))
        (setq opened t)
        (browse-url url)))))

(defun my/marp--config-args ()
  "Theme-agnostic flags: the global Marp config and theme-set, when present.
The actual theme is chosen separately with --theme by the caller."
  (let ((cfg (expand-file-name "marp.config.yml" my/marp-config-dir))
        (themes (my/marp--themes-dir))
        (args nil))
    (when (file-directory-p themes)
      (setq args (append (list "--theme-set" themes) args)))
    (when (file-exists-p cfg)
      (setq args (append (list "--config-file" cfg) args)))
    args))

(defun my/marp-start-server (&optional arg)
  "Start a Marp live-preview server for the current file's directory.
Uses `my/marp-preview-theme'; with a prefix ARG, prompt for the theme.
If a server is already running, just surface its output buffer."
  (interactive "P")
  (if (process-live-p (get-process my/marp--server-name))
      (progn
        (display-buffer my/marp--server-buffer)
        (message "Marp server already running — %s" (my/marp--preview-url)))
    (let* ((theme (if arg
                      (my/marp--read-theme "Preview theme: " my/marp-preview-theme)
                    my/marp-preview-theme))
           (dir (file-name-directory (or buffer-file-name default-directory)))
           (default-directory dir)
           (buf (get-buffer-create my/marp--server-buffer))
           (args (append (my/marp--config-args)
                         (list "--theme" theme "--server" ".")))
           (proc (apply #'start-process my/marp--server-name buf "marp" args)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc
                            (lambda (p event)
                              (when (memq (process-status p) '(exit signal))
                                (message "Marp server exited: %s" (string-trim event)))))
      (when my/marp-server-open-browser
        (set-process-filter proc (my/marp--browser-opening-filter (my/marp--preview-url))))
      (display-buffer buf)
      (message "Marp server started for %s — open %s"
               (abbreviate-file-name dir)
               (my/marp--preview-url)))))

(defun my/marp-stop-server ()
  "Stop the Marp live-preview server if it is running."
  (interactive)
  (let ((proc (get-process my/marp--server-name)))
    (if (process-live-p proc)
        (progn
          (delete-process proc)
          (message "Marp server stopped"))
      (message "No Marp server is running"))))

(defun my/marp-export (format theme)
  "Export the current buffer's file to FORMAT (pdf, pptx, or html) using THEME."
  (interactive
   (list (completing-read "Marp export: " '("pdf" "pptx" "html") nil t "pdf")
         (my/marp--read-theme "Theme: " my/marp-export-theme)))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((buf (get-buffer-create "*marp-export*")))
    (with-current-buffer buf
      (erase-buffer))
    (apply #'start-process "marp-export" buf "marp"
           (append (my/marp--config-args)
                   (list "--theme" theme
                         (concat "--" format) "--" buffer-file-name)))
    (display-buffer buf)
    (message "Marp exporting %s → %s (%s)…"
             (file-name-nondirectory buffer-file-name) format theme)))

(my/define-key
 (:map markdown-mode-map
       :after markdown-mode
       :key
       "C-c m s" #'my/marp-start-server
       "C-c m k" #'my/marp-stop-server
       "C-c m e" #'my/marp-export))

(provide 'my-app-marp)
;;; my-app-marp.el ends here
