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
If a server is already running, report its URL.  Use `my/marp-server-buffer'
\\[my/marp-server-buffer] to inspect server output on demand."
  (interactive "P")
  (if (process-live-p (get-process my/marp--server-name))
      (message "Marp server already running — %s  (C-c m l to view log)" (my/marp--preview-url))
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
      (message "Marp server started for %s — open %s  (C-c m l to view log)"
               (abbreviate-file-name dir)
               (my/marp--preview-url)))))

(defun my/marp-server-buffer ()
  "Display the Marp live-preview server output buffer.
The server buffer is not shown automatically on startup (like `eglot-events-buffer').
Use this command to inspect server output on demand."
  (interactive)
  (if (get-buffer my/marp--server-buffer)
      (display-buffer my/marp--server-buffer)
    (message "No Marp server buffer (start with C-c m s)")))

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

;;; Follow mode: track the cursor in the live preview.
;; The bespoke preview encodes the current page as the URL hash `#N' (1-based)
;; and navigates on `hashchange' without reloading.  So following the cursor is
;; just: compute the slide at point, then set `location.hash'.  On xwidget-capable
;; builds (macOS/Linux) we drive an embedded WebKit preview directly; elsewhere
;; (e.g. native Windows, where xwidget is unavailable) `my/marp-goto-slide-in-browser'
;; offers a portable single-shot jump via the external browser.

(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-execute-script "xwidget")
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-live-p "xwidget")

;; Defined below by `define-minor-mode'; forward-declared so the helper
;; functions can reference the mode variable without a compiler warning.
(defvar my/marp-follow-mode)

(defun my/marp--current-slide ()
  "Return the 1-based slide number of the Marpit deck at point.
Count `---' page separators before point, skipping the leading YAML
frontmatter and any `---' inside fenced code blocks.  Point inside the
frontmatter returns 1.  Fences are tracked by toggling on ``` / ~~~ lines
so the result does not depend on font-lock/syntax-propertize state."
  (save-excursion
    (save-restriction
      (widen)
      (let ((target (line-beginning-position))
            (page 1)
            (in-code nil))
        (goto-char (point-min))
        ;; A `---' on the very first line opens YAML frontmatter; skip past
        ;; its closing `---' so neither delimiter counts as a page break.
        (when (looking-at-p "^---[ \t]*$")
          (forward-line 1)
          (while (and (not (eobp)) (not (looking-at-p "^---[ \t]*$")))
            (forward-line 1))
          (unless (eobp) (forward-line 1)))
        (while (< (point) target)
          (cond
           ((looking-at-p "^[ \t]*\\(```\\|~~~\\)")
            (setq in-code (not in-code)))
           ((and (not in-code) (looking-at-p "^---[ \t]*$"))
            (setq page (1+ page))))
          (forward-line 1))
        page))))

(defvar-local my/marp-follow--xwidget nil
  "The xwidget session showing this buffer's live preview.")

(defvar-local my/marp-follow--last-slide nil
  "Last slide number pushed to the preview, to skip redundant updates.")

(defun my/marp-follow--sync ()
  "Push the slide at point to the follow preview when it changes."
  (when (and my/marp-follow-mode
             (xwidget-live-p my/marp-follow--xwidget))
    (let ((slide (my/marp--current-slide)))
      (unless (eql slide my/marp-follow--last-slide)
        (setq my/marp-follow--last-slide slide)
        (xwidget-webkit-execute-script
         my/marp-follow--xwidget
         (format "location.hash = '%d';" slide))))))

(defun my/marp-follow--enable ()
  "Open the embedded preview and start following point."
  (unless (featurep 'xwidget-internal)
    (setq my/marp-follow-mode nil)
    (user-error "Follow mode needs an xwidget-capable Emacs; use C-c m g instead"))
  (unless buffer-file-name
    (setq my/marp-follow-mode nil)
    (user-error "Buffer is not visiting a file"))
  (require 'xwidget)
  ;; Ensure a server is running without also popping the external browser.
  (unless (process-live-p (get-process my/marp--server-name))
    (let ((my/marp-server-open-browser nil))
      (my/marp-start-server)))
  (let* ((src (current-buffer))
         (editor-win (selected-window))
         (url (format "%s/%s" (my/marp--preview-url)
                      (file-name-nondirectory buffer-file-name)))
         (sess nil))
    (xwidget-webkit-browse-url url)
    (setq sess (xwidget-webkit-current-session))
    ;; Restore the editor in its window and dock the preview on the right.
    (set-window-buffer editor-win src)
    (select-window editor-win)
    (display-buffer (xwidget-buffer sess)
                    '(display-buffer-in-side-window
                      (side . right) (window-width . 0.5)))
    (with-current-buffer src
      (setq my/marp-follow--xwidget sess
            my/marp-follow--last-slide nil)
      (add-hook 'post-command-hook #'my/marp-follow--sync nil t))
    (my/marp-follow--sync)
    (message "Marp follow on — preview tracks the cursor (C-c m f to stop)")))

(defun my/marp-follow--disable ()
  "Stop following and close the embedded preview."
  (remove-hook 'post-command-hook #'my/marp-follow--sync t)
  (when (and my/marp-follow--xwidget (xwidget-live-p my/marp-follow--xwidget))
    (let ((buf (xwidget-buffer my/marp-follow--xwidget)))
      (when (buffer-live-p buf)
        (kill-buffer buf))))
  (setq my/marp-follow--xwidget nil
        my/marp-follow--last-slide nil)
  (message "Marp follow off"))

(define-minor-mode my/marp-follow-mode
  "Track the cursor in a Marp live preview shown in an Emacs xwidget.
As point moves between slides, the embedded WebKit preview navigates to
the matching slide without reloading.  Requires an xwidget-capable build
(macOS/Linux); on other platforms use \\[my/marp-goto-slide-in-browser]."
  :lighter " MarpFollow"
  (if my/marp-follow-mode
      (my/marp-follow--enable)
    (my/marp-follow--disable)))

(defun my/marp-goto-slide-in-browser ()
  "Open the slide at point in the external browser.
A portable single-shot fallback for `my/marp-follow-mode' on builds
without xwidget support.  Requires a running server (\\[my/marp-start-server])."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (unless (process-live-p (get-process my/marp--server-name))
    (user-error "No Marp server running — start it first with C-c m s"))
  (browse-url (format "%s/%s#%d"
                      (my/marp--preview-url)
                      (file-name-nondirectory buffer-file-name)
                      (my/marp--current-slide))))

(my/define-key
 (:map markdown-mode-map
       :after markdown-mode
       :key
       "C-c m s" #'my/marp-start-server
       "C-c m k" #'my/marp-stop-server
       "C-c m l" #'my/marp-server-buffer
       "C-c m e" #'my/marp-export
       "C-c m f" #'my/marp-follow-mode
       "C-c m g" #'my/marp-goto-slide-in-browser))

(provide 'my-app-marp)
;;; my-app-marp.el ends here
