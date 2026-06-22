;;; my-app-marp.el --- Marp slide helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; Drive the Marp CLI from markdown buffers: a live-preview server (start/stop)
;; and one-shot exports.  All commands pass the global config (local-file
;; embedding, raw HTML) plus the custom theme.  YAML config is used because the
;; Scoop standalone binary cannot load JS/ESM config; the theme is passed as a
;; flag so it works without relying on config-relative path resolution.
;;; Code:

(defgroup my-marp nil
  "Marp slide preview and export helpers."
  :group 'tools)

(defcustom my/marp-server-open-browser t
  "Non-nil means automatically open the preview URL in the default browser when server starts."
  :type 'boolean
  :group 'my-marp)

(defvar my/marp-config-dir
  (expand-file-name "marp"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name ".config" "~")))
  "Directory holding the global Marp config and themes.")

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
  "Flags applying the global Marp config + custom theme, when present."
  (let ((cfg (expand-file-name "marp.config.yml" my/marp-config-dir))
        (theme (expand-file-name "themes/custom.css" my/marp-config-dir))
        (args nil))
    (when (file-exists-p theme)
      (setq args (append (list "--theme" theme) args)))
    (when (file-exists-p cfg)
      (setq args (append (list "--config-file" cfg) args)))
    args))

(defun my/marp-start-server ()
  "Start a Marp live-preview server for the current file's directory.
If a server is already running, just surface its output buffer."
  (interactive)
  (if (process-live-p (get-process my/marp--server-name))
      (progn
        (display-buffer my/marp--server-buffer)
        (message "Marp server already running — %s" (my/marp--preview-url)))
    (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
           (default-directory dir)
           (buf (get-buffer-create my/marp--server-buffer))
           (args (append (my/marp--config-args) (list "--server" ".")))
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

(defun my/marp-export (format)
  "Export the current buffer's file to FORMAT (pdf, pptx, or html)."
  (interactive
   (list (completing-read "Marp export: " '("pdf" "pptx" "html") nil t "pdf")))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((buf (get-buffer-create "*marp-export*")))
    (with-current-buffer buf
      (erase-buffer))
    (apply #'start-process "marp-export" buf "marp"
           (append (my/marp--config-args)
                   (list (concat "--" format) "--" buffer-file-name)))
    (display-buffer buf)
    (message "Marp exporting %s → %s…"
             (file-name-nondirectory buffer-file-name) format)))

(my/define-key
 (:map markdown-mode-map
       :after markdown-mode
       :key
       "C-c m s" #'my/marp-start-server
       "C-c m k" #'my/marp-stop-server
       "C-c m e" #'my/marp-export))

(provide 'my-app-marp)
;;; my-app-marp.el ends here
