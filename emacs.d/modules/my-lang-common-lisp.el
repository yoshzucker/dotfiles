;;; my-lang-common-lisp.el --- Common Lisp with SLY and SBCL -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Common Lisp using SLY and SBCL, on the Mac only.
;;
;; Place the Common Lisp Hyperspec in `~/.local/share/HyperSpec/` for local browsing.

;;; Code:

(use-package sly
  ;; Not on Windows, which has no SBCL.  While it runs, SLY's slynk listens on
  ;; a local port with no token and evaluates whatever Lisp it is sent, so it
  ;; is not loaded on the machine that is not meant to run it.
  :if (eq system-type 'darwin)
  ;; Loaded by `M-x sly'.  Eager, it pulled in `hyperspec' and `browse-url' and
  ;; `arc-mode' on every startup, for a REPL that is started deliberately or not
  ;; at all.
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (my/define-key
   (:map sly-mrepl-mode-map
         :state insert
         :key
         "C-p" #'sly-mrepl-previous-input-or-button
         "C-n" #'sly-mrepl-next-input-or-button))

  (defcustom my/hyperspec-install-dir
    (expand-file-name "HyperSpec/" "~/.local/share/")
    "Directory where the Common Lisp HyperSpec should be installed (XDG-style)."
    :type 'directory
    :group 'common-lisp)

  (defcustom my/hyperspec-download-url
    "https://www.lispworks.com/documentation/HyperSpec/HyperSpec-7-0.tar.gz"
    "URL to download the Common Lisp HyperSpec tarball."
    :type 'string
    :group 'common-lisp)

  (setq common-lisp-hyperspec-root
        (concat "file://" (expand-file-name my/hyperspec-install-dir)))

  (defun my/setup-hyperspec ()
    "Download and install the Common Lisp HyperSpec locally.
Sets `common-lisp-hyperspec-root` to the local path."
    (interactive)
    (let* ((target-dir (file-name-as-directory (expand-file-name my/hyperspec-install-dir)))
           (tar-file (expand-file-name "HyperSpec.tar.gz" temporary-file-directory)))
      (unless (file-directory-p target-dir)
        (make-directory target-dir t))
      (message "Downloading HyperSpec from: %s" my/hyperspec-download-url)
      (url-copy-file my/hyperspec-download-url tar-file t)
      (message "Extracting HyperSpec to: %s" target-dir)
      (let ((default-directory target-dir))
        (call-process "tar" nil "*hyperspec-install*" t "-xzf" tar-file "--strip-components=1"))
      (setq common-lisp-hyperspec-root (concat "file://" target-dir))
      (message "HyperSpec is ready at: %s" common-lisp-hyperspec-root)))

  (setq browse-url-handlers
        '(("file:///.*HyperSpec/" . eww-browse-url))))

(provide 'my-lang-common-lisp)
;;; my-lang-common-lisp.el ends here
