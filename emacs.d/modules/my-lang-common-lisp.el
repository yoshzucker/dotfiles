;;; my-lang-common-lisp.el --- Common Lisp development setup with SLIME -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Common Lisp using SLY and SBCL.
;;
;; Place the Common Lisp Hyperspec in `~/.local/share/HyperSpec/` for local browsing.

;;; Code:

(use-package sly
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

  (setq browse-url-browser-function
        '(("file:///.*HyperSpec/" . eww-browse-url)
          ("." . browse-url-default-browser)))

  (defun my/sly-patch-slynk ()
    "Patch `slynk.lisp` to replace any `~<newline>` sequences with `~%`.

This works around Windows newline issues affecting format strings.
The function is idempotent: it only rewrites the file when actual
changes are detected."
    (let ((file (locate-library "slynk/slynk.lisp")))
      (when (and file (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((modified nil))
            (while (re-search-forward "~\r?\n" nil t)
              (setq modified t)
              (replace-match "~%"))
            (when modified
              (write-region (point-min) (point-max) file)
              (message "Patched %s to normalize format string newlines." file)))))))
  
  (when (eq system-type 'windows-nt)
    (advice-add 'sly :before #'my/sly-patch-slynk)))

(use-package lisp-extra-font-lock
  :config
  (lisp-extra-font-lock-global-mode 1))

(provide 'my-lang-common-lisp)
;;; my-lang-common-lisp.el ends here
