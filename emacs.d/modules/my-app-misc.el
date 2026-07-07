;;; my-app-misc.el --- Miscellaneous App integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; This file handles integrations with external applications.

;;; Code:
(use-package symon
  :config
  (symon-mode))

(defcustom my/convert-pptx-program (expand-file-name "~/.local/bin/convert-pptx-to-pdf.ps1")
  "Path to the PowerShell script for pptx to pdf conversion using PowerPoint COM."
  :type 'string
  :group 'my)

(defun my/convert-pptx-to-pdf ()
  "Convert selected pptx to PDF using PowerPoint COM. Result shown in echo area only."
  (interactive)
  (let* ((coding-system-for-read 'cp932)
         (coding-system-for-write 'cp932)
         (default (when (derived-mode-p 'dired-mode)
                    (dired-get-file-for-visit)))
         (initial (when (and default (string-suffix-p ".pptx" default t))
                    (file-name-nondirectory default)))
         (file (file-truename
                (expand-file-name
                 (read-file-name "Select PPTX file: " nil default t initial
                                 (lambda (f)
                                   (or (file-directory-p f)
                                       (string-suffix-p ".pptx" f t)))))))
         (ps1-path (expand-file-name my/convert-pptx-program))
         (cmd (format "powershell -NoProfile -ExecutionPolicy Bypass -File %s %s"
                      (shell-quote-argument ps1-path)
                      (shell-quote-argument (expand-file-name file))))
         (output (string-trim (shell-command-to-string cmd))))
    
    (if (string-prefix-p "ERROR:" output)
        (message "Conversion failed: %s" (substring output 7))
      (message "Conversion complete! PDF saved as: %s" output))))

(provide 'my-app-misc)
;;; my-app-misc.el ends here
