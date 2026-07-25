;;; my-lang-misc.el --- Minor languages and filetype modes -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures support for small or auxiliary file formats
;; such as Dockerfile, YAML, etc., that do not warrant a dedicated file.

;;; Code:

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :config
  (setq dockerfile-indent-offset 2))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (setq yaml-indent-offset 2))

(defun my/markdown-table-align-all ()
  "Align every GFM table in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (if (markdown-table-at-point-p)
            (progn
              (markdown-table-align)
              (setq count (1+ count))
              ;; `markdown-table-align' restores the cell position, so point
              ;; stays inside the table; jump past its (re-formatted) end
              ;; before scanning for the next one.
              (goto-char (markdown-table-end))
              (forward-line 1))
          (forward-line 1)))
      (message "Aligned %d table%s" count (if (= count 1) "" "s")))))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown"))

(my/define-key
 (:map markdown-mode-map
       :after markdown-mode
       :key
       "C-c C-c a" #'markdown-table-align          ; align table at point
       "C-c C-c A" #'my/markdown-table-align-all))  ; align all tables in buffer

(provide 'my-lang-misc)
;;; my-lang-misc.el ends here
