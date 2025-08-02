;;; my-completion-minibuffer.el --- Minibuffer completion setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides minibuffer completion configuration using Vertico, Orderless, and Marginalia.

;;; Code:

(use-package vertico
  :config
  (vertico-mode)
  (my/define-key
   (:map vertico-map
         :key
         "<escape>" #'vertico-exit
         "?" #'minibuffer-completion-help
         "M-RET" #'minibuffer-force-complete-and-exit
         "M-TAB" #'minibuffer-complete
         "C-h" #'backward-kill-word
         "C-j" #'vertico-next
         "C-k" #'vertico-previous
         "C-l" #'vertico-insert
         "C-f" #'vertico-scroll-up
         "C-b" #'vertico-scroll-down))

  ;; Use grid layout only for completing-read-multiple
  (require 'vertico-multiform)
  (setq vertico-multiform-commands
        '((completing-read-multiple
           grid (vertico-grid-rows 3)
           (vertico-grid-columns 3))))
  (vertico-multiform-mode 1))

(use-package emacs
  :straight nil
  :config
  ;; Use space/tab-separated input for completing-read-multiple
  (setq crm-separator "[ \t]+")

  ;; Display a CRM indicator in the minibuffer prompt
  (defun my/crm-indicator (args)
    (cons (format "[CRM] %s" (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package savehist
  :straight nil
  :config
  (savehist-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  (with-eval-after-load 'migemo
    (defun my/orderless-migemo-matcher (arg)
      (let ((pattern (migemo-get-pattern arg)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))

    (defun my/orderless-dot-dispatcher (pattern _index _total)
      "Treat '.' in PATTERN as '.*' (regex wildcard match)."
      (when (string-match-p "\\." pattern)
        `(orderless-regexp . ,(replace-regexp-in-string "\\." ".*" pattern))))

    (orderless-define-completion-style my/orderless-migemo-dot
      (orderless-matching-styles
       '(orderless-literal orderless-regexp my/orderless-migemo-matcher))
      (orderless-style-dispatchers '(my/orderless-dot-dispatcher)))

    (dolist (entry
             '((file             (styles my/orderless-migemo-dot))
               (multi-category   (styles my/orderless-migemo-dot))
               (imenu            (styles my/orderless-migemo-dot))
               (org-heading      (styles my/orderless-migemo-dot))
               (org-refile       (styles my/orderless-migemo-dot))
               (org-roam-node    (styles my/orderless-migemo-dot))
               (consult-location (styles my/orderless-migemo-dot))
               (nil              (styles my/orderless-migemo-dot))))
      (setq completion-category-overrides
            (assq-delete-all (car entry) completion-category-overrides))
      (add-to-list 'completion-category-overrides entry))))

(use-package consult
  :after evil vertico
  :config
  (my/define-key
   (:map global-map
         :key
         "C-s" #'consult-line)
   (:map evil-motion-state-map
         :key
         "gs" #'consult-buffer
         "g[" #'my/consult-org-headings-all
         "g]" #'consult-imenu
         "g/" #'consult-ripgrep)
   (:map org-mode-map
         :state motion
         :key
         "g]" #'consult-org-heading))

  (consult-customize consult-buffer :preview-key '(:debounce 0.5 any))

  (defun my/sort-recentf-by-directory ()
    "Return recentf-list sorted by directory and filename."
    (sort (copy-sequence recentf-list)
          (lambda (a b)
            (let ((dir-a (file-name-directory a))
                  (dir-b (file-name-directory b)))
              (if (string= dir-a dir-b)
                  (string> a b)
                (string> dir-a dir-b))))))
  
  (setq consult-buffer-sources
        (mapcar
         (lambda (src)
           (let* ((resolved (if (symbolp src) (symbol-value src) src))
                  (copied (copy-sequence resolved)))
             (if (eq resolved consult--source-recent-file)
                 (plist-put copied :items #'my/sort-recentf-by-directory)
               src)))
         consult-buffer-sources))

  (defun my/sanitize-ascii-japanese (s)
    "Remove unprintable characters, keeping ASCII, Latin, Japanese kana/kanji and symbols."
    (replace-regexp-in-string
     "[^\u0020-\u007E\u00A0-\u00FF\u3000-\u30FF\u4E00-\u9FFF\uFF00-\uFFEF]" "" s))
  
  (defun my/find-file-from-minibuffer ()
    "Use current consult-buffer candidate as input to `find-file` after exiting minibuffer."
    (interactive)
    (let* ((raw (or (and (fboundp 'vertico--candidate)
                         (vertico--candidate))
                    (minibuffer-contents)))
           (name (my/sanitize-ascii-japanese (substring-no-properties raw)))
           (buf  (get-buffer name))
           (file (or (and buf (buffer-file-name buf))
                     (and buf (with-current-buffer buf
                                (derived-mode-p 'dired-mode buf)
                                (buffer-local-value 'default-directory buf)))
                     (and (file-exists-p name) name))))
      (when file
        (let* ((dir (file-name-directory file))
               (initial (unless (file-directory-p file)
                          (file-name-nondirectory file)))
               (callback (lambda ()
                           (let ((default-directory dir)
                                 (use-dialog-box nil))
                             (find-file
                              (read-file-name "Find file: " dir nil nil initial))))))
          (run-at-time 0 nil callback)
          (abort-minibuffers)))))

  (defvar my/consult-buffer-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-l") #'my/find-file-from-minibuffer)
      map)
    "Keymap active only during `consult-buffer` minibuffer.")
  
  (define-minor-mode my/consult-buffer-mode
    "Minor mode for `consult-buffer` specific bindings in minibuffer."
    :init-value nil
    :lighter ""
    :keymap my/consult-buffer-mode-map)
  
  (defun my/enable-consult-buffer-mode ()
    (when (eq this-command #'consult-buffer)
      (my/consult-buffer-mode 1)))
  
  (add-hook 'minibuffer-setup-hook #'my/enable-consult-buffer-mode)

  (defun my/toggle-xref-show-destination (&optional arg)
    "Toggle between `consult' and default xref UI for displaying results."
    (interactive)
    (let* ((use-consult (or arg (not (eq xref-show-definitions-function #'consult-xref))))
           (show-fn     (if use-consult #'consult-xref #'xref--show-defs-buffer))
           (xrefs-fn    (if use-consult #'consult-xref #'xref--show-xrefs-buffer)))
      (setq xref-show-definitions-function show-fn
            xref-show-xrefs-function xrefs-fn)
      (message "xref display is now: %s"
               (if use-consult "consult minibuffer" "default buffer"))))

  ;; Default to consult-based xref display
  (my/toggle-xref-show-destination 1)

  (defun my/consult-org-headings-all (&optional archivep)
    "Consult all headings under `org-directory`.
With-current-buffer prefix argument INCLUDE-ARCHIVE (C-u), also include .org_archive files."
  (interactive "P")
  (unless (and org-directory (file-directory-p org-directory))
    (user-error "Please set a valid `org-directory`"))

  (let* ((ext (if archivep "\\.org\\(_archive\\)?$" "\\.org$"))
         (files (directory-files-recursively org-directory ext)))
    (consult-org-heading nil files))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark-consult)

(use-package embark
  :config
  (my/define-key (:map minibuffer-local-map :key "C-." #'embark-act))
  (setq embark-prompter #'embark-completing-read-prompter
        embark-indicators '(embark-minimal-indicator)))

(provide 'my-completion-minibuffer)
;;; my-completion-minibuffer.el ends here
