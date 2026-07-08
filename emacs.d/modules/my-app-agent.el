;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides AI agent configuration using agent-shell and other.

;;; Code:
(use-package agent-shell
  :after evil
  :config
  (my/define-key
   (:map global-map
         :key
         "C-c s n" #'agent-shell
         "C-c s t" #'agent-shell-toggle)
   (:map agent-shell-mode-map :state insert normal
         :key "C-RET" #'my/shell-maker-submit-and-normal)
   (:map agent-shell-mode-map :state normal
         :key "q" #'quit-window)
   (:map agent-shell-mode-map
         :key
         "<backtab>" #'my/agent-shell-cycle-session-mode
         "C-c C-q" #'my/agent-shell-sayoonara))
  
  (when (eq system-type 'windows-nt)
    (dolist (path (list (expand-file-name "~/scoop/apps/nodejs/current")
                        (expand-file-name "~/scoop/apps/nodejs/current/bin")
                        (expand-file-name "~/scoop/apps/msys2/current/usr/bin")))
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ";" (getenv "PATH"))))
    
    (add-to-list 'process-coding-system-alist
                 '("claude-agent-acp" utf-8-dos . cp932-dos)))
  
  (let ((packages
         (append
          (cond ((eq system-type 'darwin)
                 '(("npm" . "brew install nodejs")))
                ((eq system-type 'windows-nt)
                 `(("npm"   . "scoop install nodejs")
                   ("msys2" . "scoop install msys2")
                   ("diff"  . ,(concat (getenv "USERPROFILE")
                                       "\\scoop\\apps\\msys2\\current\\usr\\bin\\bash.exe"
                                       " -lc \"pacman -S --noconfirm diffutils\""))))
                (t nil))
          '(("claude-agent-acp" .
             "npm install -g @agentclientprotocol/claude-agent-acp --ignore-scripts")))))
    (dolist (package packages)
      (my/ensure-system-package (car package) (cdr package))))
  
  (setq agent-shell-prefer-session-resume nil)

  (defun my/shell-maker-submit-and-normal (&rest args)
    "Submit input to shell-maker and return to `evil-normal-state'."
    (interactive)
    (apply #'shell-maker-submit args)
    (evil-normal-state))

  (defun my/agent-shell-find-file (&optional pick-shell)
    "Send any file (including outside the project) to agent-shell.
In agent-shell buffer: starts read-file-name from the shell's current directory.
Outside: pre-fills with the file at point in dired, or buffer-file-name for normal buffers."
    (interactive "P")
    (let* ((in-shell (derived-mode-p 'agent-shell-mode))
           (shell-buffer (when pick-shell
                           (completing-read
                            "Send to shell: "
                            (mapcar #'buffer-name (agent-shell-buffers))
                            nil t)))
           (dir (if in-shell
                    (with-current-buffer (or shell-buffer (current-buffer))
                      default-directory)
                  default-directory))
           (initial (unless in-shell
                      (cond ((derived-mode-p 'dired-mode)
                             (ignore-errors (dired-get-filename nil t)))
                            (t (buffer-file-name))))))
      (let* ((file (read-file-name "Send file: " dir initial t (when initial (file-name-nondirectory initial))))
             (files (list (expand-file-name file dir))))
        (agent-shell-insert :text (agent-shell--get-files-context :files files)
                            :shell-buffer shell-buffer))))

  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(my/project-agent-shell "Agent shell" "a")
                 t))

  (defun my/project-agent-shell ()
    "Start agent-shell from the root of the current project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (call-interactively #'agent-shell)))

  (defun my/agent-shell-cycle-session-mode (&optional on-success)
    "Cycle session modes, skipping any that the backend refuses.
Some modes reported as available by claude-agent-acp (e.g. `auto',
`bypassPermissions') can still be rejected by the Anthropic backend
depending on the account plan or managed policy.  On failure, advance
to the next mode instead of aborting."
    (declare (modes agent-shell-mode))
    (interactive)
    (unless (derived-mode-p 'agent-shell-mode)
      (user-error "Not in an agent-shell buffer"))
    (unless (map-nested-elt (agent-shell--state) '(:session :id))
      (user-error "No active session"))
    (let* ((mode-ids (mapcar (lambda (m) (map-elt m :id))
                             (agent-shell--get-available-modes
                              (agent-shell--state))))
           (start (or (seq-position
                       mode-ids
                       (agent-shell--current-mode-id (agent-shell--state))
                       #'string=)
                      -1))
           (buffer (current-buffer)))
      (unless mode-ids
        (user-error "No session modes available"))
      (cl-labels
          ((try (step)
             (when (>= step (length mode-ids))
               (user-error "No selectable session mode available"))
             (let ((next (nth (mod (+ start 1 step) (length mode-ids))
                              mode-ids)))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (agent-shell--config-option-set-mode-id
                    :mode-id next
                    :on-success on-success
                    :on-failure
                    (lambda (acp-error _raw)
                      (message "Skipping %s: %s" next acp-error)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (try (1+ step)))))))))))
        (try 0))))

  (defun my/agent-shell-sayoonara ()
    "Quit the current agent-shell session and kill its buffer."
    (declare (modes agent-shell-mode))
    (interactive)
    (unless (derived-mode-p 'agent-shell-mode)
      (error "Not in an agent-shell buffer"))
    (message "Quit agent and close buffer.")
    (kill-buffer (current-buffer)))

  ;; agent-shell inserts PNG/SVG icons at :height (frame-char-height), but
  ;; frame-char-height is the full line-cell height sized for CJK glyphs and is
  ;; noticeably larger than the ASCII cap-height.  That mismatch is what makes
  ;; the icons look inflated next to plain ASCII text.  Shrink them to roughly
  ;; the ASCII cap-height, which is around 60% of the full line-cell height.
  (defun my/agent-shell-icon-height ()
    "Return a pixel height matching the ASCII glyph size of the default face."
    (round (* 0.6 (frame-char-height))))

  (advice-add
   'agent-shell--config-icon :around
   (lambda (orig &rest args)
     ;; Compute the shrunk height *before* rebinding `frame-char-height':
     ;; `my/agent-shell-icon-height' itself calls `frame-char-height', so
     ;; letting the rebound function call it would recurse infinitely.
     (let ((height (my/agent-shell-icon-height)))
       (cl-letf (((symbol-function 'frame-char-height)
                  (lambda (&rest _) height)))
         (apply orig args)))))

  ;; agent-shell's header SVG receives the default face's device-pixel font
  ;; size (via `font-get :size') and device-pixel line height (via
  ;; `frame-char-height').  SVG bare numeric values are interpreted as CSS
  ;; pixels (96-DPI reference), so on displays where the device DPI diverges
  ;; from 96 (Windows display scaling > 100%, fractional Wayland scaling) the
  ;; SVG text and icon render visibly larger than the surrounding buffer
  ;; text.  Rewrite the header model to express both dimensions in CSS pixels
  ;; derived from the face's point size, which is device-independent across
  ;; Windows, macOS, and Linux.  The icon square scales together with the
  ;; text because it is sized as `3 * :font-height'.
  (defun my/agent-shell-header-model-normalize (model)
    "Return MODEL with `:font-size' and `:font-height' in CSS pixels.
Font size is the face's point size converted at 96 DPI.  Line height
preserves the font's designed height:size ratio from the incoming
model when both values are numeric, else falls back to 1.2."
    (let* ((face-height (face-attribute 'default :height))
           (pt-size (/ face-height 10.0))
           (font-size (max 1 (round (* pt-size (/ 96.0 72)))))
           (in-size (map-elt model :font-size))
           (in-height (map-elt model :font-height))
           (ratio (if (and (numberp in-size) (> in-size 0)
                           (numberp in-height))
                      (/ (float in-height) (float in-size))
                    1.2))
           (font-height (max 1 (round (* font-size ratio)))))
      (mapcar (lambda (pair)
                (pcase (car pair)
                  (:font-size (cons :font-size font-size))
                  (:font-height (cons :font-height font-height))
                  (_ pair)))
              model)))

  (advice-add 'agent-shell--make-header-model :filter-return
              #'my/agent-shell-header-model-normalize)

  ;; agent-shell-ui pads every fragment block with a trailing "\n\n" and
  ;; enforces 2 trailing newlines before the next block via
  ;; `agent-shell-ui--required-newlines'.  That's what puts one blank line
  ;; between blocks.  When the block is collapsed (label-only) the blank
  ;; line is noise; when it's expanded the blank line lets the body
  ;; breathe from the next label.  Squeeze the padding to a single
  ;; newline only for collapsed insertions.
  (defun my/agent-shell-ui-tighten-collapsed-block (orig model &rest args)
    "Around-advice: for collapsed fragments, shrink block padding to \\n."
    (if (plist-get args :expanded)
        (apply orig model args)
      (let ((orig-iro (symbol-function 'agent-shell-ui--insert-read-only))
            (orig-req (symbol-function 'agent-shell-ui--required-newlines)))
        (cl-letf (((symbol-function 'agent-shell-ui--insert-read-only)
                   (lambda (s)
                     (funcall orig-iro
                              (if (and (stringp s)
                                       (string-match-p "\\`\n+\\'" s))
                                  "\n" s))))
                  ((symbol-function 'agent-shell-ui--required-newlines)
                   (lambda (desired) (funcall orig-req (min 1 desired)))))
          (apply orig model args)))))

  (advice-add 'agent-shell-ui-update-fragment :around
              #'my/agent-shell-ui-tighten-collapsed-block))

(use-package ob-agent-shell
  :straight (:host github :repo "eddof13/ob-agent-shell")
  :after (agent-shell org)
  :config
  (add-to-list 'org-babel-load-languages '(agent-shell . t))
  (add-to-list 'org-src-lang-modes '("agent-shell" . text)))  ;; Prevent font-lock errors

(use-package agent-shell-org-transcript
  :straight (:host github :repo "lllShamanlll/agent-shell-org-transcript")
  :after (agent-shell org org-roam)
  :config
  (setq agent-shell-org-transcript-directory my/org-complexbrain-directory))

(use-package agent-shell-manager
  :straight (:host github :repo "jethrokuan/agent-shell-manager")
  :after agent-shell
  :config
  (my/define-key (:map global-map :key "C-c s m" #'agent-shell-manager-toggle)))

(use-package knockknock
  :straight (:host github :repo "konrad1977/knockknock"))

(use-package agent-shell-knockknock
  :straight (:host github :repo "xenodium/agent-shell-knockknock")
  :after (agent-shell knockkock)
  :config
  (agent-shell-knockknock-mode 1))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
