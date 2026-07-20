;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Provides AI agent configuration using agent-shell and other.

;;; Code:
(use-package agent-shell
  :after evil
  :config
  ;; Permission-button dispatch: when a permission dialog is present in
  ;; the buffer, `y'/`n'/`!' must reach the button's text-property keymap
  ;; regardless of where point sits.  The text-prop keymap approach that
  ;; agent-shell uses is fragile under evil + shell-maker's asynchronous
  ;; point behavior (point can end up on the prompt instead of the
  ;; button when the dialog appears).  Route these keys through a
  ;; menu-item :filter so we only intercept when a permission button
  ;; actually exists; otherwise the binding falls through to
  ;; evil-yank / evil-search-next / evil-shell-command (normal) or
  ;; self-insert-command (insert) as before.
  (defun my/agent-shell-permission-button-present-p ()
    "Return non-nil if an unresolved permission button exists in this buffer."
    (save-excursion
      (goto-char (point-max))
      (agent-shell-previous-permission-button)))

  (defun my/agent-shell-permission-dispatch (char)
    "Jump to the latest permission button and invoke CHAR's action.
CHAR is a string like \"y\" / \"n\" / \"!\"."
    (save-excursion
      (agent-shell-jump-to-latest-permission-button-row)
      (when-let* ((km (get-char-property (point) 'keymap))
                  (action (lookup-key km char)))
        (call-interactively action))))

  (defun my/agent-shell-make-permission-filter (char)
    "Return a keymap definition that dispatches CHAR only when a button exists."
    `(menu-item ,(format "agent-shell-permission-%s" char)
                (lambda () (interactive) (my/agent-shell-permission-dispatch ,char))
                :filter (lambda (cmd)
                          (when (my/agent-shell-permission-button-present-p)
                            cmd))))

  (my/define-key
   (:map global-map
         :prefix "C-c"
         :key
         "x" #'agent-shell
         "h" #'agent-shell-toggle)
   (:map agent-shell-mode-map :state insert normal
         :key
         "C-RET" #'my/shell-maker-submit-and-normal
         "y" (my/agent-shell-make-permission-filter "y")
         "n" (my/agent-shell-make-permission-filter "n")
         "!" (my/agent-shell-make-permission-filter "!"))
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

  ;; agent-shell paints its header text with `font-lock-variable-name-face'
  ;; and friends on top of the `header-line' face background, which gensho-
  ;; theme intentionally keeps low-contrast (chrome tone: `mono6' fg on
  ;; `mono3' bg).  The font-lock foregrounds were designed for the *body*
  ;; background, not for chrome, so the resulting pair is hard to read.
  ;; Bumping the SVG text to `font-weight="bold"' recovers perceptual
  ;; contrast via weight, without overriding either theme's colors.  The
  ;; attribute lives on the `<text>' element and is inherited by every
  ;; `<tspan>' child, so one attribute per top/bottom/bindings row is
  ;; enough.  Idempotent: skips `<text>' tags that already carry a
  ;; font-weight (agent-shell's fallback icon glyphs, `agent-shell.el:3879').
  (defcustom my/agent-shell-header-bold t
    "When non-nil, render agent-shell's graphical header text in bold.
Toggle at runtime by customizing this variable and clearing
`agent-shell--header-cache' so cached SVGs are regenerated."
    :type 'boolean
    :group 'agent-shell)

  (defun my/agent-shell-header-boldize (result)
    "Inject font-weight=\"bold\" into RESULT's embedded SVG image data.
RESULT is what `agent-shell--make-header' returns: for the graphical
style it is a propertized string whose glyph carries the header SVG
as an Emacs image descriptor on the `display' text property, not as
raw XML in the buffer text (`svg-insert-image' calls `insert-image',
so the buffer only holds a placeholder character).  The XML lives in
the image descriptor's :data plist entry, so mutate it there.  Text
and none styles have no image and pass through untouched.  Idempotent
via a font-weight= presence check."
    (when (and my/agent-shell-header-bold
               (stringp result)
               (> (length result) 0))
      (let* ((pos (if (get-text-property 0 'display result)
                      0
                    (next-single-property-change 0 'display result)))
             (disp (and pos (get-text-property pos 'display result))))
        (when (and (consp disp) (eq (car disp) 'image))
          (let ((data (plist-get (cdr disp) :data)))
            (when (and (stringp data)
                       (string-match-p "<text " data)
                       (not (string-match-p "<text[^>]*font-weight=" data)))
              (let ((new-data (replace-regexp-in-string
                               "<text " "<text font-weight=\"bold\" "
                               data t t)))
                (plist-put (cdr disp) :data new-data)
                (image-flush disp)))))))
    result)

  (advice-add 'agent-shell--make-header :filter-return
              #'my/agent-shell-header-boldize)

  ;; Header SVGs are cached keyed on model fields (not on weight), so
  ;; drop the cache once at load time to force regeneration with the
  ;; new attribute.
  (when (boundp 'agent-shell--header-cache)
    (setq agent-shell--header-cache nil))

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

;; Org-babel backend: C-c C-c on #+begin_src agent-shell sends BODY to the
;; active agent-shell session, waits for turn-complete, then returns the
;; agent response so org-babel inserts it as #+RESULTS: under the block
;; (same shape as R / shell babel blocks).  Default header args from the
;; package are (:results . "output drawer") (:exports . "both").
(use-package ob-agent-shell
  :straight (:host github :repo "eddof13/ob-agent-shell")
  :after (agent-shell org)
  :custom
  ;; Agent turns routinely exceed the package default of 30s (tool calls,
  ;; permission prompts).  Raise the global wait; override per-block with
  ;; :timeout N when needed.
  (ob-agent-shell-timeout 120)
  :config
  ;; Append rather than replace so R (registered in my-app-org) stays loaded.
  ;; `org-babel-do-load-languages' both updates the variable and requires the
  ;; backend; bare `add-to-list' alone does not force a require.
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cons '(agent-shell . t)
         (assq-delete-all 'agent-shell org-babel-load-languages)))
  ;; agent-shell is not a programming major mode; map to text to avoid
  ;; "Org mode fontification error" on src blocks.
  (add-to-list 'org-src-lang-modes '("agent-shell" . text)))

(use-package agent-shell-org-transcript
  :straight (:host github :repo "lllShamanlll/agent-shell-org-transcript")
  :after (agent-shell org org-roam)
  :config
  (setq agent-shell-org-transcript-directory org-directory))

(use-package agent-shell-manager
  :straight (:host github :repo "jethrokuan/agent-shell-manager")
  :after (agent-shell evil)
  :custom
  ;; Route display through `display-buffer-alist' (configured below in
  ;; :config) instead of the package's fixed 30%-of-frame side window.
  (agent-shell-manager-side nil)
  :config
  ;; agent-shell-manager-mode is a read-only tabulated-list buffer.
  ;; Use evil `motion' state as the initial state: it preserves hjkl,
  ;; g-prefix, and search bindings while leaving operator keys
  ;; (`c'/`d'/`r'/`m'/`x' etc.) free for mode-specific commands.
  ;; The mode-map's single-letter keys otherwise lose to evil.
  (evil-set-initial-state 'agent-shell-manager-mode 'motion)

  (defcustom my/agent-shell-manager-max-height 10
    "Maximum body-line height for the *Agent-Shell Buffers* side window.
Body lines exclude header-line, tab-line, and mode-line."
    :type 'integer
    :group 'agent-shell-manager)
  (defcustom my/agent-shell-manager-min-height 2
    "Minimum body-line height for the *Agent-Shell Buffers* side window.
Body lines exclude header-line, tab-line, and mode-line.  Keeps
the window from collapsing when the agent list is empty."
    :type 'integer
    :group 'agent-shell-manager)

  (defun my/agent-shell-manager-fit (win)
    "Fit WIN so its body shows one line per agent plus one blank row.
Clamped to [`my/agent-shell-manager-min-height',
`my/agent-shell-manager-max-height'] body lines.  Header-line,
tab-line, and mode-line are added on top of the body target so
`fit-window-to-buffer' (which sizes the total window) produces the
intended body height."
    (let* ((buf (window-buffer win))
           (agent-lines (with-current-buffer buf
                          (count-lines (point-min) (point-max))))
           (decoration (with-current-buffer buf
                         (+ (if header-line-format 1 0)
                            (if mode-line-format 1 0)
                            (if tab-line-format 1 0))))
           (desired-body (max my/agent-shell-manager-min-height
                              (min my/agent-shell-manager-max-height
                                   (1+ agent-lines))))
           (desired-total (+ desired-body decoration)))
      (fit-window-to-buffer win desired-total desired-total)))

  (add-to-list
   'display-buffer-alist
   '("\\*Agent-Shell Buffers\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . my/agent-shell-manager-fit)
     (window-parameters . ((no-delete-other-windows . t)))))

  (defun my/agent-shell-manager-refit (&rest _)
    "Re-fit the manager window height when its content grows/shrinks.
The package refreshes the list every 2 seconds but does not
recompute the window height; without this advice a newly added
shell would either overflow or leave dead space."
    (when-let* ((buf (get-buffer "*Agent-Shell Buffers*"))
                (win (get-buffer-window buf)))
      (my/agent-shell-manager-fit win)))
  (advice-add 'agent-shell-manager-refresh :after
              #'my/agent-shell-manager-refit)

  (my/define-key
   (:map global-map :key "C-c s m" #'agent-shell-manager-toggle)
   (:map agent-shell-manager-mode-map
         :state motion
         :key
         "RET" #'agent-shell-manager-goto
         ;; g single is reserved as the user's g-prefix; use gr for
         ;; refresh (evil-collection convention; gr is user-blacklisted
         ;; from evil-collection so it is available).
         "gr"  #'agent-shell-manager-refresh
         "q"   #'quit-window
         ;; Relocate keys that collide with fundamental motion:
         ;;   k -> x (kill agent; k is up)
         ;;   l -> L (toggle logging; l is right)
         "x"   #'agent-shell-manager-kill
         "c"   #'agent-shell-manager-new
         "r"   #'agent-shell-manager-restart
         "d"   #'agent-shell-manager-delete-killed
         "m"   #'agent-shell-manager-set-mode
         "M"   #'agent-shell-manager-set-model
         "t"   #'agent-shell-manager-view-traffic
         "L"   #'agent-shell-manager-toggle-logging
         "C-c C-c" #'agent-shell-manager-interrupt)))

(use-package knockknock
  :straight (:host github :repo "konrad1977/knockknock"))

(use-package agent-shell-knockknock
  :straight (:host github :repo "xenodium/agent-shell-knockknock")
  :after (agent-shell knockkock)
  :config
  (agent-shell-knockknock-mode 1))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
