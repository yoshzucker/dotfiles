;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Two ways to reach an agent from Emacs, which do different things.
;; agent-shell talks to several providers in a buffer of its own, and is
;; reached by name.  claude-code-ide talks only to Claude Code, over the
;; protocol its VS Code and JetBrains extensions speak, and is reached by a
;; key because it is the one used daily.

;;; Code:
(use-package agent-shell
  ;; Loaded on first use, by name: no key of its own, since this is the
  ;; second way in and `M-x' is enough for it.  Everything in `:config' below
  ;; adjusts agent-shell itself -- its keymap, its header rendering, its UI
  ;; padding -- and none of it means anything until agent-shell exists.  Two
  ;; things do have to hold for the whole session, so they live in `:init':
  ;; the interpreters it shells out to, and its entry in the project
  ;; switcher, which would otherwise appear only after the first visit.
  :defer t
  :commands (agent-shell agent-shell-toggle)
  :init
  (when (eq system-type 'windows-nt)
    (dolist (path (list (expand-file-name "~/scoop/apps/nodejs/current")
                        (expand-file-name "~/scoop/apps/nodejs/current/bin")
                        (expand-file-name "~/scoop/apps/msys2/current/usr/bin")))
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ";" (getenv "PATH")))))

  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(my/project-agent-shell "Agent shell" "a")
                 t))

  (defun my/project-agent-shell ()
    "Start agent-shell from the root of the current project."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (call-interactively #'agent-shell)))

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

;;;; Claude Code as an editor integration

;; The same CLI that runs in a terminal, connected to Emacs over the protocol
;; its VS Code and JetBrains extensions speak.  What that buys over running
;; `claude' in a terminal is the direction of the arrow: Claude can ask Emacs
;; things.  The file being looked at and the region marked arrive without
;; being pasted, diffs come back through ediff to be edited before they are
;; applied, and `claude-code-ide-emacs-tools-setup' hands over xref, imenu,
;; tree-sitter, the project's shape and its diagnostics as tools to call.
;;
;; What that is worth depends on the language.  Emacs answers from what it
;; has already parsed -- imenu's outline, tree-sitter's tree, project.el's
;; shape, the obarray behind elisp's apropos -- and those cost no process
;; at all, which is a fifth of a second saved on the Windows machine every
;; time one is not started.
;;
;; References are the exception worth knowing.  With eglot attached the
;; language server answers them, but Emacs Lisp has no backend for
;; references, so `xref-find-references' falls to a default that runs find
;; and grep over the project and its external roots.  Asked who calls an
;; elisp function, ripgrep is the better tool and Claude is right to reach
;; for it.
;;
;; This sits beside agent-shell rather than replacing it.  agent-shell speaks
;; to several providers and lives in a buffer of its own; this speaks only to
;; Claude Code and lives in the project.

;; claude-code-ide's MCP tools server is an HTTP server, and the library it
;; runs on is `web-server' -- eschulte's.  `simple-httpd' is skeeto's, and
;; both are published from a repository called emacs-web-server.  straight
;; keys its clones by that name, so the second one asked for finds the first
;; one's checkout: org-roam-ui pulls in simple-httpd, and `web-server' then
;; resolved to a directory holding simple-httpd.el and no web-server.el.
;;
;; The failure is quiet.  claude-code-ide requires the library inside a
;; `condition-case' that reports through its debug log, which is off, and
;; the tools server then declines to start -- so Claude is handed the
;; editor connection but none of the Emacs tools, and answers questions
;; about the code by shelling out to ripgrep instead of asking xref.
;;
;; Named here, before the module that brings in org-roam-ui, so the clone
;; this one needs is its own.
(use-package web-server
  :straight (web-server :type git :host github :repo "eschulte/emacs-web-server"
                        :local-repo "emacs-web-server-eschulte")
  :defer t)


;; The package drives a CLI it does not install: `claude', from scoop's
;; `claude-code' on Windows and from the Claude Code installer on the Mac.
;; scoop's `claude' is a different thing -- the desktop application -- and
;; both are in the scoopfile because both are wanted, not because either
;; stands in for the other.
;;
;; Worth naming because of how its absence reads.  `claude-code-ide' looks
;; for the CLI with `executable-find', which answers for Emacs; what runs it
;; is the shell the terminal backend starts, which has a PATH of its own.
;; Where those two differ the session starts, says so, and the window shuts
;; a moment later -- see the MSYS2_PATH_TYPE note in my-app-terminal.el.
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  ;; Reached through the transient, which is the entry point the package
  ;; intends: every other command is on it.
  :defer t
  :commands (claude-code-ide claude-code-ide-menu)
  :init
  ;; One key, because the transient carries the rest: showing and hiding the
  ;; project's windows, switching to the buffer, sending the region.  Reading
  ;; a Claude buffer needs no key of its own either -- `consult-buffer' finds
  ;; it like any other.
  (my/define-key
   (:map global-map :key "C-c x" #'claude-code-ide-menu))
  :custom
  ;; ghostel rather than the default vterm.  vterm needs a native module that
  ;; does not build on Windows at all, and ghostel is the backend the package
  ;; itself recommends for rendering the TUI.
  (claude-code-ide-terminal-backend 'ghostel)

  ;; As wide as the text is, rather than the default hundred, which is more
  ;; than a frame of this configuration is wide to begin with.  The window
  ;; is docked to the right, so the frame takes its two-column shape to make
  ;; room -- see `my/frame--adjust-for-docking' in my-ui-frame.el -- and
  ;; this number is then how that width is divided: one column of text each.
  (claude-code-ide-window-width 81)

  ;; The tools reach Claude and are not reached for: asked what calls a
  ;; function, it runs ripgrep, which is what it would do without an editor
  ;; attached.  So the preference is stated once, here, rather than in every
  ;; project's CLAUDE.md -- the package appends this to the system prompt of
  ;; every session it starts.
  ;;
  ;; No `;', `&' or `|' in this string.  The value is shell-quoted into the
  ;; command line that starts the CLI, and the backend then splits that line
  ;; back apart with `split-string-shell-command' -- which honours those
  ;; three as command separators even after `shell-quote-argument' has
  ;; escaped them, and returns only the words after the last one.  The
  ;; program name is among the words thrown away, so what fails is the exec,
  ;; reporting the tail of this sentence as a program it cannot find.
  (claude-code-ide-system-prompt
   (concat "Emacs is attached as an editor and answers some questions from "
           "what it has already parsed, without starting a process: "
           "claude-code-ide-mcp-imenu-list-symbols for a file's outline, "
           "claude-code-ide-mcp-treesit-info for syntax structure, "
           "claude-code-ide-mcp-project-info for the shape of the project, "
           "and claude-code-ide-mcp-xref-find-apropos to find a symbol by "
           "part of its name. Prefer these over reading whole files or "
           "searching text for those questions. "
           "claude-code-ide-mcp-xref-find-references is worth preferring "
           "where a language server is attached, since the server answers "
           "it. Emacs Lisp has no such backend and it greps like any other "
           "search, so grep is fine there."))
  :config
  ;; The CLI is exec'd directly rather than through a shell, so on Windows it
  ;; is the native `claude' and both sides speak the same path form.  Nothing
  ;; to arrange for that; it is only worth knowing when a path looks wrong.
  (claude-code-ide-emacs-tools-setup))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
