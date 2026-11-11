;;; my-app-agent.el --- AI agent setup -*- lexical-binding: t -*-
;;; Commentary:
;; Claude Code from Emacs: in a window of its own, by claude-code-ide, over
;; the protocol its VS Code and JetBrains extensions speak -- for org-dock's
;; copies as for anything else.

;;; Code:
;;;; Claude Code as an editor integration

;; The same CLI that runs in a terminal, connected to Emacs over the protocol
;; its VS Code and JetBrains extensions speak.  What that buys over running
;; `claude' in a terminal: the file being looked at and the region marked
;; arrive without being pasted, and diffs come back through ediff to be
;; edited before they are applied.
;;
;; That connection only.  The package can also start an MCP server of its
;; own, over HTTP, handing Claude xref, imenu and the project's shape as
;; tools; it is off by default and left off.  With no language server here
;; those answer little that grep and reading the file do not, and they have
;; Emacs open whatever file Claude names -- one more server listening, and
;; one more way to have Emacs visit a file nobody chose.

;; The package drives a CLI it does not install: `claude', from the Claude
;; Code installer on Windows and from Homebrew's cask on the Mac.
;;
;; Its absence reads plainly: the package runs `claude --version' through
;; `exec-path' before anything else, and refuses to start without it.  A
;; session that does start, says so, and shuts its window a moment later is
;; a CLI that was found and then rejected its own command line -- see the
;; Windows note at the end of `:config'.
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

  ;; No executeCode.  Unless told otherwise the package offers Claude a tool
  ;; that evaluates whatever Elisp it is handed, in this Emacs -- outside
  ;; Claude Code's permission rules and its sandbox, with everything Emacs
  ;; can reach.  A Claude that has read instructions planted in a page or an
  ;; invitation would have that to hand.  Nothing here wants it: Claude
  ;; writes files with Edit and Write and runs the project's tools through
  ;; the shell, where the permission rules see it.
  ;;
  ;; In `:init', so it is already nil when the package first reads it,
  ;; which it does while loading.
  (setq claude-code-ide-enable-execute-code nil)
  :config
  ;; ghostel rather than the default vterm.  vterm needs a native module that
  ;; does not build on Windows at all, and ghostel is the backend the package
  ;; itself recommends for rendering the TUI.
  (setq claude-code-ide-terminal-backend 'ghostel)

  ;; ESC always reaches Claude, as its interrupt key, rather than entering
  ;; normal state; C-[ does that instead -- see evil-ghostel in
  ;; my-app-terminal.el -- and `C-c <escape>' where C-[ cannot be told
  ;; from ESC, in a terminal.
  (setq claude-code-ide-ghostel-evil-escape 'terminal)

  ;; As wide as the text is, rather than the default hundred, which is more
  ;; than a frame of this configuration is wide to begin with.  The window
  ;; is docked to the right, so the frame takes its two-column shape to make
  ;; room -- see `my/frame--adjust-for-docking' in my-ui-frame.el -- and
  ;; this number is then how that width is divided: one column of text each.
  (setq claude-code-ide-window-width 81)

  ;; What Emacs is to the session, said once, here, rather than in every
  ;; project's CLAUDE.md -- the package appends this to the system prompt of
  ;; every session it starts.  Emacs does two things for Claude, and each
  ;; needs saying, because left to itself Claude behaves as it would with
  ;; no editor attached.
  ;;
  ;; It answers questions.  The tools reach Claude and are not reached for:
  ;; asked what calls a function, it runs ripgrep.  So the prompt names them
  ;; and says when to prefer them.
  ;;
  ;; And it shows each change to a file as an ediff, to be read and edited
  ;; before it is applied -- but only a change made with Claude's Edit or
  ;; Write tool.  When the changes are many, or look mechanical, Claude
  ;; reaches for the shell instead: a python script, perl -i.  Such a change
  ;; lands in the file unseen, auto-revert picks it up, and nothing was ever
  ;; shown.  That is how a whole round of edits to a truss.org went in.  So
  ;; the prompt rules the shell out for writing files, and leaves it for
  ;; running the project's own tools that write them.
  ;;
  ;; No `;', `&' or `|' in this string.  The value is shell-quoted into the
  ;; command line that starts the CLI, and the backend then splits that line
  ;; back apart with `split-string-shell-command' -- which honours those
  ;; three as command separators even after `shell-quote-argument' has
  ;; escaped them, and returns only the words after the last one.  The
  ;; program name is among the words thrown away, so what fails is the exec,
  ;; reporting the tail of this sentence as a program it cannot find.
  (setq claude-code-ide-system-prompt
        (concat
         "Emacs is attached as the editor. "
         "It shows the user each change you make to a file as an ediff, "
         "which they read, and can edit, before it is applied, but only when "
         "the change is made with the Edit or Write tool. So write every change "
         "to a file with those tools, never through the shell: not with sed, "
         "perl or awk, not with a script, a heredoc or a redirect, not by "
         "copying another file over it, however many changes there are or "
         "however mechanical they look. A change made that way lands unseen. "
         "Running the project's own tools that write files, a formatter or a "
         "build, is another matter and is fine."))

  ;; The CLI is exec'd directly rather than through a shell, so on Windows it
  ;; is the native `claude' and both sides speak the same path form.  Nothing
  ;; to arrange for that; it is only worth knowing when a path looks wrong.

  ;; Only the CLI may talk to the server the package starts.  It listens on
  ;; 127.0.0.1 with no token, so a page open in the browser can reach it: a
  ;; WebSocket is not held back by the same-origin policy.  A new connection
  ;; also replaces the old one, so such a page would drop Claude and stand in
  ;; its place -- reading the selection and the open files, opening diffs.
  ;;
  ;; A browser names the page's origin when it opens a WebSocket, and the
  ;; CLI, which is no browser, sends no Origin at all; so a connection
  ;; carrying one is closed before the package sees it.  Said in the echo
  ;; area, since a CLI that ever began to send one would find its
  ;; connection refused, and this is where it would show.
  ;;
  ;; The function advised is the package's internal.  Should it be renamed,
  ;; the advice would wait on a name nothing calls and the server stand open
  ;; again, quietly; my-app-agent-test.el fails instead.
  (defun my/claude-code-ide-refuse-browser-socket (orig session ws)
    "Call ORIG with SESSION and WS, unless WS was opened by a web page."
    (if-let* ((origin (websocket-origin ws)))
        (progn
          (message "claude-code-ide: refused a connection from %s" origin)
          (websocket-close ws))
      (funcall orig session ws)))

  (advice-add 'claude-code-ide-mcp--on-open :around
              #'my/claude-code-ide-refuse-browser-socket)

  ;; On Windows the command line that starts the CLI is quoted one way and
  ;; split another.  The package writes it as one string and then splits it
  ;; back into words for ghostel, which execs them.  Writing, it quotes the
  ;; system prompt with `shell-quote-argument', which on Windows quotes for
  ;; cmd.exe -- a `%' would arrive as `^%' -- while the --add-dir org-dock
  ;; puts in `claude-code-ide-cli-extra-flags' is quoted for a POSIX shell.
  ;; Reading, `split-string-shell-command' takes a backslash as an escape
  ;; only where `shell-file-name-quote-list' is non-nil, and on Windows it
  ;; is nil so that a path's backslashes survive.
  ;;
  ;; Both halves are made to speak POSIX, as they do on the Mac.  Nothing in
  ;; the line has a backslash of its own to lose: the program is the bare
  ;; name `claude', resolved to a path only after the split.
  (when (eq system-type 'windows-nt)
    (require 'shell)
    (defvar shell-file-name-quote-list)   ; let-bound around the split

    (defun my/claude-code-ide-posix-command-line (orig &rest args)
      "Call ORIG with ARGS, quoting and splitting as for a POSIX shell."
      ;; The quote list is the value shell.el gives it everywhere but Windows.
      (let ((shell-file-name-quote-list
             (append shell-delimiter-argument-list
                     '(?\s ?$ ?\* ?\! ?\" ?\' ?\` ?\# ?\\))))
        (cl-letf* ((quote-argument (symbol-function 'shell-quote-argument))
                   ((symbol-function 'shell-quote-argument)
                    (lambda (argument &optional _posix)
                      (funcall quote-argument argument t))))
          (apply orig args))))

    (advice-add 'claude-code-ide--build-claude-command :around
                #'my/claude-code-ide-posix-command-line)
    (advice-add 'claude-code-ide--parse-command-string :around
                #'my/claude-code-ide-posix-command-line)))

;;;; Claude for org-dock's copies

;; Claude asked of a copy in its window, by claude-code-ide, where each
;; change is an ediff to take or refuse.  Never in the background: a Claude
;; nobody watches, let to run commands, is a Claude that does what any text
;; it reads tells it to.  It starts where claude-code-ide would, the
;; heading's project root: Claude keeps a conversation under the directory
;; it was started in, and only from there is it found again.
;;
;; In its window, one Claude for each file headings are taken out of, for
;; as long as it runs: a heading taken out again, or another of the same
;; file, goes on with what it was told, and a heading of another file --
;; another document, in the same project -- is not told it.  Closing the
;; window ends it, and the next copy of that file starts a new one.

(defvar my/org-dock-claude--instances (make-hash-table :test #'equal)
  "The claude-code-ide instance asked of copies of each file, by that file.")

(defun my/org-dock-claude--directory ()
  "Return where Claude starts for this copy: as claude-code-ide decides it."
  (if-let* ((project (project-current nil default-directory)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun my/org-dock-claude--show (session &optional select)
  "Show SESSION's Claude window; with SELECT, select it."
  (let ((buffer (claude-code-ide-mcp-session-buffer session)))
    (when-let* ((window (or (get-buffer-window buffer)
                            (let ((claude-code-ide-focus-on-open select))
                              (claude-code-ide--display-buffer-in-side-window buffer)))))
      (when select (select-window window)))))

(defun my/org-dock-claude--start (name)
  "Start a claude-code-ide instance here, named NAME when it needs a name.
Return its session.  A project with an instance already gives the new one
a name, which is otherwise asked for; NAME is the file's, so that the
window says which file's copies it is for."
  (let* ((dir (my/org-dock-claude--directory))
         (before (claude-code-ide-mcp--sessions-for-project dir)))
    (cl-letf (((symbol-function 'claude-code-ide--read-instance-name)
               (lambda (&rest _) name)))
      (claude-code-ide--start-session))
    (seq-find (lambda (session) (not (memq session before)))
              (claude-code-ide-mcp--sessions-for-project dir))))

(defun my/org-dock-claude--send-when-ready (session text &optional tries from)
  "Type TEXT into SESSION's Claude once it is connected, and show it.
A Claude just started takes a few seconds; after thirty it is left, and
the echo area says what to type.  Its window is selected only when the
window selected when this was first called, FROM, still is: moved on to
something else meanwhile -- another copy, another Claude -- what is
typed there is not taken over, and the echo area says it is ready."
  (let ((tries (or tries 0))
        (from (or from (selected-window))))
    (cond
     ((not (buffer-live-p (claude-code-ide-mcp-session-buffer session))))
     ((claude-code-ide-mcp-session-client session)
      (with-current-buffer (claude-code-ide-mcp-session-buffer session)
        (claude-code-ide--terminal-send-string text))
      (if (eq (selected-window) from)
          (my/org-dock-claude--show session t)
        (my/org-dock-claude--show session)
        (message "%s is ready" (buffer-name (claude-code-ide-mcp-session-buffer session)))))
     ((< tries 60)
      (run-with-timer 0.5 nil #'my/org-dock-claude--send-when-ready session text (1+ tries) from))
     (t (message "Claude has not connected; type %s in its window" text)))))

(defun my/org-dock-claude (request)
  "Make Claude, in its window, ready to work on the copy REQUEST names.
The request file -- which file to write, which to read, where point is --
goes into Claude's prompt as an @-mention, and the Claude window is
selected for what to ask to be typed after it: Ctrl+G there opens the
prompt in a buffer of this Emacs, for asking at length.  The project is
the heading's own, since the copy's `default-directory' is its file's
directory, so the sources are files of the project Claude started in.
Each change Claude makes to the copy is an ediff to take or refuse.

The Claude is the one asked of copies of the same file, while it runs;
else one started for that file.  With a prefix argument, one of the
project's Claudes is chosen and made the file's -- one resumed by hand,
say."
  (require 'claude-code-ide)
  (let* ((origin (plist-get request :origin))
         (name (my/org-dock-claude--name origin))
         (mention (concat "@" (plist-get request :request) " "))
         (own (gethash origin my/org-dock-claude--instances)))
    (cond
     (current-prefix-arg
      (let ((session (claude-code-ide--read-project-session
                      "Claude for this file: " (my/org-dock-claude--directory))))
        (puthash origin session my/org-dock-claude--instances)
        (my/org-dock-claude--send-when-ready session mention)))
     ((and own (buffer-live-p (claude-code-ide-mcp-session-buffer own)))
      (my/org-dock-claude--send-when-ready own mention))
     (t
      (my/org-dock-claude--send-when-ready
       (puthash origin (my/org-dock-claude--start name) my/org-dock-claude--instances)
       mention)))))

(defun my/org-dock-claude--name (origin)
  "Return the name of a Claude for copies of ORIGIN: its title, or its name."
  (or (when-let* ((buffer (and (file-name-absolute-p origin) (find-buffer-visiting origin))))
        (with-current-buffer buffer (org-get-title)))
      (file-name-base origin)))

(provide 'my-app-agent)
;;; my-app-agent.el ends here
