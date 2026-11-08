;;; my-emacs-ops.el --- Operational settings for core Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; How Emacs behaves as a process rather than as an editor: garbage
;; collection, the server, startup behaviour, warnings, what the machine
;; underneath is doing, and the other global runtime adjustments.

;;; Code:

(setq delete-by-moving-to-trash t
      ring-bell-function 'ignore
      ;; Collections are not announced: a
      ;; line in the echo area overwrites whatever was there, which is the
      ;; message that was worth reading.
      garbage-collection-messages nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; Pipes to subprocesses as wide as Emacs reads from them.
;;
;; On Windows a pipe is made with the system's default buffer, four
;; kilobytes, while Emacs reads up to `read-process-output-max' -- sixty-four
;; -- at a time.  So a process that has a lot to say is read in many small
;; pieces, each a wait and a wake: a magit diff, ripgrep's matches, a language
;; server's reply.  The same width on both sides makes it one.
(when (eq system-type 'windows-nt)
  (setq w32-pipe-buffer-size read-process-output-max))

(use-package server
  :demand t
  :config
  ;; The server file back where emacsclient looks for it.  On Windows the
  ;; server speaks TCP and leaves its address and key in a file under
  ;; `server-auth-dir'; no-littering (loaded first, in core) moves that to
  ;; var/server/auth/, and emacsclient, which knows nothing of this config,
  ;; still looks in ~/.emacs.d/server/ -- "No socket or alternate editor".
  ;; If emacsclientw.exe still errors out after a Scoop update ("handle is
  ;; invalid"), quit Emacs completely and delete ~/.emacs.d/server/server.
  (setq server-auth-dir (locate-user-emacs-file "server/"))
  (unless (server-running-p)
    (server-start))

  ;; An editor for what runs in here and asks for one.  Claude Code's Ctrl+G
  ;; writes the prompt to a file, opens it in $VISUAL (else $EDITOR) and
  ;; waits for the editor to exit; Claude runs in a terminal inside this
  ;; Emacs, so the editor is this Emacs, through emacsclient, which returns
  ;; at `C-x #'.  Without -n: with it, emacsclient would return at once and
  ;; Claude read the prompt before it was written.  Set here, not in the
  ;; system, it reaches only what Emacs starts -- the terminals and what runs
  ;; in them -- and Windows needs no variable of its own.  The client beside
  ;; the running Emacs rather than the first on PATH, which on Windows is a
  ;; Scoop shim.
  (when-let* ((client (locate-file "emacsclient"
                                   (list invocation-directory
                                         (expand-file-name "bin" invocation-directory))
                                   exec-suffixes #'file-executable-p)))
    (setenv "VISUAL" client)
    (setenv "EDITOR" client)))

;; which-key: what a prefix can do, shown only when the next key is slow
;; in coming.  A menu on every prefix (capture's, a transient's) is in the
;; way of keys already known; this costs nothing to someone who types on,
;; and answers the one who stops.  Built into Emacs 30, and on from the
;; start because a mode that waits to be loaded is never there to ask.
(use-package which-key
  :straight nil
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; Collections wait for a pause.  The threshold early-init.el raises stays
;; raised, so a command conses without being collected underneath it, and
;; the collecting is done once Emacs has sat idle for fifteen seconds.
;; What that is worth, measured on this machine's own agenda: ten builds,
;; five seconds, and the collections that happen inside them.
;;
;;   threshold   time   GCs   in GC
;;      781k     5.2s    12    0.37s   7%
;;       32M     5.1s     7    0.35s   7%
;;      128M     5.2s     6    0.36s   7%
;;      256M     5.1s     3    0.25s   5%
;;      512M     5.0s     1    0.14s   3%
;;        1G     4.8s     0    0.00s   0%
;;
;; The count falls all the way down and the time does not, because a
;; collection costs what the *live* heap costs to walk -- fewer of them,
;; each dearer.  What removes the cost is the threshold being higher than
;; the command conses, so that none happens at all and the bill is settled
;; at the next pause.
;;
;; An idle timer also fires inside a command that waits -- on `sit-for', on
;; a process -- and collects there.  That is one collection and no more:
;; nothing lowers the threshold, so the rest of the command runs as it began.
(run-with-idle-timer 15 t #'garbage-collect)

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; *scratch* stays: killing it is refused rather than undone.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(provide 'my-emacs-ops)
;;; my-emacs-ops.el ends here
