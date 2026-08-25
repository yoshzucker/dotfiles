;;; my-emacs-ops.el --- Operational settings for core Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; How Emacs behaves as a process rather than as an editor: garbage
;; collection, the server, startup behaviour, warnings, what the machine
;; underneath is doing, and the other global runtime adjustments.

;;; Code:

(setq backup-directory-alist '((".*" . "~/.saves"))
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      garbage-collection-messages t)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package server
  :demand t
  :config
  ;; When emacsclientw.exe errors out (e.g. "handle is invalid" or "No socket"),
  ;; quit Emacs completely and delete ~/.emacs.d/server/server
  ;; (this is a common issue after Scoop updates on Windows) 
  (unless (server-running-p)
    (server-start)))

(use-package gcmh
  :diminish gcmh-mode
  :config
  (gcmh-mode 1))

(use-package symon
  ;; What the machine underneath is doing, in the echo area when nothing else
  ;; is being said.  Here rather than among the application integrations
  ;; because its subject is the process and the machine it runs on, which is
  ;; what the rest of this file is about.
  :config
  (defvar my/symon--last-message nil
    "Last echo-area string symon produced, to tell its own output from foreign messages.")
  (define-advice symon--display-update (:around (orig) my/yield-echo-area)
    "Let real echo-area messages and y/n prompts win over symon.
Only draw when the echo area is empty or still shows symon's own last output."
    (let ((cur (current-message)))
      (when (or (null cur) (equal cur my/symon--last-message))
        (funcall orig)
        (setq my/symon--last-message (current-message)))))
  (symon-mode))

(use-package immortal-scratch
  :config
  (setq eval-expression-print-length nil
        eval-expression-print-level nil)

  (immortal-scratch-mode 1))

(provide 'my-emacs-ops)
;;; my-emacs-ops.el ends here
