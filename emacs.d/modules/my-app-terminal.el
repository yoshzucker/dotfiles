;;; my-app-terminal.el --- A terminal inside Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; The other half of my-ui-tmux.el.  There Emacs is the guest: it runs in a
;; pane and has to agree with tmux about what a window is.  Here Emacs is the
;; host and the shell is the guest.
;;
;; ghostel drives the shell through libghostty's VT engine in a dynamic
;; module, which is also why it is this one rather than vterm or eat: vterm
;; has no Windows support at all and eat needs a real PTY, which Windows does
;; not give Emacs.  ghostel has a ConPTY backend and ships a prebuilt module
;; for the x86_64 Windows Emacs this configuration installs.

;;; Code:

(use-package ghostel
  :defer t
  :init
  ;; Both of these are in `:init' for the same reason: they have to be true
  ;; before the package loads.  The keys name autoloaded commands, and a key
  ;; that works only once the package is loaded is a key that can never load
  ;; it.  `ghostel-module-directory' is read while ghostel.el is loading,
  ;; because that is when it loads the native module.
  (my/define-key
   (:map global-map
         :prefix "C-c t"
         :key
         "t" #'ghostel
         "p" #'ghostel-project
         "l" #'ghostel-list-buffers))

  ;; Where the native module lives and how it is obtained.  Out of the
  ;; package tree, because straight rebuilds that tree and a rebuild would
  ;; delete the module out from under a running Emacs.
  ;;
  ;; Downloaded rather than compiled.  Compiling wants exactly Zig 0.16.0 and
  ;; neither machine here has any Zig; worse, the compile is asynchronous, so
  ;; choosing it returns immediately and the command that asked for a
  ;; terminal walks straight into a module that is not there yet.
  (setq ghostel-module-directory
        (expand-file-name "ghostel/" user-emacs-directory)
        ghostel-module-auto-install 'download)

  :config
  (when (eq system-type 'windows-nt)
    ;; `ghostel-shell' follows $SHELL, and Emacs on Windows points that at
    ;; its own cmdproxy.exe -- the helper it uses to quote arguments when it
    ;; calls a shell, not a shell to sit in.  So the shell is named here, and
    ;; it is the one mintty opens.
    ;;
    ;; MSYSTEM is what makes it that shell rather than a bare MSYS one:
    ;; /etc/profile reads it to build PATH for the ucrt64 tree, and
    ;; config/shell/env/msys.sh returns immediately without it.  Reading
    ;; /etc/profile at all is what `-l' is for.
    ;;
    ;; TERMINFO, because ncurses reads it as a colon-separated search path
    ;; and ghostel names the directory in Windows form: MSYS2's ncurses
    ;; splits `c:/Users/...' at the drive letter, looks in `c' and in
    ;; `/Users/...', and finds no xterm-ghostty in either.  A shell that
    ;; cannot read its terminfo cannot erase a character, so backspace walks
    ;; the cursor rightwards instead of deleting.  The same directory in
    ;; MSYS2's own form carries no colon.  `ghostel-environment' is
    ;; prepended to the spawn environment, so this wins over ghostel's own.
    ;;
    ;; LANG, because Windows hands Emacs `JPN', which is not a POSIX locale
    ;; name: MSYS2 fails to set it, falls back to C, and stops reading the
    ;; stream as UTF-8.  mintty runs this same shell as ja_JP.UTF-8.
    (setq ghostel-shell
          (list (expand-file-name "~/scoop/apps/msys2/current/usr/bin/zsh.exe")
                "-l")
          ghostel-environment
          (list "MSYSTEM=UCRT64"
                "LANG=ja_JP.UTF-8"
                (concat "TERMINFO="
                        (replace-regexp-in-string
                         "\\`\\([A-Za-z]\\):" "/\\1"
                         (expand-file-name
                          "etc/terminfo"
                          (file-name-directory (locate-library "ghostel")))))))))

;; Evil's operators over a line that the shell owns: `d' and `c' clamp to the
;; input and apply it over the PTY, `i' and `a' drive the shell's cursor to
;; point, `p' pastes through it.  Anything outside the shell's own line
;; editing -- a full-screen TUI, copy mode -- falls through to plain evil.
(use-package evil-ghostel
  :after ghostel)

(provide 'my-app-terminal)
;;; my-app-terminal.el ends here
