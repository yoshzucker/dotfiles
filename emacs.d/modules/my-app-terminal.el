;;; my-app-terminal.el --- A terminal inside Emacs -*- lexical-binding: t; -*-

;;; Commentary:
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
  ;;
  ;; The prefix is `C-c g', for ghostel.
  (my/define-key
   (:map global-map
         :prefix "C-c g"
         :key
         "s" #'ghostel
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

  ;; What is downloaded is a native module, loaded into Emacs, and a ConPTY
  ;; and OpenConsole on Windows -- programs, taken from a release page over
  ;; HTTPS with nothing checked but the transport.  The version is the one
  ;; the package names, so the lockfile already fixes which release; this
  ;; fixes what that release may contain.  Each file is taken only when its
  ;; SHA-256 is the one recorded here, from the digests GitHub shows for the
  ;; release's assets; anything else is deleted and the download counts as
  ;; failed.  A ghostel that wants a newer release stops here until its
  ;; digests are added:
  ;;
  ;;   curl -s https://api.github.com/repos/dakra/ghostel/releases/tags/vX.Y.Z |
  ;;     python3 -c 'import json,sys
  ;;   for a in json.load(sys.stdin)["assets"]: print(a["name"], a["digest"])'
  (defconst my/ghostel-asset-sha256
    '(("0.57.0"
       ("ghostel-module-aarch64-macos.dylib"
        . "77eb1a4425e2f69668fd7632a985732ee1292059b9c1f8ecf393b84876e53877")
       ("ghostel-module-x86_64-windows.dll"
        . "d80b310a3f487dbf569e24f8f67d20c138b6a2103311202f418dc61a3f8015de")
       ("ghostel-conpty-x86_64-windows.dll"
        . "c46dcd04f52b97f6a8cf53e8f547c85a821660bed18de2b3344afcd4a8389ad6")
       ("ghostel-openconsole-x86_64-windows-x64.exe"
        . "47828c3fe080212f69dfdb39ab3673170fcc7445924c76fe003cefd18247dd5d")
       ("ghostel-openconsole-x86_64-windows-arm64.exe"
        . "29cb3a9471c5b13bfb3ac812043e496199ba776ced1c5e14d30cb1234433a437"))
      ("0.60.0"
       ("ghostel-module-aarch64-macos.dylib"
        . "efe14b3d1ef9c9072886ae2ab5cf7ba96d05414b768315bf6e68eaea2fe33916")
       ("ghostel-module-x86_64-windows.dll"
        . "acf66453d3a73a52bbbb8624b44c04300742dcf0d01fd406a9fd8e4fa3072a3f")
       ("ghostel-conpty-x86_64-windows.dll"
        . "c46dcd04f52b97f6a8cf53e8f547c85a821660bed18de2b3344afcd4a8389ad6")
       ("ghostel-openconsole-x86_64-windows-x64.exe"
        . "47828c3fe080212f69dfdb39ab3673170fcc7445924c76fe003cefd18247dd5d")
       ("ghostel-openconsole-x86_64-windows-arm64.exe"
        . "29cb3a9471c5b13bfb3ac812043e496199ba776ced1c5e14d30cb1234433a437")))
    "SHA-256 of each ghostel release asset this configuration will take.")

  (defun my/ghostel-verify-download (orig url dest)
    "Call ORIG to fetch URL into DEST, and keep DEST only if its hash is known."
    ;; Read from the URL asked for, which names the release and the asset;
    ;; the one returned is wherever GitHub redirected to.  A `latest' URL
    ;; names no release, so nothing it fetches is kept.
    (when-let* ((final (funcall orig url dest)))
      (let* ((path (url-filename (url-generic-parse-url url)))
             (version (and (string-match "/download/v\\([^/]+\\)/" path)
                           (match-string 1 path)))
             (expected (cdr (assoc (file-name-nondirectory path)
                                   (cdr (assoc version my/ghostel-asset-sha256)))))
             (actual (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally dest)
                       (secure-hash 'sha256 (current-buffer)))))
        (if (and expected (string= expected actual))
            final
          (delete-file dest)
          (display-warning
           'ghostel
           (format "Refused %s: SHA-256 %s is not the one recorded in my-app-terminal.el"
                   (file-name-nondirectory path) actual)
           :error)
          nil))))

  (advice-add 'ghostel--download-file :around #'my/ghostel-verify-download)

  :config
  (when (eq system-type 'windows-nt)
    ;; `ghostel-shell' follows $SHELL, and Emacs on Windows points that at
    ;; its own cmdproxy.exe -- the helper it uses to quote arguments when it
    ;; calls a shell, not a shell to sit in.  So the shell is named here:
    ;; Git Bash, as mintty opens it, set up by ~/.bashrc the same way for
    ;; both.
    ;;
    ;; TERMINFO, because ncurses reads it as a colon-separated search path
    ;; and ghostel names the directory in Windows form: Git Bash's ncurses
    ;; splits `c:/Users/...' at the drive letter, looks in `c' and in
    ;; `/Users/...', and finds no xterm-ghostty in either.  A shell that
    ;; cannot read its terminfo cannot erase a character, so backspace walks
    ;; the cursor rightwards instead of deleting.  The same directory in
    ;; Git Bash's own form carries no colon.  `ghostel-environment' is
    ;; prepended to the spawn environment, so this wins over ghostel's own.
    ;;
    ;; LANG, because Windows hands Emacs `JPN', which is not a POSIX locale
    ;; name: Git Bash fails to set it, falls back to C, and stops reading the
    ;; stream as UTF-8.  mintty runs this same shell as ja_JP.UTF-8.
    (setq ghostel-shell
          (list (expand-file-name "~/scoop/apps/git/current/usr/bin/bash.exe")
                "-l")
          ghostel-environment
          (list "LANG=ja_JP.UTF-8"
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
;; The mode is per buffer and nothing in the package turns it on.
;;
;; C-[ always enters normal state, whatever ESC is routed to, so a TUI can
;; keep ESC for itself.  That takes C-[ arriving as `<C-[>', which only a
;; graphical frame gives -- see `my/distinguish-control-bracket' in
;; my-editor-evil.el.
(use-package evil-ghostel
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (evil-define-key* 'insert evil-ghostel-mode-map
                    [C-\[] #'evil-normal-state))

(provide 'my-app-terminal)
;;; my-app-terminal.el ends here
