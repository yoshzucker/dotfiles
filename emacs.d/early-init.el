;;; early-init.el --- Minimal early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Disable package.el in favor of straight.el.

;;; Code:

(setq package-enable-at-startup nil)

;; Windows: one C toolchain, and it has to be chosen before anything compiles.
;;
;; Two things here want a C compiler and they are not interchangeable.
;; Tree-sitter builds grammars with whatever `executable-find' turns up.
;; Native compilation is fussier: libgccjit does not assemble or link by
;; itself, it invokes a gcc driver, and it has to be *its own* -- the driver
;; from the toolchain that built the libgccjit-0.dll Emacs loaded.  Hand it a
;; gcc from somewhere else and every compilation fails with "error invoking
;; gcc driver", each file falls back to byte-code, and the only trace is a
;; warning nobody keeps.  That is what was happening: bootstrap.ps1 copies
;; libgccjit from MSYS2's mingw64 into Emacs's own bin, while the front of
;; PATH offered a Scoop gcc from a different build entirely.
;;
;; So mingw64 goes first and serves both.  It is the same toolchain as the
;; libgccjit beside it, and it is MSVCRT-based like the official Emacs
;; Windows build, which is the runtime a grammar wants to be linked against
;; too.  The MSYS2 packages this configuration installs are all ucrt64
;; (see pkg/pacman/msys2-packages.txt), so mingw64/bin holds the native-comp
;; toolchain and little else -- putting it first shadows nothing that is used.
;;
;; In early-init because straight builds and native-compiles packages during
;; init, and a PATH set afterwards is a PATH set too late.
(when (eq system-type 'windows-nt)
  (let ((mingw (expand-file-name "~/scoop/apps/msys2/current/mingw64/bin")))
    (when (file-exists-p (expand-file-name "gcc.exe" mingw))
      (add-to-list 'exec-path mingw)
      (setenv "PATH" (concat mingw ";" (getenv "PATH"))))))
(setq straight-built-in-pseudo-packages
      '(project xref jsonrpc flymake external-completion eglot))

;;; early-init.el ends here
