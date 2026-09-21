;;; early-init.el --- Minimal early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Disable package.el in favor of straight.el.

;;; Code:

(setq package-enable-at-startup nil)

;; Chrome the frame is never going to have, said before there is a frame.
;;
;; `tool-bar-mode -1' later does the same thing the long way round: the tool
;; bar is built with the frame, then torn off it, and the frame is measured
;; again around the space that frees -- 128 ms of it, measured, for something
;; no session ever shows.  Asked for here it is simply never made.
;;
;; `menu-bar-lines' is the same shape of saving and none of the cost on macOS,
;; where the menu bar is the system's and takes no room in the frame; it is
;; here for the other platforms, and for the symmetry.
;;
;; Only these two.  Everything else a frame is given -- fringes, transparency,
;; where it sits -- is `my/frame-default-alist' in my-ui-frame.el, and belongs
;; in one place; these are here for the one reason a setting has to be, which
;; is that afterwards is too late.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

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
