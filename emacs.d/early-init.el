;;; early-init.el --- Minimal early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Disable package.el in favor of straight.el.

;;; Code:

(setq package-enable-at-startup nil)

;; Collections are rare, from the start and for the whole session.
;;
;; At the stock 800k a startup collects thirty-one times, a quarter of a second
;; out of a second and a third on macOS -- each one walking a heap that is only
;; growing, for garbage the next few files will make again.  The threshold is
;; never put back: my-emacs-ops.el collects when Emacs is idle instead, which
;; is why it can stay this high.
(setq gc-cons-threshold (* 1024 1024 1024))

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

;; Windows: ~/.local/bin ahead of scoop's shims, because the shell puts it
;; there and the two have to agree.
;;
;; A tool installed both by scoop and by its own installer is two programs
;; under one name.  config/shell/env/common.sh prepends ~/.local/bin, so the
;; shell runs the installer's; the Windows PATH appends it, so Emacs would
;; run scoop's.  The Claude Code installer puts `claude' there.
;;
;; The shell's answer is the one to match rather than a third one to invent:
;; the installer's copy is the one keeping itself up to date.
;;
;; Windows only because it is the only one that can diverge.  On the Mac
;; `exec-path-from-shell' starts a login shell and takes its PATH whole
;; (core/my-core-env.el), so Emacs is looking at the list the shell built.
(when (eq system-type 'windows-nt)
  (let ((local (expand-file-name "~/.local/bin")))
    (when (file-directory-p local)
      (add-to-list 'exec-path local)
      (setenv "PATH" (concat local ";" (getenv "PATH"))))))
(setq straight-built-in-pseudo-packages
      '(org project xref jsonrpc flymake external-completion eglot))

;;; early-init.el ends here
