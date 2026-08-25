;;; my-app-eww.el --- Reading HTML without leaving Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Not for the web, which stopped rendering without JavaScript some years ago.
;; For the HTML that is already local and already the answer: the Common Lisp
;; HyperSpec (see my-lang-common-lisp.el, which routes it here), and whatever
;; Org has just exported.  Anything that wants a real browser is opened in one.

;;; Code:

(use-package eww
  :defer t
  :config
  ;; Proportional fonts in a terminal make shr's column arithmetic wrong: the
  ;; width it measures is not the width the terminal draws, and tables come
  ;; out ragged.
  (unless (display-graphic-p)
    (setq shr-use-fonts nil))

  ;; A name per page.  eww reuses the buffer called `*eww*'; renamed, it finds
  ;; none and makes a new one -- which is what lets two HyperSpec entries be
  ;; open beside each other.
  (defun my/eww-rename-buffer ()
    "Give this EWW buffer a name of its own."
    (rename-buffer "eww" t))

  (add-hook 'eww-mode-hook #'my/eww-rename-buffer))

(provide 'my-app-eww)
;;; my-app-eww.el ends here
