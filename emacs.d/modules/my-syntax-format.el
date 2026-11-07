;;; my-syntax-format.el --- Formatting code on save -*- lexical-binding: t; -*-

;;; Commentary:
;; apheleia, which formats a buffer on save with whatever formatter its
;; major mode has, and leaves alone a buffer whose formatter is not there.

;;; Code:

(use-package apheleia
  ;; Where code is written, and not where anything else is.  The global mode
  ;; turns it on in every buffer there is and leaves it to each one's major
  ;; mode to have no formatter; a hook says the same thing without loading a
  ;; formatter to open a text file.
  ;;
  ;; Everywhere, Windows included.  Nothing in it is Unix's: it runs a
  ;; formatter as a subprocess and swaps the result in without moving
  ;; point, and a formatter that is not installed is resolved with
  ;; `executable-find', logged, and let go of -- the buffer is saved
  ;; either way.
  :diminish apheleia-mode
  :defer t
  :init
  (my/add-hook
   (:hook prog-mode-hook
          :func #'apheleia-mode)))

(provide 'my-syntax-format)
;;; my-syntax-format.el ends here
