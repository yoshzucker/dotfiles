;;; my-test.el --- Load pieces of the configuration for testing  -*- lexical-binding: t; -*-

;;; Commentary:

;; The configuration is one Emacs's worth of `use-package' forms, most of them
;; wrapped around packages straight has to clone first.  A test cannot load a
;; module the way Emacs does without building that whole world, so it takes
;; the definitions it is about to test out of the module by name and
;; evaluates those.
;;
;; That limit is worth saying plainly rather than discovering later: a test
;; here exercises the functions, not the `:config' block that installs them.
;; A definition that only works because of a `setq' elsewhere in the module
;; will pass here and fail in Emacs.  So the thing to test this way is a
;; function with an argument and an answer, or one whose effect on a buffer
;; can be read back -- not the wiring.
;;
;; `my-test-load' returns how many forms it evaluated, and a test file is
;; expected to check that the names it wanted are really there.  A renamed
;; function would otherwise leave a file of tests that pass because they
;; test nothing.

;;; Code:

(require 'cl-lib)

(defun my-test--forms-in (form pattern found)
  "Evaluate sub-forms of FORM whose printed text matches PATTERN.
FOUND is a counter cons cell.  Walks car and cdr rather than mapping, so a
dotted pair in a quoted setting does not end the walk."
  (when (consp form)
    (if (and (memq (car-safe form)
                   '(defun defvar defconst defmacro defcustom
                     advice-add add-to-list add-hook))
             (string-match-p pattern (prin1-to-string form)))
        (progn (eval form t) (cl-incf (car found)))
      (my-test--forms-in (car form) pattern found)
      (my-test--forms-in (cdr form) pattern found))))

(defun my-test-load (file pattern)
  "Evaluate the definitions in FILE whose printed text matches PATTERN.

Returns how many forms were evaluated.  Definitions nested inside a
`use-package' form are reached, which is where most of this configuration
keeps them."
  (let ((found (list 0)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (my-test--forms-in (read (current-buffer)) pattern found))
        (end-of-file nil)))
    (car found)))

(defun my-test-module (name)
  "Return the absolute path of configuration module NAME."
  (expand-file-name (format "emacs.d/modules/%s.el" name)
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "emacs.d")))

(provide 'my-test)

;;; my-test.el ends here
