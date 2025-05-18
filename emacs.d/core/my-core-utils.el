;;; my-core-utils.el --- Core utility macros and functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides macros and helper functions shared across core and modules.

;;; Code:

(use-package diminish
  :config
  (defmacro diminish-major-mode (hook new-name)
    `(add-hook ,hook (lambda ()
                       (setq mode-name ,new-name)))))

;;;###autoload
(defmacro my/map-env (emacs-var env-var &optional mapping)
  "Set EMACS-VAR from ENV-VAR, optionally using MAPPING.
If MAPPING is provided, it should be an alist mapping string values
to symbols or other values. If not, the value is interned."
  `(setq ,emacs-var
         (let ((val (getenv ,env-var)))
           (cond
            ((and val ,mapping) (cdr (assoc val ,mapping)))
            (val (intern val))
            (t ,emacs-var)))))

(defun my/plist-key-values (plist key)
  "Return all values for KEY from PLIST until the next keyword or end."
  (let ((result nil)
        (found nil))
    (while plist
      (let ((el (car plist)))
        (cond
         ((eq el key)
          (setq found t)
          (setq plist (cdr plist)))
         ((and found (keywordp el))
          (setq plist nil)) ;; break on next keyword
         (found
          (push el result)
          (setq plist (cdr plist)))
         (t
          (setq plist (cdr plist))))))
    (nreverse result)))

(defun my/wrap-after (body afters)
  "Wrap BODY in nested with-eval-after-load forms for each AFTER entry."
  (if (null afters)
      body
    `(with-eval-after-load ',(car afters)
        ,(my/wrap-after body (cdr afters)))))

;;;###autoload
(defmacro my/define-key (&rest specs)
  "Declarative keybinding macro with keyword syntax:
  :map     SYMBOLS      -- One or more keymap symbols (no parentheses).
  :state   STATES       -- Evil states (e.g., normal, insert).
  :prefix  STRING       -- Optional prefix key to prepend.
  :key     KEY-FUNC     -- Flat key/function pairs, or (KEYS...) FUNC pairs.
  :after   FEATURES     -- Optional features; delay bindings with `with-eval-after-load`."
  `(progn
     ,@(mapcan
        (lambda (spec)
          (unless (listp spec)
            (error "Invalid key spec: %S" spec))
          (let* ((maps   (my/plist-key-values spec :map))
                 (afters (my/plist-key-values spec :after))
                 (states (my/plist-key-values spec :state))
                 (prefix (car (my/plist-key-values spec :prefix)))
                 (pairs  (my/plist-key-values spec :key))
                 (body (mapcan (lambda (map)
                                 (cl-loop for (keys func) on pairs by 'cddr
                                          append (cl-loop for key in (if (listp keys) keys (list keys)) 
                                                          append (let ((full-key (if prefix (concat prefix " " key) key)))
                                                                   (if states
                                                                       (cl-loop for state in states
                                                                                collect `(evil-define-key ',state ,map (kbd ,full-key) ,func))
                                                                     (list `(define-key ,map (kbd ,full-key) ,func)))))))
                               maps)))
            (list (my/wrap-after `(progn ,@body) afters))))
        specs)))

;;;###autoload
(defmacro my/add-hook (&rest specs)
  "Declarative `add-hook` macro with keyword syntax:
  :hook    HOOKS        -- One or more hook symbols (no parentheses).
  :func    FUNCTIONS    -- One or more function symbols to add.
  :after   FEATURES     -- Optional features; delay hook registration with `with-eval-after-load`."
  `(progn
     ,@(mapcan
        (lambda (spec)
          (unless (listp spec)
            (error "Invalid key spec: %S" spec))
          (let* ((hooks  (my/plist-key-values spec :hook))
                 (afters (my/plist-key-values spec :after))
                 (funcs  (my/plist-key-values spec :func))
                 (body (mapcan (lambda (hook)
                                 (mapcar (lambda (func)
                                           `(add-hook ',hook ,func))
                                         funcs))
                               hooks)))
            (list (my/wrap-after `(progn ,@body) afters))))
        specs)))

(provide 'my-core-utils)
;;; my-core-utils.el ends here
