;;; my-core-keys.cl --- Core key bindings and modifier settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Basic keyboard setup: unbind keys, set modifiers, define platform-specific key sequences.

;;; Code:

;; remap '¥' (Yen) to '\\' (Backslash)
(when (eq system-type 'darwin)
  (keyboard-translate ?¥ ?\\))

;; Heuristics to guess slash/backslash key representations
(defcustom my/key-symbols
  '((backslash . ((darwin . "_") (default . "\\")))
    (slash     . ((default . "_")))
    (c-:       . (((darwin . gui) . "s-:") (default . "C-:"))))
  "Mapping of symbolic keys to OS and display (GUI/TTY) specific representations.
Each entry can have keys like (system-type . gui), (system-type . tty), system-type, tty or just 'default."
  :type '(alist :key-type symbol
                :value-type
                (alist :key-type (choice symbol (cons symbol symbol))
                       :value-type string)))

(defun my/system-key (key)
  "Return appropriate key representation for KEY symbol based on system type and display environment."
  (let ((sys system-type)
        (env (if (display-graphic-p) 'gui 'tty))
        (key-table (alist-get key my/key-symbols)))
    (or (cdr (assoc (cons sys env) key-table))
        (cdr (assoc sys key-table))
        (cdr (assoc env key-table))
        (cdr (assoc 'default key-table)))))

(defcustom my/backslash (my/system-key 'backslash)
  "Automatically guessed backslash key, overridable by user."
  :type 'string)

(defcustom my/slash (my/system-key 'slash)
  "Automatically guessed slash key, overridable by user."
  :type 'string)

(defcustom my/c-: (my/system-key 'c-:)
  "Automatically guessed C-: key, overridable by user."
  :type 'string)

;; Unbind certain global keys
(dolist (key `(,my/c-: "M-:" "C-z"))
  (global-unset-key (kbd key)))

;; Key translation
(dolist (key '("<C-return>" "C-<return>"))
  (define-key key-translation-map (kbd key) (kbd "C-RET")))

(provide 'my-core-keys)
;;; my-core-keys.el ends here
