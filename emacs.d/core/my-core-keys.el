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
    (slash     . ((default . "_"))))
  "Mapping of symbolic keys to OS-specific representations.
Each entry is keyed by a `system-type' symbol or by `default'."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type string)))

(defun my/system-key (key)
  "Return the representation of KEY symbol for this `system-type'."
  (let ((key-table (alist-get key my/key-symbols)))
    (or (cdr (assq system-type key-table))
        (cdr (assq 'default key-table)))))

(defcustom my/backslash (my/system-key 'backslash)
  "Automatically guessed backslash key, overridable by user."
  :type 'string)

(defcustom my/slash (my/system-key 'slash)
  "Automatically guessed slash key, overridable by user."
  :type 'string)

;; Unbind certain global keys
(dolist (key '("C-:" "M-:" "C-z"))
  (global-unset-key (kbd key)))

;; Key translation
(dolist (key '("<C-return>" "C-<return>"))
  (define-key key-translation-map (kbd key) (kbd "C-RET")))

(provide 'my-core-keys)
;;; my-core-keys.el ends here
