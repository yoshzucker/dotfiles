;;; my-core-encoding.el --- Language and encoding settings -*- lexical-binding: t; -*-

;;; Commentary:
;; System-dependent language environment and file encoding settings.
;; Supports Linux, macOS (HFS), and Windows (CP932).

;;; Code:

(pcase system-type
  ('gnu/linux
   (set-language-environment "Japanese")
   (prefer-coding-system 'utf-8)
   (set-file-name-coding-system 'utf-8)
   (setq locale-coding-system 'utf-8))

  ('darwin
   (when (require 'ucs-normalize nil t)
     (set-language-environment "Japanese")
     (prefer-coding-system 'utf-8)
     (set-file-name-coding-system 'utf-8-hfs)
     (setq locale-coding-system 'utf-8)))

  ('windows-nt
   (set-language-environment "Japanese")
   (prefer-coding-system 'utf-8)
   (set-file-name-coding-system 'cp932)))

(provide 'my-core-encoding)
;;; my-core-encoding.el ends here
