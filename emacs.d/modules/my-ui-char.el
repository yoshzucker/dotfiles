;;; my-ui-char.el --- Character width and glyph display settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Tweaks for CJK character width and glyph rendering in terminal and Windows.

;; How to inspect the settings applied by `set-language-environment`:
;; 1. Evaluate `(get-language-info "Japanese" 'setup-function)`
;; 2. Examine the returned function (e.g., `setup-japanese-environment-internal`)
;; 3. You’ll see it calls `prefer-coding-system` and `use-cjk-char-width-table`
;;    — so always call `prefer-coding-system` after `set-language-environment`

;; Explicitly specifying character widths:
;; 1. If cursor movement is incorrect when moving left/right, your width table may be wrong
;; 2. Evaluate `(char-width ?•)` to check the current width of “•”
;; 3. Place the cursor on the character and press `C-u C-x =`
;; 4. The *Help* buffer will show the Unicode code point (e.g., 0x2340 for Japanese-jisx0213.2004-1 “•”)
;; 5. Use `(aset char-width-table ?• 2)` or `(aset char-width-table #x2340 2)`
;; 6. Or specify a range in `cjk-char-width-table-list` and call `use-cjk-char-width-table`  
;;    to set width 2 for an entire block of code points

;; Note: In a terminal, parts of the Japanese-jisx0213.2004-1 block are treated as width 2.

;;; Code:

(when (not (display-graphic-p))
  ;; Configure ambiguous-width CJK characters in terminal Emacs
  (setq cjk-char-width-table-list
        '((ja_JP nil
                 (japanese-jisx0213.2004-1 (#x2121 . #x2D7E))
                 (japanese-jisx0208 (#x2121 . #x287E))
                 (cp932-2-byte (#x8140 . #x879F)))
          (zh_CN nil
                 (chinese-gb2312 (#x2121 . #x297E)))
          (zh_HK nil
                 (big5-hkscs (#xA140 . #xA3FE) (#xC6A0 . #xC8FE)))
          (zh_TW nil
                 (big5 (#xA140 . #xA3FE))
                 (chinese-cns11643-1 (#x2121 . #x427E)))
          (ko_KR nil
                 (korean-ksc5601 (#x2121 . #x2C7E)))))
  (use-cjk-char-width-table 'ja_JP)
  ;; Specific glyph fixes
  (aset char-width-table ?© 1))

(when (eq system-type 'windows-nt)
  (let ((table (or (default-value 'buffer-display-table)
                   standard-display-table
                   (make-display-table)))
        (glyph (if (display-graphic-p)
                   ?╲   ; GUI: diagonal slash
                 ?∖))) ; Terminal: division slash
    (setq-default buffer-display-table table)
    (aset table ?\\ (vector glyph))))

(provide 'my-ui-char)
;;; my-ui-char.el ends here
