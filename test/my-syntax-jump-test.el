;;; my-syntax-jump-test.el --- Tests for the ASCII-only pattern given to rg  -*- lexical-binding: t; -*-

;;; Commentary:

;; On Windows `:rg' hands ripgrep its pattern on a shell line that Windows
;; reads as cp932, so my-syntax-jump.el writes every non-ASCII character as
;; ripgrep's \x{HEX} first.  It does that by advising one of xref's internal
;; names: rename it upstream and the advice waits on a name nothing calls,
;; and a Japanese `:rg' quietly matches nothing again.  So: the name is
;; still xref's, the conversion leaves ASCII alone, and ripgrep reads what
;; it makes as the characters it replaced.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'xref)

(defvar my-syntax-jump-test--loaded
  (my-test-load (my-test-module "my-syntax-jump") "xref-ascii-regexp")
  "How many definitions were taken out of the module.")

(ert-deftest my-syntax-jump-test-advised-name-is-xrefs ()
  "The function advised on Windows still exists in xref."
  (should (>= my-syntax-jump-test--loaded 1))
  (should (fboundp 'xref--regexp-to-extended)))

(ert-deftest my-syntax-jump-test-ascii-left-alone ()
  "ASCII, regexp syntax included, passes through unchanged."
  (should (equal (my/xref-ascii-regexp "foo|ba[rz]\\b") "foo|ba[rz]\\b")))

(ert-deftest my-syntax-jump-test-non-ascii-escaped ()
  "Each non-ASCII character becomes \\x{HEX}."
  (should (equal (my/xref-ascii-regexp "検索|a")
                 "\\x{691C}\\x{7D22}|a")))

(ert-deftest my-syntax-jump-test-ripgrep-reads-it-back ()
  "ripgrep matches the escaped pattern where the original appears."
  (skip-unless (executable-find "rg"))
  (let ((file (make-temp-file "my-syntax-jump-test" nil ".txt")))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file file (insert "これは検索のテスト\nenglish line\n"))
          (with-temp-buffer
            (should (eq 0 (call-process "rg" nil t nil "--no-config" "-c"
                                        (my/xref-ascii-regexp
                                         (xref--regexp-to-extended "検索"))
                                        file)))
            (should (equal (string-trim (buffer-string)) "1"))))
      (delete-file file))))

(provide 'my-syntax-jump-test)
;;; my-syntax-jump-test.el ends here
