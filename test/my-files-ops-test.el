;;; my-files-ops-test.el --- Tests for where a file opened outside goes  -*- lexical-binding: t; -*-

;;; Commentary:

;; A PDF opens in the browser, never in Emacs.  `browse-url-of-file' looks
;; like the way to do that, but Emacs's own handlers send a file: URL that is
;; not HTML to `browse-url-emacs', and the PDF comes back into Emacs as text.
;; So: a PDF resolves to the browser, and what it is handed to is not Emacs.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'browse-url)

(defvar my-files-ops-test--loaded
  (my-test-load (my-test-module "my-files-ops")
                "my/open-\\(in-browser\\|with-system\\|as-text\\)-extension\\|my/open--\\(browser\\|resolve-method\\|extension\\|force-p\\)")
  "How many definitions were taken out of the module.")

(ert-deftest my-files-ops-test-pdf-goes-to-browser ()
  "A PDF resolves to the browser."
  (should (>= my-files-ops-test--loaded 4))
  (should (eq (my/open--resolve-method "/x/a.pdf" 'auto nil) 'browser)))

(ert-deftest my-files-ops-test-browser-is-not-emacs ()
  "A PDF handed to the browser leaves Emacs."
  (let (called)
    (cl-letf (((symbol-function 'browse-url-default-browser)
               (lambda (url &rest _) (setq called (list 'outside url))))
              ((symbol-function 'browse-url-emacs)
               (lambda (url &rest _) (setq called (list 'emacs url)))))
      (should-not (my/open--browser "/x/a.pdf")))
    (should (eq (car called) 'outside))
    (should (string-match-p "\\`file://.*a\\.pdf\\'" (cadr called)))))

(provide 'my-files-ops-test)
;;; my-files-ops-test.el ends here
