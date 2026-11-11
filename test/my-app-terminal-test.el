;;; my-app-terminal-test.el --- Tests for the guards on ghostel  -*- lexical-binding: t; -*-

;;; Commentary:

;; ghostel downloads a native module, and on Windows a ConPTY and an
;; OpenConsole, and my-app-terminal.el keeps each only when its SHA-256 is
;; the one recorded.  The check is advice on one of ghostel's internal
;; functions: rename it upstream and the advice waits on a name nothing
;; calls, and whatever the release holds is loaded unchecked.  The first test
;; is the one that matters -- the name advised is still ghostel's.
;;
;; Against straight's build of ghostel, so it is skipped where straight has
;; not built it.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)

(defconst my-app-terminal-test--build
  (expand-file-name "straight/build" (or (getenv "STRAIGHT_DIR")
                                         (expand-file-name "~/.emacs.d")))
  "Where straight builds packages.")

(defvar my-app-terminal-test--available
  (when (file-directory-p (expand-file-name "ghostel" my-app-terminal-test--build))
    (add-to-list 'load-path (expand-file-name "ghostel" my-app-terminal-test--build) t)
    (require 'ghostel-module-install nil t))
  "Non-nil when ghostel's installer could be loaded.")

(defvar my-app-terminal-test--loaded
  (my-test-load (my-test-module "my-app-terminal") "ghostel-verify\\|ghostel-asset")
  "How many definitions were taken out of the module.")

(ert-deftest my-app-terminal-test-advised-name-is-ghostels ()
  "The function advised still exists in ghostel, and is advised."
  (skip-unless my-app-terminal-test--available)
  (should (>= my-app-terminal-test--loaded 3))
  (should (fboundp 'ghostel--download-file))
  (should (advice-member-p #'my/ghostel-verify-download 'ghostel--download-file)))

(ert-deftest my-app-terminal-test-only-a-known-hash-is-kept ()
  "A download is kept when its hash is recorded, and deleted otherwise."
  (let* ((dir (make-temp-file "ghostel-test" t))
         (dest (expand-file-name "module" dir))
         (url "https://github.com/dakra/ghostel/releases/download/v9.9.9/asset.dll")
         (fetch (lambda (content)
                  (lambda (_url dest)
                    (with-temp-file dest (insert content))
                    "https://objects.githubusercontent.com/redirected")))
         (my/ghostel-asset-sha256
          `(("9.9.9" ("asset.dll" . ,(secure-hash 'sha256 "good"))))))
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning) #'ignore))
          (should (my/ghostel-verify-download (funcall fetch "good") url dest))
          (should (file-exists-p dest))
          (should-not (my/ghostel-verify-download (funcall fetch "bad") url dest))
          (should-not (file-exists-p dest))
          (should-not (my/ghostel-verify-download
                       (funcall fetch "good")
                       "https://github.com/dakra/ghostel/releases/download/v1.0.0/asset.dll"
                       dest))
          (should-not (file-exists-p dest))
          (should-not (my/ghostel-verify-download
                       (funcall fetch "good")
                       "https://github.com/dakra/ghostel/releases/latest/download/asset.dll"
                       dest)))
      (delete-directory dir t))))

;; my-app-terminal.el empties `ghostel-eval-cmds' so that nothing printed in
;; the terminal can call into Emacs.  Renamed upstream, the setting would
;; land on a name nothing reads and the default list would be back.  ghostel
;; itself loads a native module, so only its handler is taken out of it.
(defvar ghostel-eval-cmds)

(ert-deftest my-app-terminal-test-eval-follows-the-list ()
  "An OSC 52;e request calls a function when it is listed, and only then."
  (skip-unless my-app-terminal-test--available)
  (should (= (my-test-load (locate-library "ghostel.el" t)
                           "\\`(defun ghostel--osc52-eval ")
             1))
  (let (called)
    (cl-letf (((symbol-function 'find-file)
               (lambda (&rest args) (setq called args))))
      (let ((ghostel-eval-cmds '(("find-file" find-file))))
        (ghostel--osc52-eval "\"find-file\" \"/tmp/x\"")
        (should (equal called '("/tmp/x"))))
      (setq called nil)
      (let ((ghostel-eval-cmds nil)
            (inhibit-message t))
        (ghostel--osc52-eval "\"find-file\" \"/tmp/x\"")
        (should-not called)))))

(provide 'my-app-terminal-test)
;;; my-app-terminal-test.el ends here
