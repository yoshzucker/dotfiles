;;; my-editor-evil-test.el --- Tests for the hungry backspace in insert state  -*- lexical-binding: t; -*-

;;; Commentary:

;; my-editor-evil.el makes DEL hungry by remapping `delete-backward-char' to
;; `backward-delete-char-untabify' in insert state's own map, because evil
;; remaps the same command there to one of its own.  Two things are relied
;; on and can change upstream: that insert state's DEL still arrives as a
;; remap of `delete-backward-char', and that `.' repeats a hungry deletion
;; and `u' takes it back as one change.  The remap is restated here rather
;; than taken from the module: it is a `my/define-key' form, which the test
;; helper does not read.
;;
;; Against straight's build of evil, so it is skipped where straight has not
;; built it.

;;; Code:

(require 'ert)

(defconst my-editor-evil-test--build
  (expand-file-name "straight/build" (or (getenv "STRAIGHT_DIR")
                                         (expand-file-name "~/.emacs.d")))
  "Where straight builds packages.")

(defvar my-editor-evil-test--available
  (when (file-directory-p (expand-file-name "evil" my-editor-evil-test--build))
    (dolist (dir '("evil" "goto-chg"))
      (add-to-list 'load-path (expand-file-name dir my-editor-evil-test--build) t))
    (require 'evil nil t))
  "Non-nil when evil could be loaded.")

(defun my-editor-evil-test--text ()
  "The buffer's text, newlines shown as `|'."
  (replace-regexp-in-string "\n" "|" (buffer-substring-no-properties
                                      (point-min) (point-max))))

(defun my-editor-evil-test--before-bar ()
  "Move point to the next `bar'."
  (search-forward "bar")
  (goto-char (match-beginning 0)))

(ert-deftest my-editor-evil-test-insert-del-is-a-remap ()
  "Insert state still sends DEL through a remap of `delete-backward-char'."
  (skip-unless my-editor-evil-test--available)
  (should (lookup-key evil-insert-state-map [remap delete-backward-char])))

(ert-deftest my-editor-evil-test-hungry-del-repeats-and-undoes ()
  "DEL takes the run of whitespace; `.' repeats it, `u' takes it back whole."
  (skip-unless my-editor-evil-test--available)
  (let ((backward-delete-char-untabify-method 'all)
        (saved (lookup-key evil-insert-state-map [remap delete-backward-char])))
    (define-key evil-insert-state-map [remap delete-backward-char]
                #'backward-delete-char-untabify)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer (get-buffer-create " *my-editor-evil-test*")
            (switch-to-buffer (current-buffer))
            (text-mode)
            (buffer-enable-undo)
            (evil-local-mode 1)
            (evil-normal-state)
            (erase-buffer)
            (insert "foo   bar\nfoo   bar\n")
            (goto-char (point-min))
            (my-editor-evil-test--before-bar)
            (execute-kbd-macro (kbd "i DEL x <escape>"))
            (should (equal (my-editor-evil-test--text) "fooxbar|foo   bar|"))
            (forward-line 1)
            (my-editor-evil-test--before-bar)
            (execute-kbd-macro (kbd "."))
            (should (equal (my-editor-evil-test--text) "fooxbar|fooxbar|"))
            (undo-boundary)
            (execute-kbd-macro (kbd "u"))
            (should (equal (my-editor-evil-test--text) "fooxbar|foo   bar|"))
            (kill-buffer)))
      (define-key evil-insert-state-map [remap delete-backward-char] saved))))

(provide 'my-editor-evil-test)
;;; my-editor-evil-test.el ends here
