;;; my-syntax-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides LSP configuration for various languages development in Emacs.

;;; Code:
(defconst my/language-tools
  '(("pyright-langserver" "Python" "uv tool install pyright")
    ("ruff"               "Python" "uv tool install ruff")
    ("clangd"             "C/C++"  "Xcode on macOS | scoop install llvm")
    ("clang-format"       "C/C++"  "brew install clang-format | scoop install llvm")
    ("xcrun"              "Swift"  "Xcode -- sourcekit-lsp and swift-format are inside it"))
  "The programs outside Emacs that the modes configured here look for.

One of each on PATH, and every checkout uses that one.  A language
server is not a dependency of the code it reads -- nothing imports
pyright -- so a copy per project is the same download repeated and an
editor that has to be told which is which.  `install_python_tools' in
bootstrap installs the Python half of this list.")

(defun my/language-tools-report ()
  "Say which of `my/language-tools' this machine has, and where the rest come from.

Nothing says so while editing, on purpose: a file opens whether or not
its toolchain is installed, `my/eglot-ensure-when-available' starts no
server it cannot find and apheleia skips a formatter it cannot find.
This is where to ask instead -- after a bootstrap, or on a machine set
up for one language and not another."
  (interactive)
  (with-current-buffer (get-buffer-create "*Language Tools*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (entry my/language-tools)
        (pcase-let ((`(,program ,language ,install) entry))
          (insert (format "%-10s %-21s %s\n" language program
                          (or (executable-find program)
                              (concat "-- " install)))))))
    (special-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defconst my/eglot-servers
  '((swift-mode     . "xcrun")
    (swift-ts-mode  . "xcrun")
    (python-mode    . "pyright-langserver")
    (python-ts-mode . "pyright-langserver")
    (c-mode         . "clangd")
    (c-ts-mode      . "clangd")
    (c++-mode       . "clangd")
    (c++-ts-mode    . "clangd"))
  "The program each mode's language server is started from.

Not the whole command, only the name to look for: `eglot-server-programs'
holds the rest, and for C it holds eglot's own entry, which picks between
clangd and ccls itself.")

(defun my/eglot-ensure-when-available ()
  "Start a language server for this buffer when there is one to start.

Opening a file should not go wrong because a toolchain is missing.
`eglot-ensure' on a mode whose server is not installed reports a failure
the reader can do nothing about at that moment -- and on a machine set up
for one language and not another, that is every file of the other kind.

Emacs Lisp, Common Lisp and R are absent from `my/eglot-servers' on
purpose.  Emacs is its own analyser for the first, and SLIME and ESS
answer for the other two from the live image or session -- xref included
-- which is better than a server reading the files from outside."
  (when-let* ((program (alist-get major-mode my/eglot-servers))
              ((executable-find program)))
    (eglot-ensure)))

(use-package eglot
  ;; Reached by the hooks below, which name `eglot-ensure' -- autoloaded, so
  ;; opening a file in one of these modes is what loads eglot.
  ;;
  ;; They are in `:init' because `:config' runs only once eglot is loaded,
  ;; and nothing else loads it.  Waiting on `(python swift-ts-mode)' waited
  ;; on both: a Python file loads `python' and never `swift-ts-mode', so
  ;; eglot stayed unloaded, these hooks unregistered, and no buffer got a
  ;; language server unless a Swift file had been opened first.
  ;;
  ;; `eglot-server-programs' stays in `:config' and is still in time: an
  ;; autoloaded function loads its file and runs the after-load forms before
  ;; its own body, so the table is filled before `eglot-ensure' reads it.
  :defer t
  :init
  (my/add-hook
   (:hook swift-mode-hook swift-ts-mode-hook
          c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook
          ;; The parent of python-mode and python-ts-mode both, and the one
          ;; pet runs on first -- so the executables found here are the
          ;; ones the project's own environment put there.
          python-base-mode-hook
          :func #'my/eglot-ensure-when-available))
  :config
  ;; Prevent eglot from hijacking imenu or other features
  (setq eglot-stay-out-of '(imenu))

  ;; Take the server's log lines however they are shaped.  eglot declares
  ;; this notification as `&key _type _message' with no `&allow-other-keys',
  ;; so a server that adds a field to it -- sourcekit-lsp sends `logName' --
  ;; makes the keyword parsing fail, once per line logged, while the handler
  ;; it failed to reach is a noop that discards them anyway.  Same shape
  ;; eglot gives `telemetry/event', and same specializers as the method it
  ;; replaces.
  (cl-defmethod eglot-handle-notification
    (_server (_method (eql window/logMessage)) &rest _any)
    "Discard the server's log lines, whatever fields they carry.")

  ;; Swift
  (dolist (mode '(swift-mode swift-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("xcrun" "sourcekit-lsp"))))

  ;; Python
  (dolist (mode '(python-mode python-ts-mode))
    (add-to-list 'eglot-server-programs
		         `(,mode . ("pyright-langserver" "--stdio"))))

  (setq-default eglot-workspace-configuration
                '((:pyright . (:useLibraryCodeForTypes t
                                                       :useTypeCheckingMode "strict"
                                                       :reportMissingImports t
                                                       :reportMissingTypeStubs t)))))
(use-package apheleia
  ;; Where code is written, and not where anything else is.  The global mode
  ;; turns it on in every buffer there is and leaves it to each one's major
  ;; mode to have no formatter; a hook says the same thing without loading a
  ;; formatter to open a text file.
  ;;
  ;; Everywhere, Windows included.  Nothing in it is Unix's: it runs a
  ;; formatter as a subprocess and swaps the result in without moving
  ;; point, and a formatter that is not installed is resolved with
  ;; `executable-find', logged, and let go of -- the buffer is saved
  ;; either way.
  :diminish apheleia-mode
  :defer t
  :init
  (my/add-hook
   (:hook prog-mode-hook
          :func #'apheleia-mode))
  :config
  ;; ruff rather than black and isort: one binary does both, and it is the
  ;; one `install_python_tools' puts on PATH.  Sorting first, then the
  ;; formatting -- apheleia runs a list in order.
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) '(ruff-isort ruff)))

  ;; swift-format is in the Xcode toolchain and not on PATH; `xcrun' is how
  ;; that toolchain is reached.  The bare `-' is what makes it read the
  ;; buffer from standard input, which is what apheleia hands it -- without
  ;; it the tool works and says it is deprecated, once per save.
  (when (eq system-type 'darwin)
    (setf (alist-get 'swift-format apheleia-formatters)
          '("xcrun" "swift-format" "-"))
    (dolist (mode '(swift-mode swift-ts-mode))
      (setf (alist-get mode apheleia-mode-alist) 'swift-format))))

(provide 'my-syntax-lsp)
;;; my-syntax-lsp.el ends here
