;;; my-app-agent-test.el --- Tests for the guard on claude-code-ide's server  -*- lexical-binding: t; -*-

;;; Commentary:

;; claude-code-ide's WebSocket server listens on 127.0.0.1 with no token,
;; and my-app-agent.el refuses whatever reaches it from a web page.  The
;; guard is advice on one of the package's internal functions, so what can
;; go wrong is quiet: rename it upstream and the advice waits on a name
;; nothing calls, and the server is open again.  The first test is the one
;; that matters -- the name advised is still the package's.
;;
;; Against straight's build of the package, so it is skipped where straight
;; has not built it.

;;; Code:

(require 'ert)
(require 'my-test)
(require 'cl-lib)

(defconst my-app-agent-test--build
  (expand-file-name "straight/build" (or (getenv "STRAIGHT_DIR")
                                         (expand-file-name "~/.emacs.d")))
  "Where straight builds packages.")

(defvar my-app-agent-test--available
  (when (file-directory-p (expand-file-name "claude-code-ide" my-app-agent-test--build))
    (dolist (dir (directory-files my-app-agent-test--build t "\\`[^.]"))
      (add-to-list 'load-path dir t))
    (require 'claude-code-ide-mcp nil t))
  "Non-nil when claude-code-ide could be loaded.")

(defvar my-app-agent-test--loaded
  (my-test-load (my-test-module "my-app-agent") "claude-code-ide-refuse")
  "How many definitions were taken out of the module.")

(ert-deftest my-app-agent-test-advised-name-is-the-packages ()
  "The function advised still exists in claude-code-ide, and is advised."
  (skip-unless my-app-agent-test--available)
  (should (>= my-app-agent-test--loaded 2))
  (should (fboundp 'claude-code-ide-mcp--on-open))
  (should (advice-member-p #'my/claude-code-ide-refuse-browser-socket
                           'claude-code-ide-mcp--on-open)))

(ert-deftest my-app-agent-test-socket-from-a-page-is-closed ()
  "A WebSocket opened with an Origin is closed and never handed on."
  (skip-unless my-app-agent-test--available)
  (let (closed handed)
    (cl-letf (((symbol-function 'websocket-origin)
               (lambda (ws) (plist-get ws :origin)))
              ((symbol-function 'websocket-close)
               (lambda (ws) (setq closed ws))))
      (my/claude-code-ide-refuse-browser-socket
       (lambda (_session ws) (setq handed ws))
       'session '(:origin "https://example.com"))
      (should closed)
      (should-not handed)
      (setq closed nil)
      (my/claude-code-ide-refuse-browser-socket
       (lambda (_session ws) (setq handed ws))
       'session '(:origin nil))
      (should-not closed)
      (should handed))))

;; my-app-agent.el turns executeCode off by setting a variable of the
;; package's, and a variable renamed upstream fails the same quiet way: the
;; setting lands on a name nothing reads, and the tool is offered again.
(defvar claude-code-ide-enable-execute-code)

(ert-deftest my-app-agent-test-execute-code-follows-the-variable ()
  "executeCode is offered when the variable is non-nil, and only then."
  (skip-unless my-app-agent-test--available)
  (let ((claude-code-ide-enable-execute-code t))
    (should (assoc "executeCode" (claude-code-ide-mcp--build-tool-list))))
  (let ((claude-code-ide-enable-execute-code nil))
    (should-not (assoc "executeCode" (claude-code-ide-mcp--build-tool-list)))))

(provide 'my-app-agent-test)
;;; my-app-agent-test.el ends here
