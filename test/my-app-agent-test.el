;;; my-app-agent-test.el --- Tests for the guard on claude-code-ide's servers  -*- lexical-binding: t; -*-

;;; Commentary:

;; claude-code-ide's two servers listen on 127.0.0.1 with no token, and
;; my-app-agent.el refuses whatever reaches them from a web page.  The guard
;; is advice on two of the package's internal functions, so what can go
;; wrong is quiet: rename one upstream and the advice waits on a name
;; nothing calls, and the servers are open again.  The first test is the
;; one that matters -- the names advised are still the package's.
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
    (and (require 'claude-code-ide-mcp nil t)
         (require 'claude-code-ide-mcp-http-server nil t)))
  "Non-nil when claude-code-ide could be loaded.")

(defvar my-app-agent-test--loaded
  (my-test-load (my-test-module "my-app-agent") "claude-code-ide-refuse")
  "How many definitions were taken out of the module.")

(ert-deftest my-app-agent-test-advised-names-are-the-packages ()
  "The two functions advised still exist in claude-code-ide, and are advised."
  (skip-unless my-app-agent-test--available)
  (should (>= my-app-agent-test--loaded 4))
  (dolist (pair '((claude-code-ide-mcp--on-open
                   . my/claude-code-ide-refuse-browser-socket)
                  (claude-code-ide-mcp-http-server--handle-post
                   . my/claude-code-ide-refuse-browser-request)))
    (should (fboundp (car pair)))
    (should (advice-member-p (cdr pair) (car pair)))))

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

(ert-deftest my-app-agent-test-request-from-a-page-is-forbidden ()
  "An MCP request with an Origin is answered 403 and never handled."
  (skip-unless my-app-agent-test--available)
  (let (status handled)
    (cl-letf (((symbol-function 'ws-response-header)
               (lambda (_process code &rest _) (setq status code)))
              ((symbol-function 'ws-send) #'ignore))
      (catch 'close-connection
        (my/claude-code-ide-refuse-browser-request
         (lambda (_request) (setq handled t))
         (make-instance 'ws-request :headers '((:ORIGIN . "https://example.com")))))
      (should (eq status 403))
      (should-not handled)
      (my/claude-code-ide-refuse-browser-request
       (lambda (_request) (setq handled t))
       (make-instance 'ws-request :headers '((:HOST . "127.0.0.1"))))
      (should handled))))

(provide 'my-app-agent-test)
;;; my-app-agent-test.el ends here
