;;; my-files-proxy.el --- Optional proxy configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets `url-proxy-services` if `my/proxy-address` is defined.
;; Expected to be defined in `local.el`.

;;; Code:

(defvar my/proxy-address nil
  "Proxy address string in the form of 'host:port'. Should be set in local.el.")

(when (and (boundp 'my/proxy-address) my/proxy-address)
  (setq url-proxy-services
        `(("http" . ,my/proxy-address)
          ("https" . ,my/proxy-address)))
  (message "Proxy enabled (%s)" my/proxy-address))

(provide 'my-files-proxy)
;;; my-files-proxy.el ends here
