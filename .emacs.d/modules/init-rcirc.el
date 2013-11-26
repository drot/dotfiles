;;; init-rcirc.el --- rcirc IRC client configuration

;; Load rcirc
(require 'rcirc)

;; Connection
(setq rcirc-server-alist
      '(("adams.freenode.net" :channels ("#archlinux" "#emacs")
         :port 7000 :encryption tls)))

;; Authorization
(when (file-exists-p "~/.ircpass")
  (load "~/.ircpass")
  (setq rcirc-authinfo
        `(("freenode" nickserv "drot" ,freenode-password)
          ("forestnet" nickserv "drot" ,freenode-password))))

;; Colorize nicks
(require 'rcirc-color)
(setq rcirc-colors '("#40b83e" "#afc4db" "#93e0e3"
                     "#dc8cc3" "#f6aa11" "#f6f080"
                     "#eb939a" "#65a4a4" "#ff4a52"))

;; Max line width and number
(setq rcirc-fill-flag t
      rcirc-fill-column 'frame-width
      rcirc-buffer-maximum-lines 1024)

;; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

;; Turn on Fly Spell mode
(add-hook 'rcirc-mode-hook 'flyspell-mode)

;; Turn on Omit mode
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

;; Track channel activity
(add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)

(provide 'init-rcirc)

;;; init-rcirc.el ends here
