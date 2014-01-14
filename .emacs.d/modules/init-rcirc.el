;;; init-rcirc.el --- rcirc IRC client configuration

;; Load rcirc
(require 'rcirc)

;; Connection
(setq rcirc-server-alist
      '(("adams.freenode.net" :channels ("#archlinux" "#emacs")
         :port 7000 :encryption tls)
        ("pine.forestnet.org" :channels ("#reloaded" "#fo2")
         :port 6697 :encryption tls)))

;; Authorization
(when (file-exists-p "~/.ircpass")
  (load "~/.ircpass")
  (setq rcirc-authinfo
        `(("freenode" nickserv "drot" ,freenode-password)
          ("forestnet" nickserv "drot" ,freenode-password))))

;; Colorize nicks
(require 'rcirc-color)
(setq rcirc-colors '("#336c6c" "#538c8c" "#205070"
                     "#0f2050" "#806080" "#6c1f1c"
                     "#732f2c" "#23733c" "#935f5c"))

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
