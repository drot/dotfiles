;;
;; my-rcirc.el - rcirc IRC client configuration
;;

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
(setq rcirc-colors '("#aaeecc" "#ff8888" "#ff3333"
                     "#FF1F69" "#ccaaff" "#aaccff"
                     "#aadddd" "#ffffff" "#aaffaa"))

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

(provide 'my-rcirc)
;; my-rcirc.el ends here
