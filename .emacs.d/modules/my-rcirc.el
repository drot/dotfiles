;;
;; my-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

; Connection
(setq rcirc-server-alist
      '(("adams.freenode.net" :channels ("#archlinux" "#emacs")
         :port 7000 :encryption tls)
        ("pine.forestnet.org" :channels ("#2238")
         :port 6697 :encryption tls)))

; Authorization
(if (file-exists-p "~/.ircpass")
    (load "~/.ircpass")
  (setq rcirc-authinfo
        `(("freenode" nickserv "drot" ,freenode-password)
          ("forestnet" nickserv "drot" ,freenode-password))))

;; Colorize nicks
(require 'rcirc-color)
(setq rcirc-colors '("#F92672" "#A6E22E" "#FD971F"
                     "#E6DB74" "#66D9EF" "#F3ECB0"
                     "#CDF187" "#FC87B0" "#FEA7F7"))

; Max line width and number
(setq rcirc-fill-flag t
      rcirc-fill-column 'frame-width
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

; Turn on Fly Spell mode
(add-hook 'rcirc-mode-hook 'flyspell-mode)

; Turn on Omit mode
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

; Track channel activity
(add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)

(provide 'my-rcirc)
;; my-rcirc.el ends here
