;;
;; my-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

; Connection
(setq rcirc-server-alist
      '(("adams.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(if (file-exists-p "~/.ircpass")
    (load "~/.ircpass"))
(setq rcirc-authinfo
      `(("freenode" nickserv "drot" ,freenode-password)))

;; Colorize nicks
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (require 'rcirc-color)))
(setq rcirc-colors '("#8d4a4a" "#953331" "#ba5b34"
                     "#96a62d" "#909737" "#546a29"
                     "#7e9960" "#34676f" "#5c737c"
                     "#385e6b" "#7f355e"))

; Max line width and number
(setq rcirc-fill-flag nil
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

;; Wrap lines that are too long
(add-hook 'rcirc-mode-hook 'visual-line-mode)

; Turn on Fly Spell mode
(add-hook 'rcirc-mode-hook 'flyspell-mode)

; Turn on Omit mode
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

; Track channel activity
(add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)

(provide 'my-rcirc)
;; my-rcirc.el ends here
