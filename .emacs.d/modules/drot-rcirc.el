;;
;; drot-rcirc.el - rcirc IRC client configuration
;;

(require 'rcirc)

; Colorize nicks and escape characters
(require 'rcirc-color)
(require 'rcirc-controls)

; Color palette for nicks
(setq rcirc-colors '("#dca3a3" "#dfaf8f" "#f0dfaf"
                     "#7f9f7f" "#93e0e3" "#cc9393"
                     "#d0bf8f" "#afd8af" "#5c888b"
                     "#dc8cc3" "#8cd0d3" "#94bff3"
                     "#9fc59f" "#e0cf9f" "#6ca0a3"))

; Connection
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(when (file-exists-p "~/.ircpass")
    (load "~/.ircpass"))
(setq rcirc-authinfo
      `(("freenode" nickserv "drot" ,freenode-password)))

; Max line width and number
(setq rcirc-fill-flag nil
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

; Turn on spell checking
(add-hook 'rcirc-mode-hook 'drot-enable-flyspell)

; Keep prompt at bottom
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
		 8192)))

; Turn on omit-mode
(add-hook 'rcirc-mode-hook '(lambda ()
			      (rcirc-omit-mode)))

; Track channel activity
(add-hook 'rcirc-mode-hook (lambda ()
			     (rcirc-track-minor-mode 1)))

(provide 'drot-rcirc)
;; drot-rcirc.el ends here
