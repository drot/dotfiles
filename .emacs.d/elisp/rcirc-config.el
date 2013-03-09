; drot rcirc
(require 'rcirc)

; Connection
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(setq rcirc-authinfo
      '(("freenode" nickserv "drot" "")))

; Colorize nicks
(eval-after-load 'rcirc '(require 'rcirc-color))

; Color palette
(setq rcirc-colors '("#dca3a3" "#dfaf8f" "#f0dfaf"
		     "#7f9f7f" "#93e0e3"
		     "#d0bf8f" "#d0bf8f"
		     "#afd8af" "#5c888b"
		     "#dc8cc3" "#8cd0d3"))

; Max line width and number
(setq rcirc-fill-column 'frame-width
      rcirc-buffer-maximum-lines 2000)

; Keep prompt at bottom
(add-hook 'rcirc-mode-hook (lambda ()
			     (set (make-local-variable 'scroll-conservatively)
				  8192)))

; Turn on spell checking
(add-hook 'rcirc-mode-hook (lambda ()
			     (flyspell-mode 1)))

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT"))
(add-hook 'rcirc-mode-hook '(lambda ()
			      (rcirc-omit-mode)))

; Track channel activity
(add-hook 'rcirc-mode-hook (lambda ()
			     (rcirc-track-minor-mode 1)))

(provide 'rcirc-config)
