;;
;; drot-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

; Connection
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(if (file-exists-p "~/.ircpass")
    (load "~/.ircpass"))
(setq rcirc-authinfo
      `(("freenode" nickserv "drot" ,freenode-password)))

;; Colorize nicks
(require 'rcirc-color)
(setq rcirc-colors '("#953331" "#ba5b34" "#909737"
                     "#546a29" "#34676f" "#385e6b"
                     "#7f355e" "#96a62d" "#7e9960"
                     "#5c737c" "#8d4a4a"))

; Max line width and number
(setq rcirc-fill-flag nil
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

(defun my-rcirc-prompt ()
  "Keep prompt at bottom."
  (set (make-local-variable 'scroll-conservatively) 8192))

(add-hook 'rcirc-mode-hook 'my-rcirc-prompt)

;; Wrap lines that are too long
(add-hook 'rcirc-mode-hook 'visual-line-mode)

; Turn on spell checking
(add-hook 'rcirc-mode-hook 'flyspell-mode)

; Turn on Omit mode
(add-hook 'rcirc-mode-hook 'rcirc-omit-mode)

; Track channel activity
(add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)

(provide 'drot-rcirc)
;; drot-rcirc.el ends here
