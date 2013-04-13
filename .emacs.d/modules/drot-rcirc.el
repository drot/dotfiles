;;
;; drot-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

; Colorize nicks and escape characters
(require 'rcirc-color)
(require 'rcirc-controls)

(defun activate-bottom-prompt ()
  "Keep prompt at bottom."
  (set (make-local-variable 'scroll-conservatively) 8192))

(defun activate-omit-mode ()
  "Turn rcirc Omit mode on."
  (rcirc-omit-mode))

(defun activate-track-mode ()
  "Turn rcirc Track mode on."
  (rcirc-track-minor-mode 1))

; Connection
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(when (file-exists-p "~/.ircpass")
    (load "~/.ircpass"))
(setq rcirc-authinfo
      `(("freenode" nickserv "drot" ,freenode-password)))

; Color palette for nicks
(setq rcirc-colors '("#dca3a3" "#dfaf8f" "#f0dfaf"
                     "#7f9f7f" "#93e0e3" "#cc9393"
                     "#d0bf8f" "#afd8af" "#5c888b"
                     "#dc8cc3" "#8cd0d3" "#94bff3"
                     "#9fc59f" "#e0cf9f" "#6ca0a3"))

; Max line width and number
(setq rcirc-fill-flag nil
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

; Keep prompt at bottom
(add-hook 'rcirc-mode-hook 'activate-bottom-prompt)

; Turn on spell checking
(add-hook 'rcirc-mode-hook 'activate-flyspell-mode)

; Turn on Omit mode
(add-hook 'rcirc-mode-hook 'activate-omit-mode)

; Track channel activity
(add-hook 'rcirc-mode-hook 'activate-track-mode)

(provide 'drot-rcirc)
;; drot-rcirc.el ends here
