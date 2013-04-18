;;
;; drot-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

(defun regular-irc-prompt ()
  "Keep prompt at bottom."
  (set (make-local-variable 'scroll-conservatively) 8192))

; Connection
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels ("#archlinux" "#emacs"))))

; Authorization
(if (file-exists-p "~/.ircpass")
    (load "~/.ircpass"))
(setq rcirc-authinfo
      `(("freenode" nickserv "drot" ,freenode-password)))

; Max line width and number
(setq rcirc-fill-flag nil
      rcirc-buffer-maximum-lines 1024)

; Hide IRC spam
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

; Keep prompt at bottom
(add-hook 'rcirc-mode-hook 'regular-irc-prompt)

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
