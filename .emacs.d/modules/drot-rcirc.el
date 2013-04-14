;;
;; drot-rcirc.el - rcirc IRC client configuration
;;

;; Load rcirc
(require 'rcirc)

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
