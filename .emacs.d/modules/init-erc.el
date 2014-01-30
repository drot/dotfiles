;;; init-erc.el --- Configuration for ERC

;; Load ERC
(require 'erc)

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "adams.freenode.net" :port 6697
           :nick "drot")
  (erc-tls :server "pine.forestnet.org" :port 6697
           :nick "drot"))

;; Auto identify
(when (file-exists-p (expand-file-name "~/.ercpass"))
  (load "~/.ercpass")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode (("drot" . ,freenode-password)))
          (ForestNet (("drot" . ,freenode-password))))))

;; Auto join selected channels
(setq erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                      ("forestnet" "#reloaded" "#fo2")))

;; Enable Fly Spell mode
(erc-spelling-mode 1)

;; Static text fill
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-column 100)
(setq erc-fill-static-center 15)

;; Hide IRC spam
(setq erc-hide-list '("JOIN" "PART" "QUIT" "AWAY"))

;; Show timestamp on the left side
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-only-if-changed-flag nil)

;; Interpret mIRC colors
(setq erc-interpret-mirc-color t)

;; Default nick uniquifer
(setq erc-nick-uniquifier "_")

;; Disable nick buttonization
(setq erc-button-buttonize-nicks nil)

;; Prompt format
(setq erc-prompt ">>")

(provide 'init-erc)

;;; init-erc.el ends here
