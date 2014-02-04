;;; init-erc.el --- Configuration for ERC

;; Load ERC
(require 'erc)

;; Load modules
(add-to-list 'erc-modules 'notifications)
(add-to-list 'erc-modules 'scrolltobottom)
(add-to-list 'erc-modules 'smiley)

(defun my-erc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "adams.freenode.net" :port 6697
           :nick "drot")
  (erc-tls :server "pine.forestnet.org" :port 6697
           :nick "drot"))

;; Don't prompt for a password
(setq erc-prompt-for-password nil)

;; Auto join selected channels
(setq erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                    ("forestnet" "#reloaded" "#fo2")))

;; Always reconnect
(setq erc-server-reconnect-attempts t)
(setq erc-server-reconnect-timeout 10)

;; Enable Fly Spell mode
(erc-spelling-mode 1)

;; Truncate buffers
(erc-truncate-mode 1)

;; Static text fill
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-column 120)
(setq erc-fill-static-center 15)

(add-hook 'erc-mode-hook
          (defun fix-scrolling-bug ()
            "Keep the prompt at bottom"
            (set (make-local-variable 'scroll-conservatively) 1000)))

;; Hide IRC spam
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

;; Don't track the server buffer
(setq erc-track-exclude-server-buffer t)

;; Tracking options
(setq erc-track-showcount t)
(setq erc-track-switch-direction 'importance)
(setq erc-track-visibility 'selected-visible)

;; Show timestamp on the left side
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-only-if-changed-flag nil)

;; Timestamp format
(setq erc-timestamp-format "[%H:%M] ")

;; Interpret mIRC colors
(setq erc-interpret-mirc-color t)

;; Default nick uniquifer
(setq erc-nick-uniquifier "_")

;; Header line format
(setq erc-header-line-format "%t: %o")

;; Prompt format
(setq erc-prompt (lambda ()
                   (if erc-network
                       (concat "[" (symbol-name erc-network) "]")
                     (concat "[" (car erc-default-recipients) "]"))))

(provide 'init-erc)

;;; init-erc.el ends here
