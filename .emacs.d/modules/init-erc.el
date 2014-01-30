;;; init-erc.el --- Configuration for ERC

;; Load ERC
(require 'erc)

;; Enable Fly Spell mode
(erc-spelling-mode 1)

;; Static text fill
(setq erc-fill-function 'erc-fill-static)
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
