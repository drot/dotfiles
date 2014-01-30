;;; init-erc.el --- Configuration for ERC

;; Load ERC
(require 'erc)

;; Enable Fly Spell mode
(erc-spelling-mode 1)

;; Disable button mode
(erc-button-mode nil)

;; Hide IRC spam
(setq erc-hide-list '("JOIN" "PART" "QUIT" "AWAY"))

;; Interpret mIRC colors
(setq erc-interpret-mirc-color t)

;; Default nick uniquifer
(setq erc-nick-uniquifier "_")

;; Static text fill
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 15)

;; Prompt format
(setq erc-prompt ">>")

(provide 'init-erc)

;;; init-erc.el ends here
