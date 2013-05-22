;; my-rmail.el - Configuration for Rmail

;; Mail receiving
(setq rmail-primary-inbox-list '("po:drot:lavabit.com"))
(setq rmail-pop-password-required t)

;; Mail sending
(setq user-full-name "Davor Rotim")
(setq user-mail-address "drot@lavabit.com")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-smtp-server "lavabit.com"
      smtpmail-smtp-service 587)

;; Don't show the number of lines in each message
(setq rmail-summary-line-count-flag nil)

;; Always display the summary buffer
(setq rmail-display-summary t)

;; URL handling
(add-hook 'rmail-show-message-hook 'goto-address-mode)

(provide 'my-rmail)
;; my-rmail.el ends here
