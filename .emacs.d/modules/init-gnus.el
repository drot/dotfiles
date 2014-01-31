;;; init-gnus.el --- Configuration for Gnus

;; User information
(setq user-full-name "Davor Rotim")
(setq user-mail-address "mlinfo@cock.li")

;; Incoming mail
(setq gnus-select-method '(nnimap "cock"
				  (nnimap-address "mail.cock.li")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))

;; Outgoing mail
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.cock.li"
      smtpmail-smtp-service 587)

;; Message date format
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A %H:%M")
        ((gnus-seconds-month) . "%A %d")
        ((gnus-seconds-year) . "%B %d")
        (t . "%B %d '%y")))

;; Summary format
(setq gnus-summary-line-format
      (concat "%U%R %~(max-right 17)~(pad-right 17)&user-date; "
	      "%~(max-right 20)~(pad-right 20)n %B%s\n"))

(provide 'init-gnus)

;;; init-gnus.el ends here
