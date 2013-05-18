;; Gnus setup for GMail

;; User information
(setq user-mail-address "")
(setq user-full-name "")

;; Incoming mail
(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-authinfo-file "~/.authinfo.gpg")
				  (nnimap-stream ssl)))

;; Outgoing mail
(setq send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-smtp-server "smtp.gmail.com"
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

;; Summary tree format
(setq gnus-sum-thread-tree-false-root " ♽ "
      gnus-sum-thread-tree-single-indent "⚙ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "⚈ "
      gnus-sum-thread-tree-leaf-with-other "├─►"
      gnus-sum-thread-tree-single-leaf "└─►"
      gnus-sum-thread-tree-vertical "┆" )
