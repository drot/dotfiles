;; Don't show the splash screen
(setq gnus-inhibit-startup-message t)

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

;; Change Gnus default directories
(setq gnus-directory "~/.gnus")
(setq message-directory "~/.gnus/mail")
(setq nnml-directory "~/.gnus/nnml-mail")
(setq gnus-article-save-directory "~/.gnus/saved")
(setq gnus-kill-files-directory "~/.gnus/scores")
(setq gnus-cache-directory "~/.gnus/cache")

;; Group topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Group format
(setq gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%g%)\n")

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
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-false-root "o ")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "| ") 
(setq gnus-sum-thread-tree-single-leaf "`-> ")

;; Kill the message buffer after sending a message
(setq message-kill-buffer-on-exit t)

;; Display images in messages
(setq gnus-blocked-images nil)

;; Display attached images
(setq mm-inline-large-images 'resize)
(setq mm-attachment-override-types '("image/.*"))

;; Display GnuPG signatures
(setq gnus-buttonized-mime-types '("multipart/signed" "multipart/alternative"))
(setq mm-verify-option 'always)

;; Prefer plain text over HTML
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
