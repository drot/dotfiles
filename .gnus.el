;; User information
(setq user-full-name "Davor Rotim"
      user-mail-address "augerbow@gmail.com")

;; Incoming mail
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

;; Outgoing mail
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Sent Mail")

;; Set return email address based on incoming email address
(setq gnus-posting-styles
      '(((header "to" "address@outlook.com")
         (address "address@outlook.com"))
        ((header "to" "address@gmail.com")
         (address "address@gmail.com"))))

;; Confirm to send and kill the buffer after
(setq message-confirm-send t
      message-kill-buffer-on-exit t)

;; Change Gnus default directories
(setq gnus-directory "~/.gnus"
      message-directory "~/.gnus/mail"
      nnml-directory "~/.gnus/nnml-mail"
      gnus-article-save-directory "~/.gnus/saved"
      gnus-kill-files-directory "~/.gnus/scores"
      gnus-cache-directory "~/.gnus/cache")

;; Group topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Configure visible headers
(setq gnus-visible-headers
      "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

;; Show the article headers in this order
(setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))

;; Article fetching options
(setq gnus-article-browse-delete-temp t
      gnus-fetch-old-headers t
      gnus-treat-strip-trailing-blank-lines 'last
      gnus-mime-display-multipart-related-as-mixed t
      gnus-auto-select-first nil)

;; Display of the summary buffer
(setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
      gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
      gnus-group-line-format "%M%S%p%P%5y:%B %G\n" ;;"%B%(%g%)"
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
      gnus-sum-thread-tree-false-root "o "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "`-> "
      gnus-sum-thread-tree-vertical "| "
      smiley-style 'medium)
