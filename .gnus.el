;; User information
(setq user-full-name "Davor Rotim"
      user-mail-address "mlinfo@cock.li")

;; Incoming mail
(setq gnus-select-method '(nnimap "cock"
                                  (nnimap-address "mail.cock.li")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

;; Outgoing mail
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.cock.li"
      smtpmail-smtp-service 587)

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

;; Group format
(setq gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%g%)\n")

;; Summary format
(setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-thread-sort-functions '(gnus-thread-sort-by-date))

;; Summary tree format
;; Summary tree format
(setq gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-false-root "o "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-vertical "| "
      gnus-sum-thread-tree-single-leaf "`-> ")

;; Display images in messages
(setq gnus-blocked-images nil)

;; Display attached images
(setq mm-inline-large-images 'resize
      mm-attachment-override-types '("image/.*"))

;; Display GnuPG signatures
(setq gnus-buttonized-mime-types '("multipart/signed" "multipart/alternative")
      mm-verify-option 'always)

;; Prefer plain text over HTML
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
