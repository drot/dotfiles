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
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Summary format
(setq gnus-summary-line-format "%U%R%z %(%&user-date; %-15,15f %* %B%s%)\n"
      gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-thread-sort-functions '(gnus-thread-sort-by-date)
      gnus-sum-thread-tree-false-root "┈┬──▷ "
      gnus-sum-thread-tree-single-indent " ● "
      gnus-sum-thread-tree-root "┌─▶ "
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-leaf-with-other "├┬─► "
      gnus-sum-thread-tree-single-leaf "╰┬─► "
      gnus-sum-thread-tree-indent " ")
