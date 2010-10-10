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

;; Summary look
(copy-face 'gnus-summary-high-unread 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'gnus-summary-normal-unread 'gnus-face7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-summary-normal-read 'gnus-face-8)
(setq gnus-face-8 'gnus-face-8)
(copy-face 'gnus-summary-normal-undownloaded 'gnus-face-9)
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
      gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "┏● " 
      gnus-sum-thread-tree-false-root " ○ "
      gnus-sum-thread-tree-single-indent " ● "
      gnus-sum-thread-tree-leaf-with-other "┣━━❯ " 
      gnus-sum-thread-tree-vertical "┃"
      gnus-sum-thread-tree-single-leaf "┗━━❯ ")