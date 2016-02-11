;; Configure news server
(setq gnus-select-method '(nnnil)
      gnus-secondary-select-methods '((nntp "news.gwene.org")))

;; Article fetching options
(setq gnus-article-browse-delete-temp t
      gnus-treat-strip-trailing-blank-lines 'last
      gnus-mime-display-multipart-related-as-mixed t)

;; Group topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Configure visible headers
(setq gnus-visible-headers
      "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

;; Show the article headers in this order
(setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))

;; Set return email address based on incoming email address
(setq gnus-posting-styles
      '(((header "to" "address@outlook.com")
         (address "address@outlook.com"))
        ((header "to" "address@gmail.com")
         (address "address@gmail.com"))))

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
