;;; init-org.el --- Configuration for Org mode

;; Use Ido for completion
(setq org-completion-use-ido t)

;; Show timestamps
(setq org-log-done 'time)

;; Languages which can be evaluated
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (sh . t)))

;; Treat code blocks as regular code
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; Calendar
(setq calendar-mark-holidays-flag t
      holiday-general-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      holiday-islamic-holidays nil
      holiday-hebrew-holidays nil
      calendar-date-style 'european
      calendar-latitude 43.20
      calendar-longitude 17.48
      calendar-location-name "Mostar, Bosnia and Herzegovina")

(provide 'init-org)

;;; init-org.el ends here
