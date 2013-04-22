;;
;; drot-org.el - Configuration for Org mode
;;

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

;; Use Ido for completion
(setq org-completion-use-ido t)

;; Show timestamps
(setq org-log-done 'time)

(provide 'drot-org)
;; drot-org.el ends here
