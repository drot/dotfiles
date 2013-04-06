;;
;; drot-abbrev.el - Emacs abbreviation mode configuration
;;

;; Load abbrevs and enable abbrev-mode
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs t)
(quietly-read-abbrev-file)
(setq default-abbrev-mode t)

(provide 'drot-abbrev)
;; drot-abbrev.el ends here
