;;
;; drot-abbrev.el - Emacs abbreviation mode configuration
;;

;; Load abbrevs and enable abbrev-mode
(setq abbrev-file-name (expand-file-name "abbrev_defs" drot-saves-dir)
      save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)

(provide 'drot-abbrev)
;; drot-abbrev.el ends here
