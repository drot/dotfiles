;;
;; drot-abbrev.el - Emacs abbreviation mode configuration
;;

;; Load abbrevs and enable Abbrev mode
(setq abbrev-file-name (expand-file-name "abbrev_defs" drot-saves-dir)
      save-abbrevs t)
(when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)

;; Hippie expand is an improved dabbrev expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(provide 'drot-abbrev)
;; drot-abbrev.el ends here
