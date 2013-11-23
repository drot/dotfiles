;;; abbrev.conf.el --- Abbrev Mode configuration

;; Load abbrevs and enable Abbrev Mode
(setq abbrev-file-name (expand-file-name "abbrev_defs" my-saves-dir)
      save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)

;; Hippie expand is an improved dabbrev expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(provide 'abbrev.conf)

;;; abbrev.conf.el ends here
