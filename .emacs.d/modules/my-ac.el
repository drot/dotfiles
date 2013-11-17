;;
;; my-ac.el - Configuration for Auto Complete
;;

(require 'auto-complete-config)

;; Enable Auto Complete
(ac-config-default)

;; Change cache file location
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-saves-dir))

;; Enable fuzzy completion
(setq ac-fuzzy-enable t)

;; Default key for completion
(ac-set-trigger-key "TAB")

;; Don't start automatically
(setq ac-auto-start nil)

;; Make movement keys select candidates
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'my-ac)
;; my-ac.el ends here
