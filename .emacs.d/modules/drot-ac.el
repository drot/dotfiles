;;
;; drot-ac.el - Configuration for Auto Complete
;;

;; Enable Auto Complete
(require 'auto-complete-config)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" drot-saves-dir))
(ac-config-default)

;; Fix conflict with YASnippet
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(provide 'drot-ac)
;; drot-ac.el ends here
