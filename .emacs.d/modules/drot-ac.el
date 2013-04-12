;;
;; drot-ac.el - Configuration for Auto Complete
;;

;; Enable Auto Complete
(require 'auto-complete-config)
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" drot-saves-dir))
(ac-config-default)

(provide 'drot-ac)
;; drot-ac.el ends here
