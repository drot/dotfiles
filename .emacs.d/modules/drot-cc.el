;;
;; drot-cc.el - Configuration for the C/C++ mode
;;

;; Indentation style
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Enable Auto Fill mode
(add-hook 'c-mode-common-hook 'auto-fill-mode)

(provide 'drot-cc)
;; drot-cc.el ends here
