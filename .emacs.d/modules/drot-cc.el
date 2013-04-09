;;
;; drot-cc.el - Configuration for the C/C++ mode
;;

;; Indentation style
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(defun drot-c-pair-mode ()
  "Turn on Electric Pair mode."
  (electric-pair-mode +1))

;; Enable Electric Pair mode for all related languages
(add-hook 'c-mode-common-hook 'drot-c-pair-mode)

(provide 'drot-cc)
;; drot-cc.el ends here
