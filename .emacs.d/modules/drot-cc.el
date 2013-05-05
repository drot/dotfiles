;;
;; drot-cc.el - Configuration for the C/C++ mode
;;

;; Indentation style
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Enable Auto Fill mode
(add-hook 'c-mode-common-hook 'auto-fill-mode)

;; C-mode compile command
(add-hook 'c-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "gcc "
                           (buffer-file-name))))))

(provide 'drot-cc)
;; drot-cc.el ends here
