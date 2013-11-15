;;
;; my-company.el - Configuration for Company
;;

;; Enable the following backends
(setq company-backends '(company-elisp 
                         company-nxml
                         company-css
                         company-eclim
                         company-semantic
                         company-cmake
                         company-gtags
                         company-etags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev))

;; Enable Company
(add-hook 'after-init-hook 'global-company-mode)

(provide 'my-company)
;; my-company.el ends here
