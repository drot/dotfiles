;;; init-company.el --- Configuration for Company

;; Enable the following backends
(setq company-backends '(company-elisp 
                         company-nxml
                         company-css
                         company-semantic
                         company-cmake
                         company-gtags
                         company-etags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev))

;; No idle completion
(setq company-idle-delay 0.3)

;; Disable echo delay
(setq company-echo-delay 0)

;; Shorten Company lighter
(setq company-default-lighter " cmp")

;; Enable Company
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)

;;; init-company.el ends here
