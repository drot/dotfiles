;;; init-company.el --- Configuration for Company

;; Disable the following backends
(setq company--disabled-backends '(company-eclim
                                   company-clang
                                   company-xcode
                                   company-ropemacs
                                   company-oddmuse))

;; Disable echo delay
(setq company-echo-delay 0)

;; Shorten Company lighter
(setq company-default-lighter " cmp")

;; Enable Company
(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)

;;; init-company.el ends here
