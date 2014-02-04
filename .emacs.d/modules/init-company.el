;;; init-company.el --- Configuration for Company mode

;; Disable the following backends
(setq company--disabled-backends '(company-eclim
                                   company-clang
                                   company-xcode
                                   company-ropemacs
                                   company-oddmuse))

;; Disable echo delay
(setq company-echo-delay 0)

;; Show quick-access numbers
(setq company-show-numbers t)

;; Shorten Company lighter
(setq company-default-lighter " co")

;; Enable Company mode
(global-company-mode)

(provide 'init-company)

;;; init-company.el ends here
