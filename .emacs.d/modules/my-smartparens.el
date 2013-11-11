;;
;; my-smartparens.el - Configuration for smartparens
;;

;; Load the default configuration
(require 'smartparens-config)

;; Enable smartparens mode
(smartparens-global-mode t)

;; Highlight matching parens
(show-smartparens-global-mode t)

;; Enable smartparens strict mode for selected modes
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'ielm-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
(add-hook 'scheme-mode-hook 'smartparens-strict-mode)

(provide 'my-smartparens)
;; my-smartparens.el ends here
