;;
;; drot-keybindings.el - Custom key bindings configuration
;;

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Imenu with Ido completion
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(provide 'drot-keybindings)
;; drot-keybindings.el ends here
