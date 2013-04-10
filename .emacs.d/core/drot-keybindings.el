;;
;; drot-keybindings.el - Custom key bindings configuration
;;

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Imenu with Ido completion
(global-set-key (kbd "M-i") 'ido-goto-symbol)

(provide 'drot-keybindings)
;; drot-keybindings.el ends here
