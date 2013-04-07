;;
;; drot-keybindings.el - Custom key bindings configuration
;;

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Helm find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Helm Imenu
(global-set-key (kbd "M-i") 'helm-imenu)

(provide 'drot-keybindings)
;; drot-keybindings.el ends here
