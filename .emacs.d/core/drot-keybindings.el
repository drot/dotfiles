;;
;; drot-keybindings.el - Custom key bindings configuration
;;

;; Use hippie-expand instead of dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Use Helm to find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Use Helm for M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Use Helm for buffer list
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(provide 'drot-keybindings)
;; drot-keybindings.el ends here
