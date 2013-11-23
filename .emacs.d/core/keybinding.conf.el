;;; keybinding.conf.el --- Custom key bindings configuration

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replace dabbrev-expand with hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Swap Isearch with regexp Isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(provide 'keybinding.conf)

;;; keybinding.conf.el ends here
