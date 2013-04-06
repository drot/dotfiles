;;
;; drot-helm.el - Configuration for Helm
;;

;; Helm
(require 'helm-config)
(helm-mode 1)

;; Replace commands with Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Helm describe bindings
(helm-descbinds-mode)
(setq helm-descbinds-window-style 'split-window)

;; Multiple regexp matching methods
(helm-match-plugin-mode t)

(provide 'drot-helm)
;; drot-helm.el ends here
