;;
;; drot-helm.el - Configuration for Helm
;;

;; Helm
(require 'helm-config)
(helm-mode 1)

;; Helm describe bindings
(helm-descbinds-mode)
(setq helm-descbinds-window-style 'split-window)

;; Multiple regexp matching methods
(helm-match-plugin-mode t)

(provide 'drot-helm)
;; drot-helm.el ends here
