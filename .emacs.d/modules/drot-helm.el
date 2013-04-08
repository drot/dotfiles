;;
;; drot-helm.el - Configuration for Helm
;;

;; Helm
(require 'helm-config)
(helm-mode 1)

;; Helm describe bindings
(setq helm-descbinds-window-style 'split-window)
(helm-descbinds-mode)

;; Multiple regexp matching methods
(helm-match-plugin-mode t)

(provide 'drot-helm)
;; drot-helm.el ends here
