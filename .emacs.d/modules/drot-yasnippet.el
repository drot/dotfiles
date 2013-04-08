;;
;; drot-yasnippet.el - Configuration for YASnippet
;;

;; Use Helm to select snippets
(require 'shk-yas-helm)
(setq yas-prompt-functions '(yas-helm-prompt))

;; Enable YASnippet
(yas-global-mode 1)

(provide 'drot-yasnippet)
;; drot-yasnippet.el ends here
