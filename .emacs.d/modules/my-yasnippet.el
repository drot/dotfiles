;;
;; my-yasnippet.el - Configuration for YASnippet
;;

;; Enable YASnippet
(yas-global-mode 1)

;; Display fewer messages
(setq yas-verbosity 1)

;; Use Ido to select snippets
(setq yas-prompt-functions '(yas-ido-prompt))

(provide 'my-yasnippet)
;; my-yasnippet.el ends here
