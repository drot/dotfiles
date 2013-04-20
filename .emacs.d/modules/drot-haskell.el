;;
;; drot-haskell.el - Configuration for Haskell mode
;;

;; Enable Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(provide 'drot-haskell)
;; drot-haskell.el ends here
