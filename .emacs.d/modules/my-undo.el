;;
;; my-undo.el - Undo Tree configuration
;;

;; Store Undo Tree history in the tmp directory
(setq undo-tree-history-directory-alist
      `((".*" . ,my-tmp-dir)))

;; Enable Undo Tree history
(setq undo-tree-auto-save-history t)

;; Enable Undo Tree
(global-undo-tree-mode)

(provide 'my-undo)
;; my-undo.el ends here
