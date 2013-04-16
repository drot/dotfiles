;;
;; drot-undo.el - Undo tree configuration
;;

;; Store Undo Tree history in the tmp directory
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Enable Undo tree history
(setq undo-tree-auto-save-history t)

;; Enable Undo tree
(global-undo-tree-mode)

;; Shorten mode name
(diminish 'undo-tree-mode "UTr")

(provide 'drot-undo)
;; drot-undo.el ends here
