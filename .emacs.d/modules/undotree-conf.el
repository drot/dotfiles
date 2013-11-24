;;; undotree-conf.el --- Undo Tree configuration

;; Store Undo Tree history in the tmp directory
(setq undo-tree-history-directory-alist
      `((".*" . ,my-tmp-dir)))

;; Enable Undo Tree history
(setq undo-tree-auto-save-history t)

;; Shorten Undo Tree mode lighter
(setq undo-tree-mode-lighter " UT")

;; Enable Undo Tree
(global-undo-tree-mode)

(provide 'undotree-conf)

;;; undotree-conf.el ends here
