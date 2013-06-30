;;
;; my-diminish.el - Configuration for Diminish
;;

;; Shorten Abbrev mode
(eval-after-load 'abbrev '(diminish 'abbrev-mode "Abv"))

;; Shorten ParEdit
(eval-after-load 'paredit '(diminish 'paredit-mode "PEd"))

;; Shorten Undo Tree
(eval-after-load 'undo-tree '(diminish 'undo-tree-mode "UTr"))

(provide 'my-diminish)
;; my-diminish.el ends here
