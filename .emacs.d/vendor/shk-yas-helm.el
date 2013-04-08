;;
;; shk-yas-helm.el - Select a snippet using Helm
;;

(defun yas-helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet."
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "User quit!")
          (cdr (assoc result rmap))))
    nil))

(provide 'shk-yas-helm)
;; shk-yas-helm.el ends here
