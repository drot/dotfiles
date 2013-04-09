;;
;; drot-paredit.el - ParEdit configuration
;;

(defun drot-paredit-mode ()
  "Turn ParEdit mode on."
  (paredit-mode +1))

;; Turn ParEdit mode on for selected modes
(add-hook 'emacs-lisp-mode-hook 'drot-paredit-mode)
(add-hook 'lisp-mode-hook 'drot-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'drot-paredit-mode)
(add-hook 'scheme-mode-hook 'drot-paredit-mode)

(defun drot-paredit-slime ()
  "Fix ParEdit conflict with SLIME."
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

;; Turn ParEdit mode for the SLIME REPL
(add-hook 'slime-repl-mode-hook 'drot-paredit-slime)

(provide 'drot-paredit)
;; drot-paredit.el ends here
