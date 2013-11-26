;;; init-paredit.el --- ParEdit configuration

;; Make ParEdit work with ElDoc
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; Make ParEdit work with delete-selection mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; Turn ParEdit mode on for selected modes
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(defvar paredit-minbuf-commands '(eval-expression
                                  pp-eval-expression
                                  eval-expression-with-eldoc
                                  ibuffer-do-eval
                                  ibuffer-do-view-and-eval)
  "Interactive commands for which ParEdit should be enabled in the minibuffer.")

(defun paredit-minbuf ()
  "Enable ParEdit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minbuf-commands)
      (paredit-mode)))

;; Use ParEdit in the minibuffer
(add-hook 'minibuffer-setup-hook 'paredit-minbuf)

(defun paredit-slime-fix ()
  "Fix ParEdit conflict with SLIME."
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

;; Use ParEdit with SLIME
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'paredit-slime-fix)

(provide 'init-paredit)

;;; init-paredit.el ends here
