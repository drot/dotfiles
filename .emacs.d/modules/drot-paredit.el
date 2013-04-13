;;
;; drot-paredit.el - ParEdit configuration
;;

(autoload 'enable-paredit-mode "paredit")

;; Turn ParEdit mode on for selected modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(defun override-slime-repl-bindings-with-paredit ()
  "Fix ParEdit conflict with SLIME."
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which ParEdit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable ParEdit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

;; Use ParEdit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(provide 'drot-paredit)
;; drot-paredit.el ends here
