;;
;; my-ac.el - Configuration for Auto Complete
;;

(require 'auto-complete-config)
(require 'auto-complete-clang-async)

(defun my-ac-setup ()
  "Change some defaults."
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-saves-dir))
  (ac-set-trigger-key "TAB"))

(defun my-ac-cc-setup ()
  "Enable Clang completion for CC mode."
  (setq ac-sources (append '(ac-source-clang-async) ac-sources))
  (ac-clang-launch-completion-process))

(defun my-ac-init ()
  "Start Auto Complete for selected modes."
  (setq-default ac-sources '(ac-source-abbrev
                             ac-source-dictionary
                             ac-source-filename
                             ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'my-ac-cc-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'my-ac-setup)
  (global-auto-complete-mode t))

;; Enable Auto Complete
(my-ac-init)

(provide 'my-ac)
;; my-ac.el ends here
