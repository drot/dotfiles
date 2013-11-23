;;; editing.conf.el --- Emacs editing configuration

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Highlight matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Enable CUA mode for rectangular selection
(cua-selection-mode 1)

;; Delete a selection with a keypress
(delete-selection-mode 1)

;; Recognize CamelCase words
(global-subword-mode 1)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Enable Fly Spell mode for text mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; Enable code folding with Hide Show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Show documentation with ElDoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(provide 'editing.conf)

;;; editing.conf.el ends here
