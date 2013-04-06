;;
;; drot-editor.el - Emacs editing configuration
;;

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Enable Easy PG
(require 'epa-file)

;; Default major mode
(setq default-major-mode 'text-mode)

(provide 'drot-editor)
;; drot-editor.el ends here
