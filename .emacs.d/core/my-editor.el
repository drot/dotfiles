;;
;; my-editor.el - Emacs editing configuration
;;

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

;; Parenthesis matching
(electric-pair-mode t)

;; Enable CUA mode for rectangular selection
(cua-selection-mode 1)

;; Delete a selection with a keypress
(delete-selection-mode 1)

;; Recognize CamelCase words
(global-subword-mode 1)

;; Scroll compilation buffer to first error
(setq compilation-scroll-output 'first-error)

;; Use Unified diff format
(setq diff-switches "-u")

;; Ediff window placement
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Load abbrevs and enable Abbrev mode
(setq abbrev-file-name (expand-file-name "abbrev_defs" my-saves-dir)
      save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)

;; Hippie expand is an improved dabbrev expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Shorten mode name
(diminish 'abbrev-mode "Abv")

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Show indicators for wrapped lines in Visual Line mode
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Org mode as default major mode
(setq-default major-mode 'org-mode)

;; Enable Fly Spell mode for text mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; Enable Fly Spell mode for comments and strings in source code
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable code folding with Hide Show mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Show documentation with ElDoc mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'my-editor)
;; my-editor.el ends here
