;; Load path
(add-to-list 'load-path "~/.emacs.d")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Color theme
(require 'color-theme-subdued)
(color-theme-initialize)
(color-theme-subdued)

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Shut off message buffer
(setq message-log-max nil)
(kill-buffer "*Messages*")

;; Show column number in modeline
(setq column-number-mode t)

;; Modeline setup
(setq default-mode-line-format
      '("-"
       mode-line-mule-info
       mode-line-modified
       mode-line-frame-identification
       mode-line-buffer-identification
       "  "
       global-mode-string
       "   %[(" mode-name mode-line-process minor-mode-alist "%n"")%]--"
       (line-number-mode "L%l--")
       (column-number-mode "C%c--")
       (-3 . "%p")
       "-%-")
)

;; Default Web Browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignore case on completion
(setq completion-ignore-case t
  read-file-name-completion-ignore-case t)

;; No backup
(setq make-backup-files nil)

;; No save 
(setq auto-save-list-file-name nil)

;; No auto save
(setq auto-save-default nil)

;; Haskell mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Key bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)