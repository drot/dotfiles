;; Start server
(server-start)

;; Load path
(add-to-list 'load-path "~/.emacs.d")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(load "color-theme-ir-black.el")
(if window-system
(color-theme-ir-black))

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Shut off message buffer
(setq message-log-max nil)
(kill-buffer "*Messages*")

;; Show column number in modeline
(setq column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode t)

;; IDO
(require 'ido)
(ido-mode t)
(setq
  ido-save-directory-list-file "~/.emacs.d/emacs-ido-last"
  ido-ignore-buffers               ; Ignore buffers:
    '("\\` " "^\*Back" "^\*Compile-Log" ".*Completion" "^\*Ido")
  ido-everywhere t                 ; Enabled for various dialogs
  ido-case-fold  t                 ; Case-insensitive
  ido-use-filename-at-point nil    ; Don't use filename at point
  ido-use-url-at-point nil         ; Don't use url at point
  ido-enable-flex-matching t       ; More flexible
  ido-max-prospects 6              ; Keep minibuffer clean
)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignore case on completion
(setq completion-ignore-case t
  read-file-name-completion-ignore-case t)

;; Change backup behavior to save in a specified directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/"))
 backup-by-copying      t
 version-control        t
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
)

;; Default browser
(setq browse-url-browser-function 'browse-url-generic 
      browse-url-generic-program "conkeror")

;; Haskell mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)