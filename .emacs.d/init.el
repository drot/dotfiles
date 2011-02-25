;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Color theme
(require 'zenburn)
(color-theme-zenburn)

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Message buffer size
(setq message-log-max 100)

;; Show column number in modeline
(setq column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; IDO
(require 'ido)
(ido-mode t)
(setq
  ido-save-directory-list-file "~/.emacs.d/emacs-ido-last"
  ido-ignore-buffers               ; ignore buffers:
    '("\\` " "^\*Back" "^\*Compile-Log" ".*Completion" "^\*Ido")
  ido-everywhere t                 ; enabled for various dialogs
  ido-case-fold  t                 ; case-insensitive
  ido-use-filename-at-point nil    ; don't use filename at point
  ido-use-url-at-point nil         ; don't use url at point
  ido-enable-flex-matching t       ; more flexible
  ido-max-prospects 6              ; keep minibuffer clean
)

;; Edit files in compressed archives
(auto-compression-mode t)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-separator ":")
(setq uniquify-buffer-name-style 'post-forward)

;; ERC
(require 'erc)

;; Easy PG
(require 'epa-file)

;; Magit
(require 'magit)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Ediff
;; Don't spawn a new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split the frame horizontally
(setq ediff-split-window-function 'split-window-horizontally)

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

;; Default major mode
(setq default-major-mode 'text-mode)

;; Wrap lines at 70 in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Haskell mode
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Calendar
(setq
 mark-holidays-in-calendar t
 all-christian-calendar-holidays t
 all-islamic-calendar-holidays nil
 all-hebrew-calendar-holidays nil
 european-calendar-style t
 display-time-24hr-format t
 calendar-latitude 43.20
 calendar-longitude 17.48
 calendar-location-name "Mostar, Bosnia and Herzegovina"
)

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
; save abbrevs when files are saved
(setq save-abbrevs t)
; load abbrevs on startup
(quietly-read-abbrev-file)
; always on
(setq default-abbrev-mode t)
