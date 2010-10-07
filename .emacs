;; Load path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(load "manoj-colors.el")
(setq color-theme-is-global t)
(color-theme-manoj-dark)

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Message buffer size
(setq message-log-max 100)

;; Show column number in modeline
(setq column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode t)

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
(auto-compression-mode 1)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-separator ":")
(setq uniquify-buffer-name-style 'post-forward)

;; ERC
(require 'erc)

;; Easy PG
(require 'epa-file)

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

;; Lua mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

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

;; Yasnippet
(add-to-list 'load-path
	       "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
; save abbrevs when files are saved
(setq save-abbrevs t)
; load abbrevs on startup
(quietly-read-abbrev-file)
; always on
(setq default-abbrev-mode t)