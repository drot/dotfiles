;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/helm/")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Show tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Color theme
(load-theme 'manoj-dark t)

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Message buffer size
(setq message-log-max 100)

;; Show column number in modeline
(setq column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Haskell mode
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Easy PG
(require 'epa-file)

;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Edit files in compressed archives
(auto-compression-mode t)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-separator ":")
(setq uniquify-buffer-name-style 'post-forward)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Ediff
; don't spawn a new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
; split the frame horizontally
(setq ediff-split-window-function 'split-window-horizontally)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Change backup behavior to save in a specified directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/"))
 backup-by-copying      t
 version-control        t
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
)

;; Default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Default major mode
(setq default-major-mode 'text-mode)

;; Wrap lines at 70 in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; Colored output fix
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
