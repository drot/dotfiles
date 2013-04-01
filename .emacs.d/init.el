;;
;; drot emacs
;;

;; Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Don't show the welcome message
(setq inhibit-startup-screen t
      initial-scratch-message nil
      gnus-inhibit-startup-message t)

;; Color theme
(load-theme 'zenburn t)

; Recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Show tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Message buffer size
(setq message-log-max 100)

;; Show column number in modeline
(setq column-number-mode t)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Change backup behavior to save in a specified directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/"))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      4
      kept-old-versions      2)

;; Save minibuffer history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;; X clipboard copy and paste
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; Enable Easy PG
(require 'epa-file)

;; rcirc
(require 'rcirc-config)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-separator ":"
      uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Ediff window placement
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Default major mode
(setq default-major-mode 'text-mode)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; CC mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs t)
(quietly-read-abbrev-file)
(setq default-abbrev-mode t)

;; Calendar
(setq mark-holidays-in-calendar t
      all-christian-calendar-holidays t
      all-islamic-calendar-holidays nil
      all-hebrew-calendar-holidays nil
      european-calendar-style t
      display-time-24hr-format t
      calendar-latitude 43.20
      calendar-longitude 17.48
      calendar-location-name "Mostar, Bosnia and Herzegovina")

;; Helm
(require 'helm-config)
(helm-mode 1)

; Replace commands with Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

; Helm describe bindings
(helm-descbinds-mode)
(setq helm-descbinds-window-style 'split-window)

; Multiple regexp matching methods
(helm-match-plugin-mode t)
