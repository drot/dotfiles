;;
;; drot emacs
;;

;; Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Color theme
(load-theme 'jazz t)

;; Show tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Message buffer size
(setq message-log-max 100)

;; Show column number in modeline
(setq column-number-mode t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Colored output fix
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; X clipboard copy and paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Edit files in compressed archives
(auto-compression-mode t)

;; Enable Easy PG
(require 'epa-file)

;; Ediff window placement
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-ignore-buffers-re "^\\*")

;; Save minibuffer history
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/.savehist")

;; Use Ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/ido-last"
      ido-ignore-buffers
      '("\\` " "^\*Back" "^\*Compile-Log" ".*Completion" "^\*Ido")
      ido-everywhere t
      ido-case-fold t
      ido-create-new-buffer 'prompt
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching t
      ido-max-prospects 6)

;; Smex
(require 'smex)
(setq smex-save-file "~/.emacs.d/smex-items")
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; Default major mode
(setq default-major-mode 'text-mode)

;; Wrap lines at 70 in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Haskell mode
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Change backup behavior to save in a specified directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/"))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      6
      kept-old-versions      2)

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
 calendar-location-name "Mostar, Bosnia and Herzegovina")

;; Abbreviations
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(setq default-abbrev-mode t)
