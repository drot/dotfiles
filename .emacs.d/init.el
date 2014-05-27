(defvar user/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defvar user/save-directory (expand-file-name "saves" user/emacs-directory)
  "This directory houses all save files.")
(make-directory user/save-directory t)

(defvar user/custom-file (expand-file-name "custom.el" user/save-directory)
  "Store changes from the customize interface in the selected file.")

;; Package repository selection and activation
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/"))
      package-enable-at-startup nil)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Turn off the menu bar
(menu-bar-mode 0)

;; Turn off the toolbar
(tool-bar-mode 0)

;; Turn off the scrollbar
(scroll-bar-mode 0)

;; Don't show the welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Configuration for backup files
(setq backup-directory-alist `((".*" . ,user/save-directory))
      auto-save-file-name-transforms `((".*" ,user/save-directory t))
      auto-save-list-file-prefix (expand-file-name ".saves-" user/save-directory)
      version-control t
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Show column number and buffer size on the modeline
(column-number-mode 1)
(size-indication-mode 1)

;; Show tooltips in echo area
(tooltip-mode 0)

;; Indicate buffer boundaries
(setq-default indicate-buffer-boundaries 'right
              indicate-empty-lines t)

;; Pretty lambda
(global-prettify-symbols-mode 1)

;; Keep point on same position when scrolling
(setq scroll-preserve-screen-position 1)

;; Enable X clipboard usage
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Color theme
(use-package alect-themes
  :ensure t
  :config
  (defadvice custom-theme-set-variables
      (around fix-inhibit-bug activate)
    "Allow setting of undefined variables in themes."
    (let (custom--inhibit-theme-enable)
      ad-do-it))
  (load-theme 'alect-black t))

;; Save minibuffer history
(use-package savehist
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "minbuf.hist" user/save-directory))
  (savehist-mode 1))

;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saved-places" user/save-directory))
  (setq-default save-place t))

;; Bookmarks save directory
(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" user/save-directory)
        bookmark-save-flag 1))

;; Eshell save directory
(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (expand-file-name "eshell" user/save-directory)))

;; Highlight matching parentheses
(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Delete a selection with a keypress
(use-package delsel
  :config
  (delete-selection-mode 1))

;; Scroll compilation buffer to first error
(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error))

;; Use Unified diff format
(use-package diff
  :defer t
  :config
  (setq diff-switches "-u"))

;; Ediff window split
(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;; TRAMP configuration
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist `((".*" . ,user/save-directory))
        tramp-auto-save-directory user/save-directory))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (setq gnutls-min-prime-bits 1024))

;; Use ANSI colors within shell-mode
(use-package shell
  :defer t
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; Load abbrevs and enable Abbrev Mode
(use-package abbrev
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user/save-directory)
        save-abbrevs t)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq-default abbrev-mode t))

;; Hippie expand is an improved dabbrev expand
(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

;; Fly Spell mode configuration
(use-package flyspell
  :config
  (setq ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english")
  (add-hook 'text-mode-hook 'flyspell-mode))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300
        doc-view-continuous t))

;; Open URLs in Conkeror
(use-package browse-url
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror"))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Icomplete
(use-package icomplete
  :config
  (setq icomplete-prospects-height 1)
  (icomplete-mode 1))

;; Company mode
(use-package company
  :ensure t
  :diminish "co"
  :config
  (setq company-echo-delay 0
        company-show-numbers t
        company-backends '(company-nxml company-css company-eclim company-semantic
                                        company-capf company-dabbrev-code company-etags
                                        company-keywords company-files company-dabbrev))
  (global-company-mode 1))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (defun user/c-mode-hook ()
    "C mode setup"
    (unless (or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
           (concat "gcc " (buffer-file-name) " -o "))))

  (defun user/c++-mode-hook ()
    "C++ mode setup"
    (unless (or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
           (concat "g++ " (buffer-file-name) " -o "))))

  (add-hook 'c-mode-hook 'user/c-mode-hook)
  (add-hook 'c++-mode-hook 'user/c++-mode-hook)
  (add-hook 'c-mode-common-hook 'auto-fill-mode)

  (setq c-basic-offset 4
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "stroustrup"))))

;; Lua mode
(use-package lua-mode
  :ensure t
  :defer t)

;; PKGBUILD mode
(use-package pkgbuild-mode
  :ensure t
  :defer t)

;; ParEdit
(use-package paredit
  :ensure t
  :diminish "PEd"
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)

  (defvar user/paredit-minbuf-commands '(eval-expression
                                         pp-eval-expression
                                         eval-expression-with-eldoc
                                         ibuffer-do-eval
                                         ibuffer-do-view-and-eval)
    "Interactive commands for which ParEdit should be enabled in the minibuffer.")

  (defun user/paredit-minbuf ()
    "Enable ParEdit during lisp-related minibuffer commands."
    (if (memq this-command user/paredit-minbuf-commands)
        (paredit-mode)))

  (add-hook 'minibuffer-setup-hook 'user/paredit-minbuf)

  (defun user/paredit-slime-fix ()
    "Fix ParEdit conflict with SLIME."
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))

  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'slime-repl-mode-hook 'user/paredit-slime-fix)

  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t))

;; Show documentation with ElDoc mode
(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Hide Show mode
(use-package hideshow
  :config
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode))

;; Skeleton mode configuration
(use-package skeleton
  :config
  (setq skeleton-further-elements '((abbrev-mode nil))))

(define-skeleton cpp-skeleton
  "C++ skeleton"
  nil
  "#include <iostream>\n"
  "\n"
  "int main ()\n"
  "{\n"
  > _
  "\n"
  > "return 0;"
  "\n}")

;; Undo Tree
(use-package undo-tree
  :ensure t
  :diminish "UT"
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,user/save-directory))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

;; ERC configuration
(defun irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "orwell.freenode.net" :port 6697
           :nick "drot")
  (erc-tls :server "pine.forestnet.org" :port 6697
           :nick "drot"))

(use-package erc
  :ensure erc-hl-nicks
  :defer t
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'smiley)

  (add-hook 'erc-mode-hook (lambda ()
                             (set (make-local-variable 'scroll-conservatively) 1000)))

  (add-hook 'erc-mode-hook (lambda ()
                             (erc-fill-mode 0)))

  (add-hook 'erc-mode-hook 'visual-line-mode)

  (erc-spelling-mode 1)

  (setq erc-prompt-for-password nil
        erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                      ("forestnet" "#fo2"))
        erc-server-reconnect-timeout 10
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        erc-track-exclude-server-buffer t
        erc-track-showcount t
        erc-track-switch-direction 'importance
        erc-track-visibility 'selected-visible
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "[%H:%M] "
        erc-header-line-format "%t: %o"
        erc-interpret-mirc-color t
        erc-button-buttonize-nicks nil
        erc-format-nick-function 'erc-format-@nick
        erc-nick-uniquifier "_"
        erc-show-my-nick nil
        erc-prompt (lambda ()
                     (concat (buffer-name) ">"))))

;; Calendar configuration
(use-package calendar
  :defer t
  :config
  (setq calendar-mark-holidays-flag t
        holiday-general-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        holiday-solar-holidays nil
        holiday-islamic-holidays nil
        holiday-hebrew-holidays nil
        calendar-date-style 'european
        calendar-latitude 43.20
        calendar-longitude 17.48
        calendar-location-name "Mostar, Bosnia and Herzegovina"))

;; Org mode configuration
(bind-keys*
 ("\C-cl" . org-store-link)
 ("\C-ca" . org-agenda))

(use-package org
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (sh . t)))
  (setq org-log-done 'time
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

;; Load changes from the customize interface
(setq custom-file user/custom-file)
(if (file-exists-p user/custom-file)
    (load user/custom-file))
