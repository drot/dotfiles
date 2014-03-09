;; Define directories
(defvar my-emacs-dir (file-name-directory load-file-name)
  "The root directory of the Emacs distribution.")

(defvar my-saves-dir (expand-file-name "saves" my-emacs-dir)
  "This directory houses all save files.")

(defvar my-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid))
  "This directory houses all backup and auto-save files.")

;; Define customize file
(defvar my-custom-file (expand-file-name "custom.el" my-saves-dir)
  "Store changes from the customize interface in the selected file.")

;; Package repository selection and activation
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Turn off the menu bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Turn off the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Turn off the scrollbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-frame-font "Envy Code R-10")

;; Don't show the welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Store all backup and auto-save files in the tmp directory
(setq backup-directory-alist
      `((".*" . ,my-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-tmp-dir t)))
(setq auto-save-list-file-prefix my-tmp-dir)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Show tooltips in echo area
(setq tooltip-use-echo-area t)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Show column number and buffer size on the modeline
(column-number-mode t)
(size-indication-mode t)

;; Disable point blink
(blink-cursor-mode 0)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Keep point on same position when scrolling
(setq scroll-preserve-screen-position 1)

;; Enable X clipboard usage
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Enable CUA mode for rectangular selection
(cua-selection-mode 1)

;; Delete a selection with a keypress
(delete-selection-mode 1)

;; Recognize CamelCase words
(global-subword-mode 1)

;; Make buffer names unique
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Save minibuffer history
(use-package savehist
  :init
  (progn
    (setq savehist-additional-variables '(search-ring regexp-search-ring)
          savehist-autosave-interval 60
          savehist-file (expand-file-name "minbuf.hist" my-saves-dir)))
  (savehist-mode t))

;; Remember point position in files
(use-package saveplace
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name "saved-places" my-saves-dir)))

;; Bookmarks save directory
(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" my-saves-dir)
        bookmark-save-flag 1))

;; Eshell save directory
(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (expand-file-name "eshell" my-saves-dir)))

;; Highlight matching parentheses
(use-package paren
  :init
  (show-paren-mode 1)
  :config
  (setq show-paren-delay 0))

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

;; TRAMP default file transfer method
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (setq gnutls-min-prime-bits 1024))

;; Use ANSI colors within shell-mode
(use-package shell
  :defer t
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; Load abbrevs and enable Abbrev Mode
(use-package abbrev
  :init
  (progn
    (setq abbrev-file-name (expand-file-name "abbrev_defs" my-saves-dir)
          save-abbrevs t)
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    (setq-default abbrev-mode t)))

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

;; Enable code folding with Hide Show mode
(use-package hideshow
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Show documentation with ElDoc mode
(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :init
  (progn
    (defun my-c-mode-hook ()
      "C mode setup"
      (unless (or (file-exists-p "makefile")
                  (file-exists-p "Makefile"))
        (set (make-local-variable 'compile-command)
             (concat "gcc " (buffer-file-name) " -o "))))

    (add-hook 'c-mode-hook 'my-c-mode-hook)

    (defun my-c++-mode-hook ()
      "C++ mode setup"
      (unless (or (file-exists-p "makefile")
                  (file-exists-p "Makefile"))
        (set (make-local-variable 'compile-command)
             (concat "g++ " (buffer-file-name) " -o "))))

    (add-hook 'c++-mode-hook 'my-c++-mode-hook)
    (add-hook 'c-mode-common-hook 'auto-fill-mode))
  :config
  (setq c-basic-offset 4
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "stroustrup"))))

;; Fly Spell mode configuration
(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "english"))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300
        doc-view-continuous t))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Swap Isearch with regexp Isearch
(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

;; Open URLs in the selected browser
(use-package browse-url
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

;; Saner regex syntax
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

;; Enable Ido for completion
(use-package ido
  :init
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 5
          ido-save-directory-list-file (expand-file-name "ido.hist" my-saves-dir)
          ido-default-file-method 'selected-window
          ido-everywhere t)
    (ido-mode 1)))

;; ERC configuration
(use-package erc
  :init
  (progn
    (defun my-erc ()
      "Connect to IRC."
      (interactive)
      (erc-tls :server "adams.freenode.net" :port 6697
               :nick "drot")
      (erc-tls :server "pine.forestnet.org" :port 6697
               :nick "drot"))

    (add-to-list 'erc-modules 'notifications)
    (add-to-list 'erc-modules 'scrolltobottom)
    (add-to-list 'erc-modules 'smiley)
    (erc-spelling-mode 1)
    
    (add-hook 'erc-mode-hook
              (defun fix-scrolling-bug ()
                "Keep the prompt at bottom"
                (set (make-local-variable 'scroll-conservatively) 1000))))
  :config
  (setq erc-prompt-for-password nil
        erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                      ("forestnet" "#reloaded" "#fo2"))
        erc-server-reconnect-timeout 10
        erc-fill-function 'erc-fill-static
        erc-fill-column 120
        erc-fill-static-center 15
        erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        erc-track-exclude-server-buffer t
        erc-track-showcount t
        erc-track-switch-direction 'importance
        erc-track-visibility 'selected-visible
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-only-if-changed-flag nil
        erc-timestamp-format "[%H:%M] "
        erc-interpret-mirc-color t
        erc-nick-uniquifier "_"
        erc-header-line-format "%t: %o"
        erc-prompt (lambda ()
                     (if erc-network
                         (concat "[" (symbol-name erc-network) "]")
                       (concat "[" (car erc-default-recipients) "]")))))

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
(use-package org
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (sh . t)))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda))
  :config
  (setq org-completion-use-ido t
        org-log-done 'time
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

;; Skeleton mode configuration
(use-package skeleton
  :init
  (define-skeleton my-cpp-skel
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
  :config
  (setq skeleton-further-elements '((abbrev-mode nil))))

;; -- Color theme ---

(use-package naquadah-theme
  :ensure t)

;; --- company mode ---

(use-package company
  :ensure t
  :diminish "co"
  :init
  (global-company-mode)
  :config
  (setq company-echo-delay 0
        company-show-numbers t
        company--disabled-backends '(company-eclim
                                     company-clang
                                     company-xcode
                                     company-ropemacs
                                     company-oddmuse)))

;; --- ERC Highlight Nicks ---

(use-package erc-hl-nicks
  :ensure t)

;; --- Ido Hacks ---

(use-package ido-hacks
  :ensure t
  :init
  (ido-hacks-mode t))

;; --- Magit ---

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

;; --- ParEdit ---

(use-package paredit
  :ensure t
  :diminish "PEd"
  :init
  (progn    
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'ielm-mode-hook 'paredit-mode)
    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'paredit-mode)    

    (defvar paredit-minbuf-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
      "Interactive commands for which ParEdit should be enabled in the minibuffer.")

    (defun paredit-minbuf ()
      "Enable ParEdit during lisp-related minibuffer commands."
      (if (memq this-command paredit-minbuf-commands)
          (paredit-mode)))

    (add-hook 'minibuffer-setup-hook 'paredit-minbuf)

    (defun paredit-slime-fix ()
      "Fix ParEdit conflict with SLIME."
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))

    (add-hook 'slime-repl-mode-hook 'paredit-mode)
    (add-hook 'slime-repl-mode-hook 'paredit-slime-fix))
  :config
  (progn
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-open-round 'delete-selection t)
    (put 'paredit-open-square 'delete-selection t)
    (put 'paredit-doublequote 'delete-selection t)
    (put 'paredit-newline 'delete-selection t)))

;;; --- Rainbow Delimiters ---

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; --- Undo Tree ---

(use-package undo-tree
  :ensure t
  :diminish "UT"
  :init
  (progn
    (setq undo-tree-history-directory-alist `((".*" . ,my-tmp-dir))
          undo-tree-auto-save-history t)
    (global-undo-tree-mode)))

;; Load changes from the customize interface
(if (file-exists-p my-custom-file)
    (load my-custom-file))
