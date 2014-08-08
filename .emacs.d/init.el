(defconst drot/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defconst drot/cache-directory (expand-file-name "cache" drot/emacs-directory)
  "This directory houses all cache files.")
(make-directory drot/cache-directory t)

(defconst drot/custom-file (expand-file-name "custom.el" drot/cache-directory)
  "Store changes from the customize interface in the selected file.")

(defconst drot/yas-directory (expand-file-name "snippets" drot/emacs-directory)
  "This directory houses all snippets.")
(make-directory drot/yas-directory t)

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

;; Show tooltips in the echo area
(tooltip-mode 0)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Show column number and buffer size on the modeline
(column-number-mode 1)
(size-indication-mode 1)

;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;; Don't show the welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Enable recursive minibuffers and indicate depth
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Delete duplicates from minibuffer history
(setq history-delete-duplicates t)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Pretty lambdas
(global-prettify-symbols-mode 1)

;; Display read-only buffers in view mode
(setq view-read-only t
      view-inhibit-help-message t)

;; Configuration for backup files
(setq backup-directory-alist `((".*" . ,drot/cache-directory))
      auto-save-file-name-transforms `((".*" ,drot/cache-directory t))
      auto-save-list-file-prefix (expand-file-name ".saves-" drot/cache-directory)
      version-control t
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Set fallback font
(set-fontset-font "fontset-default" nil
                  (font-spec :size 16 :name "Symbola"))

;; Color theme
(use-package zenburn-theme
  :ensure t)

;; Icomplete
(use-package icomplete
  :config
  (progn
    (setq icomplete-prospects-height 1)
    (icomplete-mode 1)))

;; Save minibuffer history
(use-package savehist
  :config
  (progn
    (setq savehist-additional-variables '(search-ring regexp-search-ring)
          savehist-autosave-interval 60
          savehist-file (expand-file-name "minbuf.hist" drot/cache-directory))
    (savehist-mode 1)))

;; Remember point position in files
(use-package saveplace
  :config
  (progn
    (setq save-place-file (expand-file-name "saved-places" drot/cache-directory))
    (setq-default save-place t)))

;; Find file at point
(use-package ffap
  :config
  (progn
    (ffap-bindings)))

;; Highlight matching parentheses
(use-package paren
  :config
  (progn
    (setq show-paren-delay 0)
    (show-paren-mode 1)))

;; Highlight regexps interactively
(use-package hi-lock
  :config
  (progn
    (global-hi-lock-mode 1)))

;; Regexp builder
(use-package re-builder
  :defer t
  :config
  (progn
    (setq reb-re-syntax 'string)))

;; Enable rectangle selection with CUA mode
(use-package cua-base
  :config
  (progn
    (cua-selection-mode 1)))

;; Bookmarks save directory
(use-package bookmark
  :defer t
  :config
  (progn
    (setq bookmark-default-file (expand-file-name "bookmarks" drot/cache-directory)
          bookmark-save-flag 1)))

;; Eshell save directory
(use-package eshell
  :defer t
  :config
  (progn
    (setq eshell-directory-name (expand-file-name "eshell" drot/cache-directory))))

;; Shell mode configuration
(use-package shell
  :defer t
  :config
  (progn
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)))

;; Disable YASnippet in term mode
(use-package term
  :defer t
  :config
  (progn
    (add-hook 'term-mode-hook (lambda ()
                                (yas-minor-mode 0)))))

;; Use Unified diff format
(use-package diff
  :defer t
  :config
  (progn
    (setq diff-switches "-u")))

;; Ediff window split
(use-package ediff
  :defer t
  :config
  (progn
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)))

;; Compilation configuration
(use-package compile
  :defer t
  :config
  (progn
    (setq compilation-scroll-output 'first-error
          compilation-ask-about-save nil)))

;; TRAMP configuration
(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-default-method "ssh"
          tramp-backup-directory-alist `((".*" . ,drot/cache-directory))
          tramp-auto-save-directory drot/cache-directory)))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (progn
    (setq gnutls-min-prime-bits 1024)))

;; Calendar configuration
(use-package calendar
  :defer t
  :config
  (progn
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
          calendar-location-name "Mostar, Bosnia and Herzegovina")))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (progn
    (setq doc-view-resolution 300
          doc-view-continuous t)))

;; Org mode configuration
(use-package org
  :defer t
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (emacs-lisp . t)
       (sh . t)))
    (setq org-log-done 'time
          org-src-fontify-natively t
          org-src-tab-acts-natively t)))

;; Open URLs in Conkeror
(use-package browse-url
  :defer t
  :config
  (progn
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "conkeror")))

;; Load abbrevs and enable Abbrev Mode
(use-package abbrev
  :config
  (progn
    (setq abbrev-file-name (expand-file-name "abbrev_defs" drot/cache-directory)
          save-abbrevs t)
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    (setq-default abbrev-mode t)))

;; Fly Spell mode configuration
(use-package flyspell
  :config
  (progn
    (setq ispell-extra-args '("--sug-mode=ultra")
          ispell-dictionary "english")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (progn
    (defun drot/c-mode-hook ()
      "C mode setup"
      (unless (or (file-exists-p "makefile")
                  (file-exists-p "Makefile"))
        (set (make-local-variable 'compile-command)
             (concat "gcc " (buffer-file-name) " -o "))))

    (defun drot/c++-mode-hook ()
      "C++ mode setup"
      (unless (or (file-exists-p "makefile")
                  (file-exists-p "Makefile"))
        (set (make-local-variable 'compile-command)
             (concat "g++ " (buffer-file-name) " -o "))))

    (add-hook 'c-mode-hook 'drot/c-mode-hook)
    (add-hook 'c++-mode-hook 'drot/c++-mode-hook)
    (add-hook 'c-mode-common-hook 'auto-fill-mode)

    (setq c-basic-offset 4
          c-default-style '((java-mode . "java")
                            (awk-mode . "awk")
                            (other . "stroustrup")))))

;; ERC configuration
(defun irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "adams.freenode.net" :port 6697
           :nick "drot")
  (erc-tls :server "pine.forestnet.org" :port 6697
           :nick "drot"))

(use-package erc
  :ensure erc-hl-nicks
  :defer t
  :config
  (progn
    (add-to-list 'erc-modules 'notifications)
    (add-to-list 'erc-modules 'smiley)

    (defun drot/erc-fill-hook ()
      "Set fill width to be dynamic in ERC buffers."
      (save-excursion
        (walk-windows
         (lambda (w)
           (let ((buffer (window-buffer w)))
             (set-buffer buffer)
             (when (eq major-mode 'erc-mode)
               (setq erc-fill-column (- (window-width w) 2))))))))

    (defun drot/erc-mode-hook ()
      "Keep prompt at bottom and disable Company and YASnippet."
      (set (make-local-variable 'scroll-conservatively) 1000)
      (company-mode 0)
      (yas-minor-mode 0))

    (make-variable-buffer-local 'erc-fill-column)
    (add-hook 'window-configuration-change-hook 'drot/erc-fill-hook)
    (add-hook 'erc-mode-hook 'drot/erc-mode-hook)
    (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

    (erc-spelling-mode 1)

    (setq erc-prompt-for-password nil
          erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                        ("forestnet" "#reloaded" "#fo2"))
          erc-server-reconnect-timeout 10
          erc-lurker-hide-list '("JOIN" "PART" "QUIT" "AWAY")
          erc-truncate-buffer-on-save t
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
                       (concat (buffer-name) ">")))))

;; Lua mode
(use-package lua-mode
  :ensure t
  :defer t)

;; Magit
(use-package magit
  :ensure t
  :defer t)

;; ParEdit
(use-package paredit
  :ensure t
  :diminish "PE"
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'ielm-mode-hook 'paredit-mode)
    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
    (add-hook 'scheme-mode-hook 'paredit-mode)

    (defvar drot/paredit-minibuffer-commands '(eval-expression
                                               pp-eval-expression
                                               eval-expression-with-eldoc
                                               ibuffer-do-eval
                                               ibuffer-do-view-and-eval)
      "Interactive commands for which ParEdit should be enabled in the minibuffer.")

    (defun drot/paredit-minibuffer ()
      "Enable ParEdit during lisp-related minibuffer commands."
      (if (memq this-command drot/paredit-minibuffer-commands)
          (paredit-mode 1)))

    (add-hook 'minibuffer-setup-hook 'drot/paredit-minibuffer)

    (defun drot/paredit-slime-fix ()
      "Fix ParEdit conflict with SLIME."
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))

    (add-hook 'slime-repl-mode-hook 'paredit-mode)
    (add-hook 'slime-repl-mode-hook 'drot/paredit-slime-fix)))

;; Show documentation with ElDoc mode
(use-package eldoc
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)
    (eldoc-add-command 'paredit-backward-delete
                       'paredit-close-round)))

;; Hide Show mode
(use-package hideshow
  :config
  (progn
    (add-hook 'c-mode-common-hook 'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    (add-hook 'python-mode-hook 'hs-minor-mode)))

;; PKGBUILD mode
(use-package pkgbuild-mode
  :ensure t
  :defer t)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (progn
    (setq yas-verbosity 1)
    (yas-global-mode 1)))

;; Company mode
(use-package company
  :ensure t
  :diminish "co"
  :config
  (progn
    (setq company-echo-delay 0
          company-show-numbers t
          company-backends '(company-nxml
                             company-css
                             company-capf (company-dabbrev-code company-keywords)
                             company-files
                             company-dabbrev))
    (global-company-mode 1)))

;; Undo Tree
(use-package undo-tree
  :ensure t
  :diminish "UT"
  :config
  (progn
    (setq undo-tree-history-directory-alist `((".*" . ,drot/cache-directory))
          undo-tree-auto-save-history t)
    (global-undo-tree-mode 1)))

;; Custom keybindings
(bind-keys*
 ("C-c y" . company-yasnippet)
 ("C-c a" . org-agenda)
 ("C-c l" . org-store-link)
 ("M-/" . hippie-expand))

;; Load changes from the customize interface
(setq custom-file drot/custom-file)
(if (file-exists-p drot/custom-file)
    (load drot/custom-file))
