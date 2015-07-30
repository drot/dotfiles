(defvar drot/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defvar drot/cache-directory (expand-file-name "cache" drot/emacs-directory)
  "This directory houses all cache files.")
(make-directory drot/cache-directory t)

(defvar drot/custom-file (expand-file-name "custom.el" drot/emacs-directory)
  "Store changes from the customize interface in the selected file.")

;; Prefer newest version of a file
(setq load-prefer-newer t)

;; Bootstrap quelpa
(package-initialize)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; Install the following packages
(quelpa 'ace-window)
(quelpa 'avy)
(quelpa 'browse-kill-ring)
(quelpa 'company)
(quelpa 'counsel)
(quelpa 'expand-region)
(quelpa 'hydra)
(quelpa 'lispy)
(quelpa 'magit)
(quelpa 'multiple-cursors)
(quelpa 'rainbow-delimiters)
(quelpa 'rcirc-color)
(quelpa 'rcirc-notify)
(quelpa 'rcirc-styles)
(quelpa 'swiper)
(quelpa 'undo-tree)
(quelpa 'use-package)
(quelpa 'volatile-highlights)
(quelpa 'yasnippet)
(quelpa 'zenburn-theme)
(quelpa 'zop-to-char)

;; Load use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Disable unnecessary GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Color theme
(use-package zenburn-theme)

;; Show tooltips in the echo area
(tooltip-mode 0)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Show column number and buffer size on the modeline
(column-number-mode)
(size-indication-mode)

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

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Increase default fill width
(setq-default fill-column 80)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Set fallback font
(defun drot/fix-emojis (&optional frame)
  (set-fontset-font "fontset-default" nil "Symbola" frame 'append))

(drot/fix-emojis)

(add-hook 'after-make-frame-functions 'drot/fix-emojis)

;; Do not save duplicates
(setq history-delete-duplicates t
      kill-do-not-save-duplicates t)

;; Configuration for backup files
(setq auto-save-file-name-transforms `((".*" ,drot/cache-directory t))
      auto-save-list-file-prefix (expand-file-name ".saves-" drot/cache-directory)
      backup-directory-alist `((".*" . ,drot/cache-directory))
      version-control t
      kept-new-versions 2
      delete-old-versions t
      backup-by-copying t)

;; Display read-only buffers in view mode
(setq view-read-only t
      view-inhibit-help-message t)

;; Replace dabbrev-expand with hippie-expand
(bind-key "M-/" 'hippie-expand)

;; Pretty lambdas
(global-prettify-symbols-mode)

;; Allow scrolling during Isearch
(setq isearch-allow-scroll t)

;; Save minibuffer history
(use-package savehist
  :config
  (setq savehist-file (expand-file-name "saved-history" drot/cache-directory)
        savehist-autosave-interval 60
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        kill-ring))
  (savehist-mode))

;; Save recent files list
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recent-files" drot/cache-directory)
        recentf-exclude (append recentf-exclude '("autoloads.el"))
        recentf-max-saved-items 100
        recentf-max-menu-items 20)
  (recentf-mode))

;; Highlight matching parentheses
(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode))

;; Highlight regexps interactively
(use-package hi-lock
  :config
  (global-hi-lock-mode))

;; Which function mode
(use-package which-func
  :config
  (setq which-func-unknown "n/a")
  (which-function-mode))

;; Indicate minibuffer recursion depth
(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode))

;; Undo and redo the window configuration
(use-package winner
  :config
  (winner-mode))

;; Ispell configuration
(use-package ispell
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; Fly Spell mode configuration
(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; Hide Show mode
(use-package hideshow
  :config
  (dolist (hook '(c-mode-common-hook
                  emacs-lisp-mode-hook
                  python-mode-hook))
    (add-hook hook 'hs-minor-mode)))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Dired-x
(use-package dired-x)

;; Regexp builder
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

;; Bookmarks save directory
(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" drot/cache-directory)
        bookmark-save-flag 1))

;; Eshell save directory
(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (expand-file-name "eshell" drot/cache-directory)))

;; Shell mode configuration
(use-package shell
  :defer t
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode))

;; Disable YASnippet in term mode
(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode 0))))

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

;; Compilation configuration
(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error
        compilation-ask-about-save nil))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4)
  (setcar (nthcdr 2 c-default-style) '(other . "k&r"))

  (add-hook 'c-mode-common-hook 'auto-fill-mode)
  (add-hook 'c-mode-common-hook 'electric-pair-mode))

;; TRAMP configuration
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp" drot/cache-directory)
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory drot/cache-directory))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (setq gnutls-min-prime-bits 1024))

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
        calendar-latitude 43.2
        calendar-longitude 17.48
        calendar-location-name "Mostar, Bosnia and Herzegovina"))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300
        doc-view-continuous t))

;; Open URLs in Firefox
(use-package browse-url
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-firefox))

;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saved-places" drot/cache-directory))
  (setq-default save-place t))

;; Load abbrevs and enable Abbrev Mode
(use-package abbrev
  :diminish "Abr"
  :config
  (setq abbrev-file-name (expand-file-name "abbrevs" drot/cache-directory)
        save-abbrevs t)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq-default abbrev-mode t))

;; Ace-window
(use-package ace-window
  :bind ("C-c o" . ace-window)
  :config
  (setq aw-dispatch-always t))

;; Avy
(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("C-+" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

;; Browse kill ring
(use-package browse-kill-ring
  :bind ("C-c y" . browse-kill-ring))

;; Company mode
(use-package company
  :diminish "co"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-c c" . company-yasnippet)
  :config
  (setq company-echo-delay 0
        company-show-numbers t
        company-backends '(company-nxml
                           company-css
                           company-capf (company-dabbrev-code company-keywords)
                           company-files
                           company-dabbrev)))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Magit
(use-package magit
  :bind ("C-c g" . magit-status))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" drot/cache-directory)))

;; Org-mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (setq org-log-done 'time
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

;; rcirc configuration
(use-package rcirc
  :defer t
  :config
  (setq rcirc-server-alist
        '(("adams.freenode.net" :port 7000 :encryption tls
           :channels ("#archlinux" "#emacs"))
          ("pine.forestnet.org" :port 6697 :encryption tls
           :channels ("#reloaded" "#rawhide"))))

  (when (file-exists-p "~/.private.el")
    (setq drot/credentials-file "~/.private.el")

    (defun drot/nickserv-password ()
      (with-temp-buffer
        (insert-file-contents-literally drot/credentials-file)
        (plist-get (read (buffer-string)) :nickserv-password)))

    (setq rcirc-authinfo
          `(("freenode" nickserv "drot" ,(drot/nickserv-password))
            ("forestnet" nickserv "drot" ,(drot/nickserv-password)))))

  (setq rcirc-fill-flag t
        rcirc-fill-column 'frame-width)

  (use-package rcirc-styles
    :config
    (setq rcirc-styles-color-vector ["#7F7F7F" "#CC9393" "#7F9F7F" "#D0BF8F"
                                     "#6CA0A3" "#DC8CC3" "#93E0E3" "#DCDCCC"
                                     "#9F9F9F" "#DCA3A3" "#BFEBBF" "#F0DFAF"
                                     "#8CD0D3" "#DC8CC3" "#93E0E3" "#FFFFEF"]))

  (use-package rcirc-color
    :config
    (setq rcirc-colors (append rcirc-styles-color-vector nil)))

  (use-package rcirc-notify
    :config
    (rcirc-notify-add-hooks))

  (defun drot/rcirc-mode-hook ()
    "Disable company and YASnippet in rcirc buffers."
    (company-mode 0)
    (yas-minor-mode 0))

  (add-hook 'rcirc-mode-hook 'drot/rcirc-mode-hook)

  (add-hook 'rcirc-mode-hook 'flyspell-mode)
  (add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode))

;; Hydra
(use-package hydra
  :bind (("C-c m" . multiple-cursors-hydra/body)
         ("C-c w" . hydra-window-resize/body))
  :config
  (defhydra multiple-cursors-hydra (:columns 3)
    "Multiple Cursors"
    ("m" mc/mark-next-like-this "Mark Next")
    ("s" mc/skip-to-next-like-this "Skip Next")
    ("u" mc/unmark-next-like-this "Unmark Next")
    ("M" mc/mark-previous-like-this "Mark Previous")
    ("S" mc/skip-to-previous-like-this "Skip Previous")
    ("U" mc/unmark-previous-like-this "Unmark Previous")
    ("l" mc/edit-lines "Edit lines" :exit t)
    ("a" mc/mark-all-like-this "Mark all" :exit t)
    ("q" nil "Quit"))

  (defhydra hydra-window-resize (:columns 2)
    "Resize Windows"
    ("j" enlarge-window "Enlarge Window")
    ("k" shrink-window "Shrink Window")
    ("h" shrink-window-horizontally "Shrink Window Horizontally")
    ("l" enlarge-window-horizontally "Enlarge Window Horizontally")
    ("b" balance-windows "Balance Windows" :exit t)
    ("q" nil "Quit")))

;; Swiper and ivy
(use-package swiper
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-c s" . swiper)
         ("C-c f" . ivy-recentf)
         ("C-c C-r" . ivy-resume)))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function))
  :config
  (setq counsel-find-file-at-point t))

;; Lispy
(use-package lispy
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'lispy-mode))

  (defvar drot/lispy-minibuffer-commands '(eval-expression
                                           pp-eval-expression
                                           eval-expression-with-eldoc
                                           ibuffer-do-eval
                                           ibuffer-do-view-and-eval)
    "Interactive commands for which lispy should be enabled in the minibuffer.")

  (defun drot/lispy-minibuffer ()
    "Enable lispy during lisp-related minibuffer commands."
    (if (memq this-command drot/lispy-minibuffer-commands)
        (lispy-mode)))

  (add-hook 'minibuffer-setup-hook 'drot/lispy-minibuffer))

;; Show documentation with ElDoc mode
(use-package eldoc
  :diminish "ElD"
  :config
  (dolist (hook '(eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook
                  emacs-lisp-mode-hook
                  ielm-mode-hook))
    (add-hook hook 'eldoc-mode)))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))

;; Volatile Highlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

;; Undo Tree
(use-package undo-tree
  :diminish "UT"
  :config
  (setq undo-tree-history-directory-alist backup-directory-alist
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; YASnippet
(use-package yasnippet
  :init
  (make-directory "~/.emacs.d/snippets" t)
  :config
  (setq yas-verbosity 1)
  (yas-global-mode))

;; Zop-to-char
(use-package zop-to-char
  :bind ("M-z" . zop-to-char))

;; Load changes from the customize interface
(setq custom-file drot/custom-file)
(when (file-exists-p drot/custom-file)
  (load-file drot/custom-file))
