;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2016 drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/master/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Collection of Emacs configuration options I've accumulated over the years.

;;; Code:

;; Set some variables
(defconst drot/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defconst drot/cache-directory (expand-file-name "cache" drot/emacs-directory)
  "This directory houses all cache files.")
(make-directory drot/cache-directory t)

(defconst drot/custom-file (expand-file-name "custom.el" drot/emacs-directory)
  "Store changes from the customize interface in the selected file.")

;; Prefer newest version of a file
(setq load-prefer-newer t)

;; Disable site default settings
(setq inhibit-default-init t)

;; Activate packages and add MELPA
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Load changes from the customize interface
(setq custom-file drot/custom-file)
(load drot/custom-file 'noerror 'nomessage)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Bug Hunter
(use-package bug-hunter
  :ensure t
  :bind (("C-c a h" . bug-hunter-init-file)
         ("C-c a H" . bug-hunter-file)))

;; Color theme
(use-package zenburn-theme
  :ensure t)

;; Disable unnecessary GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

;; Don't show startup welcome messages
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-screen t)

;; Disable scratch buffer info text
(setq initial-scratch-message nil)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p #'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Don't use dialogs for minibuffer input
(setq use-dialog-box nil)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Cycle completion on smaller number of candidates
(setq completion-cycle-threshold 5)

;; Try completion after indentation
(setq tab-always-indent 'complete)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Increase default fill width
(setq-default fill-column 80)

;; Require a final new line
(setq require-final-newline t)

;; Kill and yank clipboard options
(setq select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Display read-only buffers in view mode
(setq view-read-only t
      view-inhibit-help-message t)

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

;; Diminish already loaded modes
(diminish 'visual-line-mode " WP")
(diminish 'auto-fill-function " FL")

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
        recentf-exclude '("/\\.git/.*\\'"
                          "/elpa/.*\\'"
                          "/cache/.*\\'"
                          ".*\\.gz\\'")
        recentf-max-saved-items 100
        recentf-max-menu-items 20
        recentf-auto-cleanup 600)
  (recentf-mode))

;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saved-places" drot/cache-directory))
  (save-place-mode))

;; Indicate minibuffer recursion depth
(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode))

;; Highlight matching parentheses
(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode))

;; Highlight regexps interactively
(use-package hi-lock
  :config
  (global-hi-lock-mode))

;; Abbrev Mode
(use-package abbrev
  :diminish (abbrev-mode . "AV")
  :config
  (setq abbrev-file-name (expand-file-name "abbrevs" drot/cache-directory)
        save-abbrevs t)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq-default abbrev-mode t))

;; Electric pair mode
(use-package elec-pair
  :config
  (electric-pair-mode))

;; Electric quote mode
(use-package electric
  :config
  (electric-quote-mode))

;; Pretty lambdas
(use-package prog-mode
  :config
  (setq prettify-symbols-unprettify-at-point t)
  (global-prettify-symbols-mode))

;; Which function mode
(use-package which-func
  :config
  (setq which-func-unknown "n/a")
  (which-function-mode))

;; Highlight current line
(use-package hl-line
  :config
  (global-hl-line-mode))

;; Undo and redo the window configuration
(use-package winner
  :config
  (winner-mode))

;; Allow scrolling during Isearch
(use-package "isearch"
  :defer t
  :diminish (isearch-mode . "IS")
  :config
  (setq isearch-allow-scroll t))

;; Ispell configuration
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; Ediff window split
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; Uniquify buffer names
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;;; Version control
(use-package vc-hooks
  :defer t
  :config
  (setq vc-follow-symlinks t))

;; Customize interface options
(use-package cus-edit
  :defer t
  :config
  (setq custom-buffer-done-kill t
        custom-buffer-verbose-help nil
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

;; Python mode configuration
(use-package python
  :defer t
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)

  (let ((ipython (executable-find "ipython")))
    (when ipython
      (setq python-shell-interpreter ipython))))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r")))
  (add-hook 'c-mode-common-hook #'auto-fill-mode))

;; NXML mode configuration
(use-package nxml-mode
  :defer t
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300
        doc-view-continuous t))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (setq gnutls-min-prime-bits 1024))

;; Dired
(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-ahlF"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t))

;; Dired-x
(use-package dired-x
  :after dired
  :config
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-verbose nil))

;; Find file at point
(use-package ffap
  :defer t
  :config
  (setq ffap-machine-p-known 'reject))

;; TRAMP configuration
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp" drot/cache-directory)
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory drot/cache-directory))

;; Outline mode
(use-package outline
  :diminish (outline-minor-mode . "OM")
  :commands outline-minor-mode
  :init
  (add-hook 'text-mode-hook #'outline-minor-mode))

;; Hide Show mode
(use-package hideshow
  :commands hs-minor-mode
  :init
  (dolist (hook '(c-mode-common-hook
                  emacs-lisp-mode-hook
                  python-mode-hook))
    (add-hook hook #'hs-minor-mode)))

;; Bug References
(use-package bug-reference
  :commands (bug-reference-mode bug-reference-prog-mode)
  :init
  (add-hook 'text-mode-hook #'bug-reference-mode)
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode))

;; Goto Address mode
(use-package goto-addr
  :commands (goto-address-mode goto-address-prog-mode)
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Fly Spell mode configuration
(use-package flyspell
  :diminish (flyspell-mode . "FS")
  :bind ("C-c x f" . flyspell-buffer)
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil))

;; Indent region
(bind-key "C-c x i" #'indent-region)

;; Cycle spacing
(bind-key [remap just-one-space] #'cycle-spacing)

;; Display personal bindings
(bind-key "C-c h b" #'describe-personal-keybindings)

;; Toggle debug on error
(bind-key "C-c t d" #'toggle-debug-on-error)

;; Revert buffer
(use-package files
  :bind ("C-c f z" . revert-buffer))

;; Proced
(use-package proced
  :bind ("C-x p" . proced))

;; Ediff
(use-package ediff
  :bind ("C-c f e" . ediff))

;; EWW
(use-package eww
  :bind (("C-c a w" . eww)))

;; Gnus
(use-package gnus
  :bind ("C-c a g" . gnus))

;; Wind Move
(use-package windmove
  :bind (("C-c w <left>" . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>" . windmove-up)
         ("C-c w <down>" . windmove-down)))

;; Find function and variable definitions
(use-package find-func
  :bind (("C-c h f" . find-function)
         ("C-c h 4 f" . find-function-other-window)
         ("C-c h k" . find-function-on-key)
         ("C-c h v" . find-variable)
         ("C-c h 4 v" . find-variable-other-window)
         ("C-c h l" . find-library)))

;; Whitespace mode
(use-package whitespace
  :diminish (whitespace-mode . "WS")
  :bind (("C-c x w" . whitespace-cleanup)
         ("C-c t w" . whitespace-mode)))

;; Tildify mode
(use-package tildify
  :bind ("C-c x t" . tildify-region))

;; Align
(use-package align
  :bind ("C-c x a" . align))

;; Auto Insert
(use-package auto-insert
  :bind ("C-c i a" . auto-insert))

;; Copyright
(use-package copyright
  :bind ("C-c i r" . copyright-update)
  :config
  (setq copyright-year-ranges t
        copyright-names-regexp (regexp-quote user-login-name)))

;; Comment region
(use-package newcomment
  :bind (("C-c c k" . comment-region)
         ("C-c c u" . uncomment-region)))

;; Replace dabbrev-expand with hippie-expand
(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

;; Search more extensively with apropos
(use-package apropos
  :bind ("C-c h a" . apropos)
  :config
  (setq apropos-do-all t))

;; Regexp builder
(use-package re-builder
  :bind ("C-c a r" . re-builder)
  :config
  (setq reb-re-syntax 'string))

;; Open URLs in Firefox
(use-package browse-url
  :bind (("C-c a b" . browse-url)
         ("C-c n b" . browse-url-at-point))
  :config
  (setq browse-url-browser-function #'browse-url-firefox))

;; Bookmarks save directory
(use-package bookmark
  :bind ("C-c f b" . list-bookmarks)
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" drot/cache-directory)
        bookmark-save-flag 1))

;; Eshell save directory
(use-package eshell
  :bind ("C-c a e" . eshell)
  :config
  (setq eshell-directory-name (expand-file-name "eshell" drot/cache-directory)))

;; Shell mode configuration
(use-package shell
  :bind ("C-c a s" . shell)
  :config
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook #'compilation-shell-minor-mode))

;; ANSI term
(use-package term
  :bind ("C-c a a" . ansi-term)
  :config
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode 0))))

;; Compilation configuration
(use-package compile
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
  :config
  (setq compilation-scroll-output 'first-error
        compilation-ask-about-save nil)

  (defun drot/colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'drot/colorize-compilation-buffer))

;; Colorize ANSI escape sequences
(use-package ansi-color
  :after compile)

;; Calendar configuration
(use-package calendar
  :bind ("C-c a c" . calendar)
  :config
  (setq calendar-week-start-day 1
        calendar-mark-holidays-flag t
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

;; World time
(use-package time
  :bind ("C-c a t" . display-time-world)
  :config
  (setq display-time-world-list '(("Europe/Riga" "Riga")
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("Canada/Eastern" "Quebec")
                                  ("Asia/Saigon" "Saigon"))))

;; Org-mode
(use-package org
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o t" . org-todo-list)
         ("C-c o s" . org-search-view)
         ("C-c o l" . org-store-link))
  :config
  (setq org-directory (expand-file-name "org" drot/emacs-directory)
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done 'time
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

;; Ace-window
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-c s a" . avy-isearch)
         ("C-c n c" . avy-goto-char)
         ("C-c n j" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c j" . avy-goto-word-1)
         ("C-c n w" . avy-goto-word-0)))

;; Browse Kill Ring
(use-package browse-kill-ring
  :ensure t
  :bind ("C-c i y" . browse-kill-ring))

;; Dash
(use-package dash
  :ensure t
  :defer t
  :config
  (dash-enable-font-lock))

;; Discover My Major
(use-package discover-my-major
  :ensure t
  :bind ("C-c h m" . discover-my-major))

;; Easy-kill
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-c x e" . er/expand-region))

;; Flx
(use-package flx
  :ensure t
  :defer t)

;; Geiser
(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename (expand-file-name "geiser-history" drot/cache-directory)))

;; JavaScript mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;; Key Chord
(use-package key-chord
  :ensure t
  :commands key-chord-mode
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "3j" #'dired-jump)
  (key-chord-define-global "3l" #'avy-goto-line)
  (key-chord-define-global "3u" #'undo-tree-visualize)
  (key-chord-define-global "8w" #'avy-goto-word-1)
  (key-chord-define-global "8c" #'avy-goto-char)
  (key-chord-define-global "8f" #'counsel-find-file)
  (key-chord-define-global "8q" #'query-replace)
  (key-chord-define-global "8r" #'replace-string)
  (key-chord-define-global "8x" #'counsel-M-x)
  (key-chord-define-global "8y" #'browse-kill-ring))

;; Lua mode
(use-package lua-mode
  :ensure t
  :defer t)

;; Hydra
(use-package hydra
  :ensure t
  :bind (("C-c w r" . hydra-window-resize/body)
         ("C-c x o" . hydra-outline/body))
  :config
  (hydra-add-font-lock)

  (defhydra hydra-window-resize (:columns 2)
    "Resize Windows"
    ("j" enlarge-window "Enlarge Window")
    ("k" shrink-window "Shrink Window")
    ("l" enlarge-window-horizontally "Enlarge Window Horizontally")
    ("h" shrink-window-horizontally "Shrink Window Horizontally")
    ("q" nil "Quit"))

  (defhydra hydra-outline (:columns 4)
    "Outline Mode"
    ("q" hide-sublevels "Hide Sub-levels")
    ("t" hide-body "Hide Body")
    ("o" hide-other "Hide Other")
    ("c" hide-entry "Hide Entry")
    ("l" hide-leaves "Hide Leaves")
    ("d" hide-subtree "Hide Sub-tree")
    ("a" show-all "Show All")
    ("e" show-entry "Show Entry")
    ("i" show-children "Show Children")
    ("k" show-branches "Show Branches")
    ("s" show-subtree "Show Sub-tree")
    ("u" outline-up-heading "Up Heading")
    ("n" outline-next-visible-heading "Next Visible Heading")
    ("p" outline-previous-visible-heading "Previous Visible Heading")
    ("f" outline-forward-same-level "Forward Same Level")
    ("b" outline-backward-same-level "Backward Same Level")
    ("z" nil "Quit")))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c v v" . magit-status)
         ("C-c v c" . magit-clone)
         ("C-c v b" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull)))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-hook 'markdown-mode-hook #'whitespace-mode)
  (add-hook 'markdown-mode-hook #'tildify-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;; Move-text
(use-package move-text
  :ensure t
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; NeoTree
(use-package neotree
  :ensure t
  :bind ("C-c t n" . neotree-toggle)
  :config
  (setq neo-create-file-auto-open t
        neo-smart-open t
        neo-show-hidden-files t
        neo-auto-indent-point t))

;; Nlinum Mode
(use-package nlinum
  :ensure t
  :bind ("C-c t l" . nlinum-mode))

;; Paradox
(use-package paradox
  :ensure t
  :bind ("C-c a p" . paradox-list-packages)
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

;; PKGBUILD Mode
(use-package pkgbuild-mode
  :ensure t
  :defer t)

;; rcirc Mode
(use-package rcirc
  :bind ("C-c a i" . irc)
  :config
  (setq rcirc-server-alist
        '(("adams.freenode.net"
           :port 7000
           :encryption tls
           :channels ("#archlinux" "#emacs" "#scheme"))
          ("pine.forestnet.org"
           :port 6697
           :encryption tls
           :channels ("#reloaded" "#rawhide" "#fo2"))))

  ;; Authentication via separate file
  (when (file-exists-p "~/.private.el")
    (setq drot/credentials-file "~/.private.el")

    (defun drot/nickserv-password ()
      "Read the passwords from the credientials file"
      (with-temp-buffer
        (insert-file-contents-literally drot/credentials-file)
        (plist-get (read (buffer-string)) :nickserv-password)))

    (setq rcirc-authinfo
          `(("freenode" nickserv "drot" ,(drot/nickserv-password))
            ("forestnet" nickserv "drot" ,(drot/nickserv-password)))))

  ;; Truncate buffer output
  (setq rcirc-buffer-maximum-lines 1024)

  ;; Set fill column value to frame width
  (setq rcirc-fill-column 'frame-width)

  ;; Enable logging
  (setq rcirc-log-flag t)

  ;; Enable additional modes
  (defun drot/rcirc-mode-hook ()
    "Disable company and YASnippet in rcirc buffers."
    (company-mode 0)
    (yas-minor-mode 0))

  (add-hook 'rcirc-mode-hook #'drot/rcirc-mode-hook)
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (add-hook 'rcirc-mode-hook #'flyspell-mode)

  ;; Add some custom commands
  (defun-rcirc-command chanserv (arg)
    "Send a private message to the ChanServ service."
    (rcirc-send-string process (concat "chanserv " arg)))

  (defun-rcirc-command mystery (arg)
    "Send a private message to the Mystery service."
    (rcirc-send-string process (concat "mystery " arg)))

  (defun-rcirc-command memoserv (arg)
    "Send a private message to the MemoServ service."
    (rcirc-send-string process (concat "memoserv " arg)))

  (defun-rcirc-command nickserv (arg)
    "Send a private message to the NickServ service."
    (rcirc-send-string process (concat "nickserv " arg))))

;; rcirc color codes support
(use-package rcirc-styles
  :ensure t
  :after rcirc
  :config
  (setq rcirc-styles-color-vector
        ["#7F7F7F"
         "#CC9393"
         "#7F9F7F"
         "#D0BF8F"
         "#6CA0A3"
         "#DC8CC3"
         "#93E0E3"
         "#DCDCCC"
         "#9F9F9F"
         "#DCA3A3"
         "#BFEBBF"
         "#F0DFAF"
         "#8CD0D3"
         "#DC8CC3"
         "#93E0E3"
         "#FFFFEF"]))

;; rcirc colored nicknames
(use-package rcirc-color
  :ensure t
  :after rcirc
  :config
  (setq rcirc-colors (append rcirc-styles-color-vector nil)))

;; rcirc notifications
(use-package rcirc-notify
  :ensure t
  :after rcirc
  :config
  (rcirc-notify-add-hooks))

;; Smex
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (expand-file-name "smex-items" drot/cache-directory)))

;; Systemd mode
(use-package systemd
  :ensure t
  :defer t)

;; Web mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

;; Zop-to-char
(use-package zop-to-char
  :ensure t
  :bind ([remap zap-to-char]. zop-to-char))

;; Ace-link
(use-package ace-link
  :ensure t
  :bind ("C-c n o" . ace-link-addr)
  :commands ace-link-setup-default
  :init
  (ace-link-setup-default))

;; Adaptive Wrap
(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;; Anzu
(use-package anzu
  :ensure t
  :diminish (anzu-mode . "AZ")
  :bind (("C-c s q" . anzu-query-replace)
         ("C-c s r" . anzu-query-replace-regexp))
  :commands global-anzu-mode
  :init
  (global-anzu-mode)
  :config
  (setq anzu-replace-to-string-separator " => "))

;; Company mode
(use-package company
  :ensure t
  :diminish (company-mode . "CY")
  :bind ("C-c i c" . company-yasnippet)
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-selection-wrap-around t
        company-backends '(company-nxml
                           company-css
                           company-capf (company-dabbrev-code company-keywords)
                           company-files
                           company-dabbrev)))

;; Company Statistics
(use-package company-statistics
  :ensure t
  :commands company-statistics-mode
  :init
  (add-hook 'after-init-hook #'company-statistics-mode)
  :config
  (setq company-statistics-file (expand-file-name "company-statistics-cache.el" drot/cache-directory)))

;; Diff-Hl
(use-package diff-hl
  :ensure t
  :commands (global-diff-hl-mode diff-hl-dired-mode diff-hl-margin-mode)
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;; Eyebrowse
(use-package eyebrowse
  :ensure t
  :commands eyebrowse-mode
  :init
  (eyebrowse-mode))

;; Highlight Numbers
(use-package highlight-numbers
  :ensure t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Hl-Todo
(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;; Swiper
(use-package swiper
  :ensure t
  :diminish (ivy-mode . "IY")
  :bind (("C-c s s" . swiper)
         ("C-c s i" . swiper-from-isearch)
         ("C-c f r" . ivy-recentf)
         ("C-c t c" . ivy-resume))
  :commands ivy-mode
  :init
  (ivy-mode)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-count-format "(%d/%d) "
        ivy-format-function #'ivy-format-function-arrow
        ivy-wrap t))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-c f l" . counsel-load-library)
         ("C-c f g" . counsel-git)
         ("C-c n i" . counsel-imenu)
         ("C-c s l" . counsel-info-lookup-symbol)
         ("C-c s g" . counsel-git-grep)
         ("C-c i u" . counsel-unicode-char)
         ("C-c i p" . counsel-yank-pop))
  :config
  (setq counsel-find-file-at-point t))

;; Lispy
(use-package lispy
  :ensure t
  :commands lispy-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'lispy-mode))
  :config
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

  (add-hook 'minibuffer-setup-hook #'drot/lispy-minibuffer))

;; Form-feed
(use-package form-feed
  :ensure t
  :commands form-feed-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook
                  compilation-mode
                  outline-mode-hook-hook
                  help-mode-hook))
    (add-hook hook #'form-feed-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m <SPC>" . mc/vertical-align-with-space)
         ("C-c m a" . mc/vertical-align)
         ("C-c m e" . mc/mark-more-like-this-extended)
         ("C-c m h" . mc/mark-all-like-this-dwim)
         ("C-c m l" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)
         ("C-c m C-s" . mc/mark-all-in-region))
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" drot/cache-directory)))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Rainbow mode
(use-package rainbow-mode
  :ensure t
  :diminish (rainbow-mode . "RW")
  :bind ("C-c t r" . rainbow-mode))

;; Undo Tree
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "UT")
  :commands global-undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist backup-directory-alist
        undo-tree-auto-save-history t))

;; Visual Fill Column
(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;; Volatile Highlights
(use-package volatile-highlights
  :ensure t
  :diminish (volatile-highlights-mode . "VH")
  :commands volatile-highlights-mode
  :init
  (volatile-highlights-mode))

;; Which Key
(use-package which-key
  :ensure t
  :bind ("C-c h w" . which-key-show-top-level)
  :commands which-key-mode
  :init
  (setq which-key-show-prefix 'bottom
        which-key-special-keys nil
        which-key-separator " > ")
  (which-key-mode)
  :config
  (which-key-declare-prefixes
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x r" "register"
    "C-x w" "highlight"
    "C-x C-a" "edebug"
    "C-c @" "hs-and-outline"
    "C-c &" "yasnippet"
    "C-c a" "applications"
    "C-c c" "compile-and-comments"
    "C-c f" "files"
    "C-c h" "help"
    "C-c i" "insertion"
    "C-c m" "multiple-cursors"
    "C-c n" "navigation"
    "C-c o" "org"
    "C-c s" "search-and-symbols"
    "C-c t" "toggles"
    "C-c v" "version-control"
    "C-c w" "windows-and-frames"
    "C-c C-w" "eyebrowse"
    "C-c x" "text"))

;; YASnippet
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "YS")
  :commands yas-global-mode
  :init
  (setq yas-verbosity 1)
  (yas-global-mode))

;;; init.el ends here
