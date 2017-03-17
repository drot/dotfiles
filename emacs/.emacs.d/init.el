;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/master/emacs/.emacs.d
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

;; Delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset garbage collection threshold value to default after startup
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold 400000)))

;; Set default directory for save files
(make-directory (locate-user-emacs-file "cache") t)

;; Disable the site default settings
(setq inhibit-default-init t)

;; Prefer newest version of a file
(setq load-prefer-newer t)

;; Activate packages and add the MELPA package archive
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Package archive priorities
(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 20)))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable Imenu support for use-package
(setq use-package-enable-imenu-support t)

;; Load use-package
(eval-when-compile
  (require 'use-package))

;; Disable needless GUI elements
(dolist (mode '(tool-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Color theme
(use-package zenburn-theme
  :ensure t)

;; Don't show the startup welcome messages
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-screen t)

;; Disable scratch buffer info text
(setq initial-scratch-message nil)

;; Show column number and buffer size on the mode line
(column-number-mode)
(size-indication-mode)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p #'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.01)

;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

;; Move point all the way when scrolling to buffer boundaries
(setq scroll-error-top-bottom t)

;; Flash frame instead of beeping
(setq visible-bell t)

;; Don't use dialogs for minibuffer input
(setq use-dialog-box nil)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Cycle completion on smaller number of candidates
(setq completion-cycle-threshold 5)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Indicate minibuffer recursion depth
(minibuffer-depth-indicate-mode)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; No length limit when printing values
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Try to extract docstrings from special forms
(setq bind-key-describe-special-forms t)

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Increase default fill width
(setq-default fill-column 80)

;; Require a final new line
(setq require-final-newline t)

;; Set Text mode as the default major mode
(setq-default major-mode #'text-mode)

;; Enable Auto Fill mode for Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " FiL")

;; Put underline below the font bottom line
(setq x-underline-at-descent-line t)

;; Draw block cursor as wide as the glyph under it
(setq x-stretch-cursor t)

;; Use Emacs own tooltip implementation
(setq x-gtk-use-system-tooltips nil)

;; Highlight region even in non-selected windows
(setq highlight-nonselected-windows t)

;; Enable faster scrolling
(setq fast-but-imprecise-scrolling t)

;; Resize windows proportionally
(setq window-combination-resize t)

;; Keep window point when switching buffers
(setq switch-to-buffer-preserve-window-point t)

;; Change recenter initial position
(setq recenter-positions '(top middle bottom))

;; Prompt for buffer switch in strongly dedicated windows
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; Display read-only buffers in view mode
(setq view-read-only t)
(setq view-inhibit-help-message t)

;; Kill and yank clipboard options
(setq select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Increase maximum size of the mark ring
(setq mark-ring-max 30)

;; Repeat mark popping
(setq set-mark-command-repeat-pop t)

;; Do not save duplicates
(setq history-delete-duplicates t)
(setq kill-do-not-save-duplicates t)

;; Configuration for backup files
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "cache") t)))
(setq auto-save-list-file-prefix (locate-user-emacs-file "cache/.saves-"))
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "cache"))))
(setq version-control t)
(setq kept-new-versions 6)
(setq delete-old-versions t)
(setq backup-by-copying t)

;; Visual Line mode configuration
(setq visual-line-fringe-indicators '(nil vertical-bar))
(diminish 'visual-line-mode " ViL")

;; Enable current line highlight in programming modes
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Save minibuffer history
(use-package savehist
  :config
  (setq savehist-file (locate-user-emacs-file "cache/saved-history"))
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-mode))

;; Save recent files list
(use-package recentf
  :config
  (setq recentf-save-file (locate-user-emacs-file "cache/recent-files"))
  (setq recentf-exclude '("/\\.git/.*\\'"
                          "/elpa/.*\\'"
                          "/cache/.*\\'"
                          ".*\\.gz\\'"))
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 20)
  (setq recentf-auto-cleanup 600)
  (recentf-mode))

;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "cache/saved-places"))
  (save-place-mode))

;; Highlight matching parentheses
(use-package paren
  :config
  (setq show-paren-delay 0)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (show-paren-mode))

;; Highlight regexps interactively
(use-package hi-lock
  :config
  (setq hi-lock-auto-select-face t)
  (global-hi-lock-mode))

;; Abbrev mode
(use-package abbrev
  :config
  (setq abbrev-file-name (expand-file-name "abbrevs" user-emacs-directory))
  (setq save-abbrevs t)
  ;; Load abbrevs if they exist
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq-default abbrev-mode t)
  :diminish (abbrev-mode . "AbR"))

;; Electric pair mode
(use-package elec-pair
  :config
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  (electric-pair-mode))

;; Prettify certain symbols
(use-package prog-mode
  :config
  (setq prettify-symbols-unprettify-at-point t)
  (global-prettify-symbols-mode))

;; Which function mode
(use-package which-func
  :config
  (setq which-func-unknown "⊥")
  (which-function-mode))

;; Fast window switching
(use-package windmove
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings))

;; Undo and redo the window configuration
(use-package winner
  :bind (:map winner-mode-map
              ("C-c w u" . winner-undo)
              ("C-c w r" . winner-redo))
  :commands winner-mode
  :init
  (winner-mode)
  :config
  ;; Disable conflicting key bindings
  (unbind-key "C-c <left>" winner-mode-map)
  (unbind-key "C-c <right>" winner-mode-map))

;; Outline mode
(use-package outline
  :bind ("C-c t o" . outline-minor-mode)
  :init
  (setq outline-minor-mode-prefix (kbd "C-c O"))
  (add-hook 'text-mode-hook #'outline-minor-mode)
  :diminish (outline-minor-mode . "OuT"))

;; Hide Show mode
(use-package hideshow
  :commands hs-minor-mode
  :init
  (dolist (hook '(c-mode-common-hook
                  emacs-lisp-mode-hook
                  python-mode-hook))
    (add-hook hook #'hs-minor-mode))
  :config
  (defun drot|display-code-line-counts (ov)
    "Unique overlay function to be applied with `hs-minor-mode'."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (format "... / %d"
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))
  ;; Configure package
  (setq hs-set-up-overlay #'drot|display-code-line-counts)
  (setq hs-isearch-open t))

;; Bug Reference mode
(use-package bug-reference
  :commands (bug-reference-mode bug-reference-prog-mode)
  :init
  (add-hook 'text-mode-hook #'bug-reference-mode)
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  :config
  (setq bug-reference-url-format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"))

;; Goto Address mode
(use-package goto-addr
  :commands (goto-address-mode goto-address-prog-mode)
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Fly Spell mode configuration
(use-package flyspell
  :bind (("C-c x f" . flyspell-buffer)
         :map flyspell-mode-map
         ("C-c x c" . flyspell-auto-correct-word)
         ("C-c x C" . flyspell-correct-word-before-point))
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  ;; Disable conflicting key bindings
  (unbind-key "C-c $" flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  ;; Configure package
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-consider-dash-as-word-delimiter-flag t)
  (setq flyspell-duplicate-distance 12000)
  :diminish (flyspell-mode . "FlS"))

;; Isearch configuration
(use-package "isearch"
  :defer t
  :config
  (setq isearch-allow-scroll t)
  (setq search-default-mode #'char-fold-to-regexp)
  :diminish (isearch-mode . "IsR"))

;; Ispell default program
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra")))

;; Ediff window split configuration
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-grab-mouse nil))

;; Ediff restore window configuration
(use-package ediff-util
  :defer t
  :config
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; Uniquify buffer names
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Version control configuration
(use-package vc-hooks
  :defer t
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq vc-follow-symlinks t)
  (setq vc-make-backup-files t))

;; Customize interface options
(use-package cus-edit
  :defer t
  :config
  (setq custom-buffer-done-kill t)
  (setq custom-buffer-verbose-help nil)
  (setq custom-unlispify-tag-names nil)
  (setq custom-unlispify-menu-entries nil))

;; Treat all themes as safe
(use-package custom
  :defer t
  :config
  (setq custom-safe-themes t))

;; Imenu configuration
(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t))

;; ElDoc mode configuration
(use-package eldoc
  :defer t
  :config
  ;; Make compatible with ParEdit
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round)
  :diminish (eldoc-mode . "ElD"))

;; Python mode configuration
(use-package python
  :defer t
  :config
  ;; Use Python 3 as default
  (setq python-shell-interpreter "python3")
  ;; Disable indent offset guessing
  (setq python-indent-guess-indent-offset nil)
  ;; PEP8 conformance
  (add-hook 'python-mode-hook
            (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r")))
  (add-hook 'c-mode-common-hook #'auto-fill-mode))

;; Scheme mode configuration
(use-package scheme
  :defer t
  :config
  (setq scheme-program-name "guile"))

;; CSS mode configuration
(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

;; NXML mode configuration
(use-package nxml-mode
  :defer t
  :config
  (setq nxml-slash-auto-complete-flag t)
  (setq nxml-auto-insert-xml-declaration-flag t))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300)
  (setq doc-view-continuous t))

;; Colorize ANSI escape sequences
(use-package ansi-color
  :defer t
  :config
  (defun drot|colorize-compilation-buffer ()
    "Colorize the compilation mode buffer"
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'drot|colorize-compilation-buffer))

;; Mail sending configuration
(use-package message
  :defer t
  :config
  (setq message-send-mail-function #'smtpmail-send-it)
  (setq message-confirm-send t)
  (setq message-kill-buffer-on-exit t)
  ;; Save the BBDB database on every exit action
  (message-add-action #'bbdb-save 'exit 'postpone 'kill 'send))

;; Outgoing mail server
(use-package smtpmail
  :defer t
  :config
  (setq smtpmail-smtp-server "mail.cock.li")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

;; Smiley configuration
(use-package smiley
  :defer t
  :config
  (setq smiley-style 'medium))

;; SHR configuration
(use-package shr
  :defer t
  :config
  (setq shr-use-fonts nil))

;; Prevent GnuTLS warnings
(use-package gnutls
  :defer t
  :config
  (setq gnutls-min-prime-bits nil))

;; Dired configuration
(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-ahlF")
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t))

;; Additional features with dired-x
(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :after dired
  :config
  (setq dired-omit-verbose nil)
  ;; Ignore uninteresting files
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;; Allow permission editing in Wdired
(use-package wdired
  :defer t
  :config
  (setq wdired-allow-to-change-permissions t))

;; TRAMP configuration
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (locate-user-emacs-file "cache/tramp"))
  (setq tramp-backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq tramp-auto-save-directory temporary-file-directory))

;; Find file at point
(use-package ffap
  :bind ("C-c f f" . find-file-at-point)
  :config
  (setq ffap-machine-p-known 'reject))

;; Search more extensively with apropos
(use-package apropos
  :bind ("C-c h a" . apropos)
  :config
  (setq apropos-do-all t))

;; Copyright insertion
(use-package copyright
  :bind ("C-c i C" . copyright-update)
  :config
  (setq copyright-year-ranges t)
  (setq copyright-names-regexp (regexp-quote user-login-name)))

;; Whitespace mode
(use-package whitespace
  :bind (("C-c x w" . whitespace-cleanup)
         ("C-c t w" . whitespace-mode))
  :diminish (whitespace-mode . "WhS"))

;; Regexp builder
(use-package re-builder
  :bind ("C-c s r" . re-builder)
  :config
  (setq reb-re-syntax 'string))

;; Proced
(use-package proced
  :bind ("C-c a P" . proced)
  :config
  (setq-default proced-sort 'start)
  (setq-default proced-tree-flag t))

;; GDB
(use-package gdb-mi
  :bind ("C-c a D" . gdb)
  :config
  (setq gdb-many-windows t))

;; Open URLs with the specified browser
(use-package browse-url
  :bind (("C-c n u" . browse-url)
         ("C-c n b" . browse-url-at-point))
  :config
  (setq browse-url-browser-function #'browse-url-chromium))

;; Bookmarks
(use-package bookmark
  :bind ("C-c f b" . list-bookmarks)
  :config
  (setq bookmark-default-file (locate-user-emacs-file "cache/bookmark"))
  (setq bookmark-save-flag 1))

;; Speedbar configuration
(use-package speedbar
  :bind (("C-c p s" . speedbar)
         :map speedbar-mode-map
         ("a" . speedbar-toggle-show-all-files))
  :config
  ;; Emulate NERDTree behavior
  (setq speedbar-use-images nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-directory-unshown-regexp "^$")
  (speedbar-add-supported-extension
   '("PKGBUILD" ".lisp" ".lua" ".css" ".patch"
     ".conf" ".diff" ".sh" ".org" ".md" ".deb")))

;; Eshell configuration
(use-package eshell
  :bind ("C-c a S" . eshell)
  :config
  (setq eshell-hist-ignoredups t))

;; Eshell smart display
(use-package em-smart
  :after eshell
  :config
  (eshell-smart-initialize))

;; Shell mode configuration
(use-package shell
  :bind ("C-c a s" . shell)
  :config
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook #'compilation-shell-minor-mode))

;; IELM
(use-package ielm
  :bind ("C-c a e" . ielm)
  :config
  (setq ielm-prompt "EL> "))

;; Compilation configuration
(use-package compile
  :bind (("C-c c C" . compile)
         ("C-c c R" . recompile))
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-context-lines 3))

;; Gnus
(use-package gnus
  :bind ("C-c a g" . gnus)
  :config
  ;; Configure mail and news server
  (setq gnus-select-method '(nnimap "cock"
                                    (nnimap-address "mail.cock.li")
                                    (nnimap-server-port 993)
                                    (nnimap-stream ssl)))
  (add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
  ;; Article fetching options
  (setq gnus-article-browse-delete-temp t)
  (setq gnus-treat-strip-trailing-blank-lines 'last)
  (setq gnus-mime-display-multipart-related-as-mixed t)
  (setq gnus-auto-select-first nil)
  ;; Group by topics
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  ;; Configure visible headers
  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
  ;; Show the article headers in this order
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))
  ;; Set return email address based on incoming email address
  (setq gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com"))))
  ;; Display of the summary buffer
  (setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n")
  (setq gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M")))
  (setq gnus-group-line-format "%M%S%p%P%5y:%B %G\n")
  (setq gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references)
  (setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
  ;; Display of message threading
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-indent "    ")
  (setq gnus-sum-thread-tree-vertical "│   ")
  (setq gnus-sum-thread-tree-leaf-with-other "├──>")
  (setq gnus-sum-thread-tree-single-leaf "└──>"))

;; Calendar configuration
(use-package calendar
  :bind ("C-c a C" . calendar)
  :config
  (setq holiday-general-holidays nil)
  (setq holiday-solar-holidays nil)
  (setq holiday-bahai-holidays nil)
  (setq holiday-oriental-holidays nil)
  (setq holiday-islamic-holidays nil)
  (setq holiday-hebrew-holidays nil)
  (setq calendar-week-start-day 1)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-date-style 'european)
  (setq calendar-latitude 43.2)
  (setq calendar-longitude 17.48)
  (setq calendar-location-name "Mostar, Bosnia and Herzegovina"))

;; Org-mode configuration
(use-package org
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o t" . org-todo-list)
         ("C-c o s" . org-search-view)
         ("C-c o l" . org-store-link))
  :config
  (setq org-directory (expand-file-name "org" user-emacs-directory))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-agenda-files '("~/.emacs.d/org"))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-catch-invisible-edits 'error)
  (setq org-startup-indented t)
  ;; Avoid Wind Move conflict
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

;; World time
(use-package time
  :bind ("C-c a T" . display-time-world)
  :config
  (setq display-time-world-list '(("Europe/Riga" "Riga")
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("Canada/Eastern" "Quebec")
                                  ("Asia/Saigon" "Saigon"))))

;; Ace-window
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Anaconda mode
(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  :diminish (anaconda-mode . "AnC"))

;; Bookmark+
(use-package bookmark+
  :ensure t
  :bind ("C-x j j" . bookmark-jump)
  :after bookmark
  :config
  (setq bmkp-bmenu-state-file (locate-user-emacs-file "cache/bmkp-bmenu-state.el"))
  (setq bmkp-bmenu-commands-file (locate-user-emacs-file "cache/bmkp-bmenu-commands.el"))
  (setq bmkp-auto-light-when-set 'all-in-buffer)
  (setq bmkp-auto-light-when-jump 'all-in-buffer))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-c n c" . avy-goto-char)
         ("C-c n k" . avy-goto-char-2)
         ("C-c n J" . avy-goto-word-0)
         ("C-c n SPC" . avy-pop-mark)
         ("C-c n l" . avy-goto-line)
         ("C-c n j" . avy-goto-word-or-subword-1))
  :config
  (setq avy-all-windows 'all-frames)
  (setq avy-background t)
  (setq avy-highlight-first t)
  (avy-setup-default))

;; Dash
(use-package dash
  :ensure t
  :defer t
  :config
  (dash-enable-font-lock))

;; Debbugs
(use-package debbugs
  :ensure t
  :bind (("C-c d g" . debbugs-gnu)
         ("C-c d s" . debbugs-gnu-search)
         ("C-c d t" . debbugs-gnu-usertags)
         ("C-c d p" . debbugs-gnu-patches)
         ("C-c d b" . debbugs-gnu-bugs)
         ("C-c d O" . debbugs-org)
         ("C-c d S" . debbugs-org-search)
         ("C-c d P" . debbugs-org-patches)
         ("C-c d B" . debbugs-org-bugs)))

;; Dired+
(use-package dired+
  :ensure t
  :init
  (setq diredp-hide-details-initially-flag nil)
  :after dired-x)

;; Dired Async
(use-package dired-async
  :ensure async
  :after dired+
  :config
  (dired-async-mode))

;; Easy-kill
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Elfeed
(use-package elfeed
  :ensure t
  :bind ("C-c a f" . elfeed)
  :config
  (setq elfeed-feeds '(("https://news.ycombinator.com/rss" hnews)
                       ("https://lwn.net/headlines/rss" lwn)
                       ("https://www.reddit.com/r/emacs/.rss" emacs)
                       ("https://www.reddit.com/r/linux/.rss" linux)
                       ("https://www.reddit.com/r/linux/.rss" programming)
                       ("http://bljesak.info/rss" bljesak)))
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-search-date-format '("%d-%m-%Y" 10 :left))
  (setq elfeed-search-filter "@1-week-ago +unread"))

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
  (setq geiser-repl-history-filename (locate-user-emacs-file "cache/geiser-history")))

;;IEdit
(use-package iedit
  :ensure t
  :bind (("C-c i i" . iedit-mode)
         :map isearch-mode-map
         ("C-i" . iedit-mode-from-isearch)
         :map esc-map
         ("C-;" . iedit-execute-last-modification)
         :map help-map
         ("C-;" . iedit-mode-toggle-on-function))
  :functions iedit-mode-toggle-on-function
  :config
  (setq iedit-toggle-key-default nil))

;; Info+
(use-package info+
  :ensure t
  :after info)

;; JavaScript mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;; JSON mode
(use-package json-mode
  :pin gnu
  :ensure t
  :defer t)

;; Key Chord
(use-package key-chord
  :ensure t
  :commands key-chord-mode
  :init
  (add-hook 'after-init-hook
            (lambda () (key-chord-mode 1)))
  :config
  (key-chord-define-global "3j" #'dired-jump)
  (key-chord-define-global "3l" #'avy-goto-line)
  (key-chord-define-global "3u" #'undo-tree-visualize)
  (key-chord-define-global "8w" #'avy-goto-word-or-subword-1)
  (key-chord-define-global "8c" #'avy-goto-char)
  (key-chord-define-global "8e" #'er/expand-region)
  (key-chord-define-global "8q" #'query-replace)
  (key-chord-define-global "8r" #'replace-string))

;; Lua mode
(use-package lua-mode
  :ensure t
  :defer t)

;; Hydra
(use-package hydra
  :ensure t
  :bind (("C-c w R" . hydra-window-resize/body)
         ("C-c x O" . hydra-outline/body)
         ("C-c x M" . hydra-move-text/body))
  :config
  (hydra-add-font-lock)

  (defhydra hydra-move-text (:columns 2)
    "Move Text"
    ("p" move-text-up "Move Text Up")
    ("n" move-text-down "Move Text Down")
    ("q" nil "Quit"))

  (defhydra hydra-window-resize (:columns 2)
    "Resize Windows"
    ("n" enlarge-window "Enlarge Window")
    ("p" shrink-window "Shrink Window")
    ("f" enlarge-window-horizontally "Enlarge Window Horizontally")
    ("b" shrink-window-horizontally "Shrink Window Horizontally")
    ("q" nil "Quit"))

  (defhydra hydra-outline (:columns 4)
    "Outline Mode"
    ("z" hide-sublevels "Hide Sub-Levels")
    ("t" hide-body "Hide Body")
    ("o" hide-other "Hide Other")
    ("c" hide-entry "Hide Entry")
    ("l" hide-leaves "Hide Leaves")
    ("d" hide-subtree "Hide Sub-Tree")
    ("a" show-all "Show All")
    ("e" show-entry "Show Entry")
    ("i" show-children "Show Children")
    ("k" show-branches "Show Branches")
    ("s" show-subtree "Show Sub-Tree")
    ("u" outline-up-heading "Up Heading")
    ("n" outline-next-visible-heading "Next Visible Heading")
    ("p" outline-previous-visible-heading "Previous Visible Heading")
    ("f" outline-forward-same-level "Forward Same Level")
    ("b" outline-backward-same-level "Backward Same Level")
    ("q" nil "Quit")))

;; Macrostep
(use-package macrostep
  :ensure t
  :bind ("C-c i e" . macrostep-expand))

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
  :defer t)

;; NeoTree
(use-package neotree
  :ensure t
  :bind ("C-c f n" . neotree-toggle)
  :config
  (setq neo-theme 'nerd)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-auto-indent-point t))

;; nLinum mode
(use-package nlinum
  :ensure t
  :bind ("C-c t l" . nlinum-mode))

;; Paradox
(use-package paradox
  :ensure t
  :bind ("C-c a p" . paradox-list-packages)
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-spinner-type 'rotating-line)
  (setq paradox-display-download-count t))

;; PKGBUILD mode
(use-package pkgbuild-mode
  :ensure t
  :defer t)

;; rcirc mode
(use-package rcirc
  :bind ("C-c a i" . irc)
  :config
  ;; User defaults
  (setq rcirc-default-user-name "drot")
  (setq rcirc-reconnect-delay 10)
  ;; Connect to the specified servers and channels
  (setq rcirc-server-alist
        '(("adams.freenode.net"
           :port 6697
           :encryption tls
           :channels ("#archlinux" "#emacs" "#scheme"))
          ("pine.forestnet.org"
           :port 6697
           :encryption tls
           :channels ("#reloaded" "#rawhide" "#fo2"))))

  (defadvice rcirc (before rcirc-read-from-authinfo activate)
    "Allow rcirc to read authinfo from ~/.authinfo.gpg via the auth-source API.
This doesn't support the chanserv auth method"
    (unless arg
      (dolist (p (auth-source-search :port '("nickserv" "bitlbee" "quakenet")
                                     :max 2
                                     :require '(:port :user :secret)))
        (let ((secret (plist-get p :secret))
              (method (intern (plist-get p :port))))
          (add-to-list 'rcirc-authinfo
                       (list (plist-get p :host) method (plist-get p :user)
                             (if (functionp secret)
                                 (funcall secret) secret)))))))

  ;; Truncate buffer output
  (setq rcirc-buffer-maximum-lines 1024)
  ;; Set fill column value to frame width
  (setq rcirc-fill-column 'frame-width)
  ;; Enable logging
  (setq rcirc-log-flag t)

  ;; Enable additional modes
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (add-hook 'rcirc-mode-hook #'flyspell-mode)

  ;; Disable company mode in rcirc buffers
  (add-hook 'rcirc-mode-hook
            (lambda () (company-mode -1)))

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

;; SLIME
(use-package slime
  :ensure t
  :bind (("C-c t s" . slime)
         ("C-c t C" . slime-connect)
         :map slime-mode-indirect-map
         ("C-c $" . slime-export-symbol-at-point))
  :config
  ;; Disable conflicting key binding
  (unbind-key "C-c x" slime-mode-indirect-map)
  ;; Configure package
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-company))
  (setq slime-protocol-version 'ignore)
  (setq slime-repl-history-file (locate-user-emacs-file "cache/slime-history.eld")))

;; SLIME REPL
(use-package slime-repl
  :ensure slime
  :bind (:map slime-repl-mode-map
              ("C-c M-r" . slime-repl-previous-matching-input)
              ("C-c M-s" . slime-repl-next-matching-input))
  :defer t
  :config
  ;; Disable conflicting key bindings
  (unbind-key "DEL" slime-repl-mode-map)
  (unbind-key "M-r" slime-repl-mode-map)
  (unbind-key "M-s" slime-repl-mode-map))

;; SLIME Company
(use-package slime-company
  :ensure t
  :defer t
  :config
  (setq slime-company-completion 'fuzzy))

;; Smex
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

;; Systemd mode
(use-package systemd
  :ensure t
  :defer t)

;; Wgrep
(use-package wgrep
  :ensure t
  :defer t)

;; YAML mode
(use-package yaml-mode
  :ensure t
  :defer t)

;; Zop-to-char
(use-package zop-to-char
  :ensure t
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;; Ace-link
(use-package ace-link
  :ensure t
  :bind (("C-c n a" . ace-link-addr))
  :commands ace-link-setup-default
  :init
  (add-hook 'after-init-hook #'ace-link-setup-default))

;; Adaptive Wrap
(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;; Anzu
(use-package anzu
  :ensure t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :commands global-anzu-mode
  :init
  (add-hook 'after-init-hook #'global-anzu-mode)
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 50)
  (setq anzu-replace-to-string-separator " => ")
  :diminish (anzu-mode . "AnZ"))

;; BBDB
(use-package bbdb
  :ensure t
  :bind (("C-c o B" . bbdb)
         ("C-c o b" . bbdb-create))
  :commands (bbdb-initialize bbdb-mua-auto-update-p)
  :init
  (add-hook 'after-init-hook
            (lambda () (bbdb-initialize 'gnus 'message)))
  (add-hook 'after-init-hook
            (lambda () (bbdb-mua-auto-update-init 'gnus 'message)))
  :config
  (setq bbdb-update-records-p 'create)
  (setq bbdb-mua-pop-up nil)
  (setq bbdb-phone-style nil)
  (setq bbdb-complete-mail-allow-cycling t)
  (setq bbdb-ignore-message-alist '(("From" . "noreply")
                                    ("From" . "no-reply")
                                    ("From" . "donotreply")
                                    ("From" . "subscription")
                                    ("From" . "newsletter")
                                    ("From" . "gmane.org")
                                    ("From" . "debbugs.gnu.org")
                                    ("From" . "mailer-daemon")
                                    ("From" . "arch-general")))
  ;; Save the database after exiting Gnus
  (add-hook 'gnus-summary-exit-hook #'bbdb-save))

;; Company mode
(use-package company
  :ensure t
  :bind ("C-c i c" . company-yasnippet)
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-idle-delay 1.0)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-show-numbers t)
  (setq company-require-match 'never)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-everywhere t)
  (setq company-selection-wrap-around t)
  (setq company-backends '(company-bbdb
                           company-nxml
                           company-css
                           company-capf
                           company-files
                           (company-dabbrev-code company-keywords)
                           company-dabbrev))
  :diminish (company-mode . "CmP"))

;; Company Anaconda
(use-package company-anaconda
  :ensure t
  :commands company-anaconda
  :init
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends #'company-anaconda))))

;; Company Statistics
(use-package company-statistics
  :ensure t
  :commands company-statistics-mode
  :init
  (add-hook 'global-company-mode-hook #'company-statistics-mode)
  :config
  (setq company-statistics-file (locate-user-emacs-file "cache/company-statistics-cache.el")))

;; Diff-Hl
(use-package diff-hl
  :ensure t
  :commands global-diff-hl-mode
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  :config
  ;; Update diffs immediately
  (diff-hl-flydiff-mode)
  ;; Add hooks for other packages
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Eyebrowse
(use-package eyebrowse
  :ensure t
  :commands eyebrowse-mode
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c W"))
  (add-hook 'after-init-hook #'eyebrowse-mode)
  :config
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth t))

;; FlyCheck
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; FlyCheck GUI popups
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

;; Form-feed
(use-package form-feed
  :ensure t
  :commands form-feed-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook
                  compilation-mode-hook
                  outline-mode-hook
                  help-mode-hook))
    (add-hook hook #'form-feed-mode)))

;; Hl-Todo
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
              ("C-c p t p" . hl-todo-previous)
              ("C-c p t n" . hl-todo-next)
              ("C-c p t o" . hl-todo-occur))
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;; Ivy
(use-package ivy
  :ensure ivy-hydra
  :bind (("C-c n R" . ivy-resume))
  :commands ivy-mode
  :init
  (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-format-function #'ivy-format-function-arrow)
  (setq ivy-wrap t)
  (setq ivy-action-wrap t))

;; Counsel
(use-package counsel
  :ensure t
  :bind (("C-c f g" . counsel-git)
         ("C-c f j" . counsel-dired-jump)
         ("C-c f r" . counsel-recentf)
         ("C-c s G" . counsel-git-grep)
         ("C-c s i" . counsel-imenu)
         ("C-c s g" . counsel-grep)
         ("C-c n m" . counsel-mark-ring)
         ("C-c h l" . counsel-find-library)
         ("C-c i 8" . counsel-unicode-char))
  :commands counsel-mode
  :init
  (add-hook 'after-init-hook #'counsel-mode)
  :config
  (setq counsel-find-file-at-point t)
  :diminish (counsel-mode . "CsL"))

;; Swiper
(use-package swiper
  :ensure t
  :bind (("C-c s S" . swiper-all)
         ("C-c s s" . swiper)
         :map isearch-mode-map
         ("C-c S" . swiper-from-isearch)))

;; Paredit
(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  ielm-mode-hook
                  scheme-mode-hook
                  slime-repl-mode-hook
                  geiser-repl-mode-hook))
    (add-hook hook #'enable-paredit-mode))
  :config
  ;; Enable Paredit in other related modes
  (defvar drot|paredit-minibuffer-setup-commands
    '(eval-expression
      pp-eval-expression
      eval-expression-with-eldoc
      ibuffer-do-eval
      ibuffer-do-view-and-eval)
    "Interactive commands for which Paredit should be enabled in the minibuffer.")

  (defun drot|paredit-minibuffer-setup ()
    "Enable Paredit during lisp-related minibuffer commands."
    (if (memq this-command drot|paredit-minibuffer-setup-commands)
        (enable-paredit-mode)))

  (add-hook 'minibuffer-setup-hook #'drot|paredit-minibuffer-setup)

  ;; Disable Electric Pair mode when Paredit is active
  (add-hook 'paredit-mode-hook
            (lambda () (setq-local electric-pair-mode nil)))

  :diminish (paredit-mode . "PaR"))

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
  :bind ("C-c t R" . rainbow-mode)
  :commands rainbow-mode
  :init
  (add-hook 'css-mode-hook #'rainbow-mode)
  :diminish (rainbow-mode . "RbW"))

;; Skewer
(use-package skewer-mode
  :ensure t
  :bind ("C-c t S" . run-skewer)
  :commands skewer-mode
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  :diminish (skewer-mode . "SkW"))

;; Skewer CSS
(use-package skewer-css
  :ensure skewer-mode
  :commands skewer-css-mode
  :init
  (add-hook 'css-mode-hook #'skewer-css-mode)
  :diminish (skewer-css-mode . "SkW-CSS"))

;; Skewer HTML
(use-package skewer-html
  :ensure skewer-mode
  :commands skewer-html-mode
  :init
  (add-hook 'html-mode-hook #'skewer-html-mode)
  :diminish (skewer-html-mode . "SkW-HTML"))

;; Undo Tree
(use-package undo-tree
  :ensure t
  :commands global-undo-tree-mode
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,(locate-user-emacs-file "undo"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t)
  :diminish (undo-tree-mode . "UnT"))

;; Visual Fill Column
(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :init
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;; Volatile Highlights
(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :init
  (add-hook 'after-init-hook #'volatile-highlights-mode)
  :diminish (volatile-highlights-mode . "VlH"))

;; Which Key
(use-package which-key
  :ensure t
  :bind ("C-c h K" . which-key-show-top-level)
  :commands which-key-mode
  :init
  (setq which-key-idle-delay 2.0)
  (setq which-key-idle-secondary-delay 1.0)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (add-hook 'after-init-hook #'which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c &" "yasnippet"
    "C-c @" "hide-show"
    "C-c O" "outline"
    "C-c W" "eyebrowse"
    "C-c a" "applications"
    "C-c c" "compile-and-comments"
    "C-c d" "debbugs"
    "C-c f v" "variables"
    "C-c f" "files"
    "C-c h 4" "help-other-window"
    "C-c h" "help"
    "C-c i" "insertion"
    "C-c m" "multiple-cursors"
    "C-c n" "navigation"
    "C-c o" "organisation"
    "C-c p t" "hl-todo"
    "C-c p" "project"
    "C-c s" "search-and-symbols"
    "C-c t" "toggles"
    "C-c v" "version-control"
    "C-c w" "windows-and-frames"
    "C-c x" "text"
    "C-x C-a" "edebug"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x r" "register"
    "C-x w" "highlight"))

;; YASnippet
(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :init
  (add-hook 'after-init-hook #'yas-global-mode))

;; Display personal bindings
(bind-key "C-c h b" #'describe-personal-keybindings)

;; Toggle debug on error
(bind-key "C-c t d" #'toggle-debug-on-error)

;; Revert buffer
(bind-key "C-c f R" #'revert-buffer)

;; Ruler mode
(bind-key "C-c t r" #'ruler-mode)

;; Ediff
(bind-key "C-c f e" #'ediff)

;; Calculator
(bind-key "C-c a c" #'calc)

;; ANSI Term
(bind-key "C-c a t" #'ansi-term)

;; Grep
(bind-key "C-c s p" #'grep)

;; Project
(bind-key "C-c p f" #'project-find-file)
(bind-key "C-c p r" #'project-find-regexp)

;; EWW
(bind-key "C-c a b" #'eww)

;; Find function and variable definitions
(bind-key "C-c h f" #'find-function)
(bind-key "C-c h 4 f" #'find-function-other-window)
(bind-key "C-c h k" #'find-function-on-key)
(bind-key "C-c h v" #'find-variable)
(bind-key "C-c h 4 v" #'find-variable-other-window)

;; Indent region
(bind-key "C-c x i" #'indent-region)

;; Cycle spacing
(bind-key [remap just-one-space] #'cycle-spacing)

;; Sort lines alphabetically
(bind-key "C-c x l" #'sort-lines)

;; Tildify mode
(bind-key "C-c x t" #'tildify-region)

;; Auto Fill mode
(bind-key "C-c t f" #'auto-fill-mode)

;; Align
(bind-key "C-c x A" #'align)
(bind-key "C-c x a" #'align-current)
(bind-key "C-c x r" #'align-regexp)

;; Auto Insert
(bind-key "C-c i a" #'auto-insert)

;; Commenting
(bind-key "C-c c d" #'comment-dwim)
(bind-key "C-c c r" #'comment-region)
(bind-key "C-c c u" #'uncomment-region)

;; Local variable insertion
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v f" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

;; Replace dabbrev-expand with hippie-expand
(bind-key [remap dabbrev-expand] #'hippie-expand)

;; Load changes from the customize interface
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
