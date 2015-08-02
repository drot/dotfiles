(defvar drot/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defvar drot/cache-directory (expand-file-name "cache" drot/emacs-directory)
  "This directory houses all cache files.")
(make-directory drot/cache-directory t)

(defvar drot/custom-file (expand-file-name "custom.el" drot/emacs-directory)
  "Store changes from the customize interface in the selected file.")

;; Prefer newest version of a file
(setq load-prefer-newer t)

;; Activate packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(use-package zenburn-theme
  :ensure t)

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

;; Require a final new line
(setq require-final-newline t)

;; Don't show the welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Disable site default settings
(setq inhibit-default-init t)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0)

;; Don't use dialogs for minibuffer input
(setq use-dialog-box nil)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Cycle completion on smaller number of candidates
(setq completion-cycle-threshold 5)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Increase default fill width
(setq-default fill-column 80)

;; Kill and yank clipboard options
(setq x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;; Set fallback font
(set-fontset-font t nil (font-spec :family "Symbola") nil 'append)

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

;; Pretty lambdas
(global-prettify-symbols-mode)

;; Allow scrolling during Isearch
(setq isearch-allow-scroll t)

;; Define prefix commands for personal keybindings
(defmacro drot/define-group (prefix name &optional map)
  "Define a group at PREFIX with NAME in MAP."
  (let ((command (intern (format "group:%s" name))))
    `(progn
       (define-prefix-command ',command)
       (bind-key ,prefix #',command ,map))))

(drot/define-group "C-c a" applications)
(drot/define-group "C-c c" compile-and-comments)
(drot/define-group "C-c f" files)
(drot/define-group "C-c h" help)
(drot/define-group "C-c i" insertion)
(drot/define-group "C-c n" navigation)
(drot/define-group "C-c s" search-and-symbols)
(drot/define-group "C-c t" toggles)
(drot/define-group "C-c v" version-control)
(drot/define-group "C-c w" windows-and-frames)
(drot/define-group "C-c x" text)

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
        recentf-auto-cleanup 300
        recentf-max-saved-items 100
        recentf-max-menu-items 20)
  (recentf-mode))

;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saved-places" drot/cache-directory))
  (setq-default save-place t))

;; Highlight matching parentheses
(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed)
  (show-paren-mode))

;; Electric pair mode
(use-package elec-pair
  :config
  (electric-pair-mode))

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
  (setq flyspell-use-meta-tab nil)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; Outline mode
(use-package outline
  :config
  (dolist (hook '(text-mode-hook
                  prog-mode-hook))
    (add-hook hook 'outline-minor-mode)))

;; Hide Show mode
(use-package hideshow
  :config
  (dolist (hook '(c-mode-common-hook
                  emacs-lisp-mode-hook
                  python-mode-hook))
    (add-hook hook 'hs-minor-mode)))

;; Use Ibuffer for buffer list
(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Dired
(use-package dired
  :defer t
  :config
  (require 'dired-x)
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-dwim-target t))

;; Dired-x
(use-package dired-x
  :bind ("C-x C-j" . dired-jump))

;; Indent region
(use-package indent
  :bind ("C-c x i" . indent-region))

;; Align
(use-package align
  :bind ("C-c x a" . align))

;; Whitespace mode
(use-package whitespace
  :bind (("C-c x w" . whitespace-cleanup)
         ("C-c t w" . whitespace-mode)))

;; Comment region
(use-package newcomment
  :bind (("C-c c k" . comment-region)
         ("C-c c u" . uncomment-region)))

;; Display personal bindings
(bind-key "C-c h b" 'describe-personal-keybindings)

;; Toggle debug on error
(bind-key "C-c t d" 'toggle-debug-on-error)

;; Find function and variable definitions
(use-package find-func
  :bind (("C-c h f" . find-function)
         ("C-c h 4 f" . find-function-other-window)
         ("C-c h k" . find-function-on-key)
         ("C-c h v" . find-variable)
         ("C-c h 4 v" . find-variable-other-window)))

;; Replace dabbrev-expand with hippie-expand
(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

;; Regexp builder
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

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
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'compilation-shell-minor-mode))

;; Disable YASnippet in term mode
(use-package term
  :bind ("C-c a t" . ansi-term)
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
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
  :config
  (setq compilation-scroll-output 'first-error
        compilation-ask-about-save nil)

  (require 'ansi-color)
  (defun drot/colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook 'drot/colorize-compilation-buffer))

;; CC mode configuration
(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4)
  (setcar (nthcdr 2 c-default-style) '(other . "k&r"))

  (add-hook 'c-mode-common-hook 'auto-fill-mode))

;; NXML mode
(use-package nxml-mode
  :defer t
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

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
  :bind ("C-c a w" . display-time-world)
  :config
  (setq display-time-world-list '(("Europe/Riga" "Riga")
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("Canada/Eastern" "Quebec"))))

;; Doc View mode configuration
(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 300
        doc-view-continuous t))

;; Open URLs in Firefox
(use-package browse-url
  :bind ("C-c a u" . browse-url)
  :config
  (setq browse-url-browser-function 'browse-url-firefox))

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
  :ensure t
  :bind ([remap other-window] . ace-window))

;; Avy
(use-package avy
  :ensure t
  :bind (("C-c n s" . avy-isearch)
         ("C-c n c" . avy-goto-char)
         ("C-c n j" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c j" . avy-goto-word-1)
         ("C-c n w" . avy-goto-word-0)))

;; Browse kill ring
(use-package browse-kill-ring
  :ensure t
  :bind ("C-c i y" . browse-kill-ring))

;; Company mode
(use-package company
  :ensure t
  :diminish "co"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-c i c" . company-yasnippet)
  :config
  (setq company-echo-delay 0
        company-show-numbers t
        company-backends '(company-nxml
                           company-css
                           company-capf (company-dabbrev-code company-keywords)
                           company-files
                           company-dabbrev)))

;; Easy-kill
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-c x e" . er/expand-region))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c v v" . magit-status)
         ("C-c v c" . magit-clone)
         ("C-c v b" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (expand-file-name "mc-lists.el" drot/cache-directory)))

;; Org-mode
(use-package org
  :bind (("C-c a a" . org-agenda)
         ("C-c a l" . org-store-link))
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
    :ensure t
    :config
    (setq rcirc-styles-color-vector ["#7F7F7F" "#CC9393" "#7F9F7F" "#D0BF8F"
                                     "#6CA0A3" "#DC8CC3" "#93E0E3" "#DCDCCC"
                                     "#9F9F9F" "#DCA3A3" "#BFEBBF" "#F0DFAF"
                                     "#8CD0D3" "#DC8CC3" "#93E0E3" "#FFFFEF"]))

  (use-package rcirc-color
    :ensure t
    :config
    (setq rcirc-colors (append rcirc-styles-color-vector nil)))

  (use-package rcirc-notify
    :ensure t
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
  :ensure t
  :bind (("C-c x m" . hydra-multiple-cursors/body)
         ("C-c w r" . hydra-window-resize/body)
         ("C-c x o" . hydra-outline/body))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "

^Up^                ^Down^                  ^Miscellaneous^
-------------------------------------------------------
[_m_]: Mark Next    [_M_]: Mark Previous    [_l_]: Edit Lines
[_s_]: Skip Next    [_S_]: Skip Previous    [_a_]: Mark All
[_u_]: Unmark Next  [_U_]: Unmark Previous

"
    ("m" mc/mark-next-like-this)
    ("s" mc/skip-to-next-like-this)
    ("u" mc/unmark-next-like-this)

    ("M" mc/mark-previous-like-this)
    ("S" mc/skip-to-previous-like-this)
    ("U" mc/unmark-previous-like-this)

    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("q" nil "Quit"))

  (defhydra hydra-window-resize (:columns 2)
    "Resize Windows"
    ("j" enlarge-window "Enlarge Window")
    ("k" shrink-window "Shrink Window")
    ("l" enlarge-window-horizontally "Enlarge Window Horizontally")
    ("h" shrink-window-horizontally "Shrink Window Horizontally")
    ("b" balance-windows "Balance Windows" :exit t)
    ("q" nil "Quit"))

  (defhydra hydra-outline (:hint nil)
    "

^Hide^             ^Show^           ^Move^
-------------------------------------------------------
[_q_]: Sub-levels  [_a_]: All       [_u_]: Up
[_t_]: Body        [_e_]: Entry     [_n_]: Next Visible
[_o_]: Other       [_i_]: Children  [_p_]: Previous Visible
[_c_]: Entry       [_k_]: Branches  [_f_]: Forward Same Level
[_l_]: Leaves      [_s_]: Sub-tree  [_b_]: Backward Same Level
[_d_]: Sub-tree

"
    ("q" hide-sublevels)
    ("t" hide-body)
    ("o" hide-other)
    ("c" hide-entry)
    ("l" hide-leaves)
    ("d" hide-subtree)

    ("a" show-all)
    ("e" show-entry)
    ("i" show-children)
    ("k" show-branches)
    ("s" show-subtree)

    ("u" outline-up-heading)
    ("n" outline-next-visible-heading)
    ("p" outline-previous-visible-heading)
    ("f" outline-forward-same-level)
    ("b" outline-backward-same-level)
    ("z" nil "Quit")))

;; Swiper
(use-package swiper
  :bind (("C-c s s" . swiper)
         ("C-c s i" . swiper-from-isearch)))

;; ivy
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-c f r" . ivy-recentf)
         ("C-c t r" . ivy-resume)))

;; Counsel
(use-package counsel
  :ensure t
  :init
  (bind-keys* ([remap execute-extended-command] . counsel-M-x)
              ([remap find-file] . counsel-find-file)
              ([remap describe-variable] . counsel-describe-variable)
              ([remap describe-function] . counsel-describe-function))
  :config
  (setq counsel-find-file-at-point t))

;; Lispy
(use-package lispy
  :ensure t
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

;; nLinum mode
(use-package nlinum
  :ensure t
  :bind ("C-c t l" . nlinum-mode))

;; Page break lines mode
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))

;; Volatile Highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;; Undo Tree
(use-package undo-tree
  :ensure t
  :diminish "UT"
  :config
  (setq undo-tree-history-directory-alist backup-directory-alist
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; YASnippet
(use-package yasnippet
  :ensure t
  :init
  (make-directory (expand-file-name "snippets" drot/emacs-directory) t)
  :config
  (setq yas-verbosity 1)
  (yas-global-mode))

;; Zop-to-char
(use-package zop-to-char
  :ensure t
  :bind ([remap zap-to-char]. zop-to-char))

;; Load changes from the customize interface
(setq custom-file drot/custom-file)
(load drot/custom-file 'noerror 'nomessage)
