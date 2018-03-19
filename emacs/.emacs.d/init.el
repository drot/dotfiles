;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2018 drot

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
          (lambda () (setq gc-cons-threshold 800000)))

;; Create directories for backups and cache files
(make-directory (locate-user-emacs-file "backups") t)
(make-directory (locate-user-emacs-file "cache") t)

;; Detect `window-system' properly with daemon usage
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions #'run-after-make-frame-hooks)

(defconst drot|initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when drot|initial-frame
                  (run-after-make-frame-hooks drot|initial-frame))))

;; Disable the site default settings
(setq inhibit-default-init t)

;; Prefer newest version of a file
(setq load-prefer-newer t)

;; Activate packages and add the MELPA package archive
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Pinned packages
(setq package-pinned-packages '((json-mode . "gnu")))

;; Helper function for installing packages
(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Use a shorter alias for this commonly used macro
(defalias 'after-load 'with-eval-after-load)

;; Use `bind-key' for personal keybindings
(require-package 'bind-key)
;; Set key binding
(bind-key "C-c h b" #'describe-personal-keybindings)
;; Configuration
(after-load 'bind-key
  ;; Extract special forms
  (setq bind-key-describe-special-forms t))

;; Use `delight' for mode name shortening
(require-package 'delight)

;; Color theme
(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; Disable needless GUI elements
(dolist (mode '(tool-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

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

;; Always scroll evenly with the mouse
(setq mouse-wheel-progressive-speed nil)

;; Use visual bell instead
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

;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Sentence ends with single space
(setq sentence-end-double-space nil)

;; Increase default fill width
(setq-default fill-column 80)

;; Require a final new line
(setq require-final-newline t)

;; Set Text mode as the default major mode
(setq-default major-mode #'text-mode)

;; Enable Auto Fill mode for Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)
(delight 'auto-fill-function " fL" t)

;; Visual Line mode configuration
(setq visual-line-fringe-indicators '(nil vertical-bar))
(delight 'visual-line-mode " vL" t)

;; Use Gnus as the default mail program
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command #'gnus)

;; Put underline below the font bottom line
(setq x-underline-at-descent-line t)

;; Draw block cursor as wide as the glyph under it
(setq x-stretch-cursor t)

;; Highlight region even in non-selected windows
(setq highlight-nonselected-windows t)

;; Enable faster scrolling
(setq fast-but-imprecise-scrolling t)

;; Resize windows proportionally
(setq window-combination-resize t)

;; Draw window dividers
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(setq window-divider-default-places t)
(window-divider-mode)

;; Change recenter initial position
(setq recenter-positions '(top middle bottom))

;; Prompt for buffer switch in strongly dedicated windows
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; Display read-only buffers in view mode
(setq view-read-only t)
(setq view-inhibit-help-message t)

;; Kill and yank clipboard saving
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
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backups") t)))
(setq auto-save-list-file-prefix (locate-user-emacs-file "backups/.saves-"))
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "backups"))))
(setq version-control t)
(setq kept-new-versions 6)
(setq delete-old-versions t)
(setq backup-by-copying t)

;; Save minibuffer history
(setq savehist-file (locate-user-emacs-file "cache/saved-history"))
(setq savehist-autosave-interval 60)
(setq savehist-additional-variables '(search-ring regexp-search-ring))
;; Initialize mode
(savehist-mode)

;; Save recent files list
(setq recentf-save-file (locate-user-emacs-file "cache/recent-files"))
(setq recentf-exclude
      '("/\\.git/.*\\'"
        "/elpa/.*\\'"
        "/image-dired/.*\\'"
        "/elfeed/.*\\'"
        "/cache/.*\\'"
        ".*\\.gz\\'"
        "TAGS"))
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 20)
(setq recentf-auto-cleanup 600)
;; Initialize mode
(recentf-mode)

;; Remember point position in files
(setq save-place-file (locate-user-emacs-file "cache/saved-places"))
;; Initialize mode
(save-place-mode)

;; Line numbers display
(setq display-line-numbers-type 'relative)
;; Display line numbers only in relevant modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode)

;; Highlight matching parentheses
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
;; Initialize mode
(show-paren-mode)

;; Highlight regexps interactively
(setq hi-lock-auto-select-face t)
;; Initialize mode
(global-hi-lock-mode)

;; Abbrev mode
(delight 'abbrev-mode " aB" t)
;; Configuration
(setq abbrev-file-name (expand-file-name "abbrevs" user-emacs-directory))
(setq save-abbrevs t)
;; Load abbrevs if they exist
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
;; Initialize mode
(setq-default abbrev-mode t)

;; Electric pair mode
(setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
;; Initialize mode
(electric-pair-mode)

;; Prettify certain symbols
(setq prettify-symbols-unprettify-at-point t)
;; Initialize mode
(global-prettify-symbols-mode)

;; Which function mode
(setq which-func-unknown "n/a")
;; Initialize mode
(which-function-mode)

;; Fast window switching
(setq windmove-wrap-around t)
;; Initialize mode
(windmove-default-keybindings)

;; Undo and redo the window configuration
(winner-mode)
;; Configuration
(after-load 'winner
  ;; Set key bindings
  (bind-keys :map winner-mode-map
             ("C-c w u" . winner-undo)
             ("C-c w r" . winner-redo))
  ;; Disable conflicting key bindings
  (unbind-key "C-c <left>" winner-mode-map)
  (unbind-key "C-c <right>" winner-mode-map))

;; Hide Show mode
(dolist (hook '(c-mode-common-hook
                emacs-lisp-mode-hook
                python-mode-hook))
  (add-hook hook #'hs-minor-mode))
;; Configuration
(after-load 'hideshow
  (defun drot|hs-display-code-line-counts (ov)
    "Unique overlay function to be applied with `hs-minor-mode'."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (format "... / %d"
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))
  ;; Unfold when search is active and apply custom overlay
  (setq hs-set-up-overlay #'drot|hs-display-code-line-counts)
  (setq hs-isearch-open t))

;; Bug Reference mode
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; Configuration
(after-load 'bug-reference
  (setq bug-reference-url-format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"))

;; Goto Address mode
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;; Fly Spell mode configuration
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; Configuration
(after-load 'flyspell
  ;; Shorten mode lighter
  (delight 'flyspell-mode " fS" t)
  ;; Set key bindings
  (bind-key "C-c l b" #'flyspell-buffer)
  (bind-key "C-c l r" #'flyspell-region)
  (bind-keys :map flyspell-mode-map
             ("C-c l c" . flyspell-correct-word-before-point)
             ("C-c l p" . flyspell-check-previous-highlighted-word))
  ;; Disable conflicting key bindings
  (unbind-key "C-c $" flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  ;; Correct some annoying defaults
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-consider-dash-as-word-delimiter-flag t)
  (setq flyspell-duplicate-distance 12000))

;; Ispell
(bind-key "C-c l d" #'ispell-change-dictionary)
;; Configuration
(after-load 'ispell
  (setq ispell-program-name "hunspell"))

;; Isearch
(delight 'isearch-mode " iS" t)
(setq isearch-allow-scroll t)
(setq search-default-mode #'char-fold-to-regexp)

;; Ediff window split
(after-load 'ediff-wind
  ;; Configuration
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-grab-mouse nil))

;; Ediff restore previous window configuration
(after-load 'ediff-util
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; Uniquify buffer names
(after-load 'uniquify
  ;; Configuration
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Use Ibuffer for buffer list
(bind-key [remap list-buffers] #'ibuffer)
;; Configuration
(after-load 'ibuffer
  (setq ibuffer-default-sorting-mode 'major-mode))

;; Version control
(after-load 'vc-hooks
  ;; Configuration
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq vc-follow-symlinks t)
  (setq vc-make-backup-files t))

;; Customize interface options
(after-load 'cus-edit
  ;; Configuration
  (setq custom-buffer-done-kill t)
  (setq custom-buffer-verbose-help nil)
  (setq custom-unlispify-tag-names nil)
  (setq custom-unlispify-menu-entries nil))

;; Treat all themes as safe
(after-load 'custom
  ;; Configuration
  (setq custom-safe-themes t))

;; Auto Revert mode
(after-load 'autorevert
  ;; Shorten mode lighter
  (delight 'auto-revert-mode " aR" t))

;; Imenu configuration
(after-load 'imenu
  ;; Always rescan buffers
  (setq imenu-auto-rescan t))

;; Pcomplete configuration
(after-load 'pcomplete
  ;;Ignore case sensitivity with Pcomplete
  (setq pcomplete-ignore-case t))

;; ElDoc mode configuration
(after-load 'eldoc
  ;; Shorten mode lighter
  (delight 'eldoc-mode " eD" t)
  ;; Make compatible with Paredit
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round))

;; Python mode configuration
(after-load 'python
  ;; Use Python 3 as default
  (setq python-shell-interpreter "python3")
  ;; Disable indent offset guessing
  (setq python-indent-guess-indent-offset nil)
  ;; PEP8 conformance
  (add-hook 'python-mode-hook
            (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)
  ;; Flymake configuration
  (setq python-flymake-command '("pyflakes3")))

;; CC mode
(add-to-list 'auto-mode-alist '("\\.fos\\'" . c++-mode))
;; Configuration
(after-load 'cc-mode
  (setq c-basic-offset 4)
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "k&r")))
  (add-hook 'c-mode-common-hook #'auto-fill-mode))

;; Etags
(after-load 'etags
  ;; Configuration
  (setq tags-file-name "TAGS"))

;; Scheme mode
(after-load 'scheme
  ;; Use Guile as the default interpreter
  (setq scheme-program-name "guile"))

;; CSS mode
(after-load 'css-mode
  ;; Configuration
  (setq css-indent-offset 2))

;; NXML mode
(after-load 'nxml-mode
  ;; Configuration
  (setq nxml-slash-auto-complete-flag t)
  (setq nxml-sexp-element-flag t))

;; Doc View mode
(after-load 'doc-view
  ;; Configuration
  (setq doc-view-resolution 300)
  (setq doc-view-continuous t))

;; Colorize ANSI escape sequences
(after-load 'ansi-color
  ;; Colorization function
  (defun drot|ansi-color-compilation-buffer ()
    "Colorize the compilation mode buffer"
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  ;; Apply colorization
  (add-hook 'compilation-filter-hook #'drot|ansi-color-compilation-buffer))

;; Enable Pass integration
(after-load 'auth-source
  ;; Initialize mode
  (auth-source-pass-enable))

;; Mail sending
(bind-key "C-c i m" #'message-mark-inserted-region)
(bind-key "C-c i f" #'message-mark-insert-file)
;; Configuration
(after-load 'message
  ;; Customize
  (setq message-confirm-send t)
  (setq message-kill-buffer-on-exit t))

;; Outgoing mail server
(after-load 'smtpmail
  ;; Configuration
  (setq smtpmail-smtp-server "mail.cock.li")
  (setq smtpmail-smtp-user "drot-smtp")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

;; Smileys
(after-load 'smiley
  ;; Configuration
  (setq smiley-style 'medium))

;; Network Security Manager
(after-load 'nsm
  ;; Change default settings file location
  (setq nsm-settings-file (locate-user-emacs-file "cache/network-security.data")))

;; Prevent GnuTLS warnings
(after-load 'gnutls
  ;; Configuration
  (setq gnutls-min-prime-bits nil))

;; Dired configuration
(after-load 'dired
  ;; Load Dired Extra library for additional features
  (require 'dired-x)
  ;; Default `ls' switches
  (setq dired-listing-switches "-alhFG")
  ;; If we are on a GNU system add some more `ls' switches
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first")))
  ;; Do certain operations recursively
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'always)
  ;; Imitate orthodox file managers with two buffers open
  (setq dired-dwim-target t))

;; Dired Extra configuration
(after-load 'dired-x
  ;; Shorten Dired Omit mode lighter
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (delight 'dired-omit-mode " oT" t))
                '((name . dired-omit-mode-delight))))

;; Wdired movement and editable parts
(after-load 'wdired
  ;; Configuration
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-use-dired-vertical-movement 'sometimes))

;; Image-Dired
(after-load 'image-dired
  ;; Change default external viewer
  (setq image-dired-external-viewer "pqiv"))

;; TRAMP
(after-load 'tramp
  ;; Configuration
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (locate-user-emacs-file "cache/tramp"))
  (setq tramp-backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq tramp-auto-save-directory temporary-file-directory))

;; Bookmarks
(after-load 'bookmark
  ;; Configuration
  (setq bookmark-default-file (locate-user-emacs-file "cache/bookmark"))
  (setq bookmark-save-flag 1))

;; Find file at point
(after-load 'ffap
  ;; Configuration
  (setq ffap-machine-p-known 'reject))

;; Search more extensively with apropos
(bind-key "C-c h a" #'apropos)
;; Configuration
(after-load 'apropos
  (setq apropos-do-all t))

;; Copyright insertion
(bind-key "C-c i c" #'copyright)
(bind-key "C-c i C" #'copyright-update)
;; Configuration
(after-load 'copyright
  (setq copyright-year-ranges t)
  (setq copyright-names-regexp (regexp-quote user-login-name)))

;; Whitespace mode
(bind-key "C-c x w" #'whitespace-cleanup)
(bind-key "C-c t w" #'whitespace-mode)
;; Shorten mode lighter
(after-load 'whitespace
  (delight 'whitespace-mode " wS" t))

;; Tildify mode
(bind-key "C-c x t" #'tildify-region)
(bind-key "C-c t ~" #'tildify-mode)
;; Configuration
(add-hook 'LaTeX-mode-hook
          (lambda () (setq-local tildify-space-string "~")))

;; Regexp builder
(bind-key "C-c s b" #'re-builder)
;; Configuration
(after-load 're-builder
  (setq reb-re-syntax 'string))

;; Proced
(bind-key "C-c a P" #'proced)
;; Configuration
(after-load 'proced
  (setq-default proced-sort 'start)
  (setq-default proced-tree-flag t))

;; GDB
(bind-key "C-c a d" #'gdb)
;; Configuration
(after-load 'gdb-mi
  (setq gdb-many-windows t))

;; EWW
(bind-key "C-c a w w" #'eww)
(bind-key "C-c a w b" #'eww-list-bookmarks)
;; Configuration
(after-load 'eww
  ;; Set bookmarks directory
  (setq eww-bookmarks-directory (locate-user-emacs-file "cache")))

;; Open URLs with the specified browser
(bind-key "C-c n b" #'browse-url)
(bind-key "C-c n p" #'browse-url-at-point)
;; Configuration
(after-load 'browse-url
  (setq browse-url-browser-function #'browse-url-chromium))

;; Speedbar
(bind-key "C-c p s" #'speedbar)
;; Configuration
(after-load 'speedbar
  ;; Set key binding
  (bind-key "a" #'speedbar-toggle-show-all-files speedbar-mode-map)
  ;; Emulate NERDTree behavior
  (setq speedbar-use-images nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-directory-unshown-regexp "^$")
  ;; Don't ignore the following extensions
  (speedbar-add-supported-extension
   '(".lisp" ".clj" ".lua" ".css" ".patch"
     ".conf" ".diff" ".sh" ".org" ".md" ".deb")))

;; Eshell
(bind-key "C-c a e" #'eshell)
;; Configuration
(after-load 'eshell
  (setq eshell-hist-ignoredups t)
  (setq eshell-cmpl-ignore-case t)
  ;; Custom hook to avoid conflicts
  (defun drot|eshell-mode-hook ()
    "Use alternate completions and disable Company in Eshell buffers."
    (define-key eshell-mode-map [remap eshell-pcomplete] #'completion-at-point)
    (define-key eshell-mode-map [remap eshell-previous-matching-input-from-input] #'counsel-esh-history)
    (company-mode 0))
  (add-hook 'eshell-mode-hook #'drot|eshell-mode-hook))

;; Eshell smart display
(after-load 'eshell
  ;; Initialize mode
  (require 'em-smart)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize))

;; Shell mode
(bind-key "C-c a t" #'shell)
;; Configuration
(after-load 'shell
  (add-hook 'shell-mode-hook #'compilation-shell-minor-mode))

;; IELM
(bind-key "C-c a '" #'ielm)
;; Configuration
(after-load 'ielm
  (setq ielm-prompt "EL> "))

;; Flymake
(bind-key "C-c ! t" #'flymake-mode)
;; Configuration
(after-load 'flymake
  ;; Define Hydra
  (defhydra hydra-flymake (:columns 2)
    "Flymake"
    ("n" flymake-goto-next-error "Next Error")
    ("p" flymake-goto-prev-error "Previous Error")
    ("q" nil "Quit"))
  ;; Set key bindings
  (bind-keys :map flymake-mode-map
             ("C-c ! n" . flymake-goto-next-error)
             ("C-c ! p" . flymake-goto-prev-error)
             ("C-c ! R" . flymake-reporting-backends)
             ("C-c ! r" . flymake-running-backends)
             ("C-c ! d" . flymake-disabled-backends)
             ("C-c ! l" . flymake-switch-to-log-buffer)
             ("C-c ! h" . hydra-flymake/body)))

;; Compilation
(bind-key "C-c c C" #'recompile)
;; Configuration
(after-load 'compile
  ;; Shorten mode lighter
  (delight 'compilation-shell-minor-mode " sC" t)
  ;; Customize
  (setq compilation-ask-about-save nil)
  (setq compilation-always-kill t)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-context-lines 3))

;; Gnus
(bind-key "C-c a g" #'gnus)
;; Configuration
(after-load 'gnus
  ;; Set key bindings
  (bind-key "C-c n o" #'ace-link-gnus gnus-summary-mode-map)
  (bind-key "C-c n o" #'ace-link-gnus gnus-article-mode-map)
  ;; Configure mail and news server
  (setq gnus-select-method
        '(nnimap "mail.cock.li"
                 (nnimap-address "mail.cock.li")
                 (nnimap-user "drot")
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

;; Calendar
(bind-key "C-c a c" #'calendar)
;; Configuration
(after-load 'calendar
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

;; Outline mode
(bind-key "C-c t o" #'outline-minor-mode)
;; Set default prefix
(setq outline-minor-mode-prefix (kbd "C-c O"))
;; Configuration
(after-load 'outline
  ;; Shorten mode lighter
  (delight 'outline-minor-mode " oL" t)
  ;; Define Hydra
  (defhydra hydra-outline (:columns 4)
    "Outline Mode"
    ("z" outline-hide-sublevels "Hide Sub-Levels")
    ("t" outline-hide-body "Hide Body")
    ("o" outline-hide-other "Hide Other")
    ("c" outline-hide-entry "Hide Entry")
    ("l" outline-hide-leaves "Hide Leaves")
    ("d" outline-hide-subtree "Hide Sub-Tree")
    ("a" outline-show-all "Show All")
    ("e" outline-show-entry "Show Entry")
    ("i" outline-show-children "Show Children")
    ("k" outline-show-branches "Show Branches")
    ("s" outline-show-subtree "Show Sub-Tree")
    ("u" outline-up-heading "Up Heading")
    ("n" outline-next-visible-heading "Next Visible Heading")
    ("p" outline-previous-visible-heading "Previous Visible Heading")
    ("f" outline-forward-same-level "Forward Same Level")
    ("b" outline-backward-same-level "Backward Same Level")
    ("q" nil "Quit"))
  ;; Set key binding
  (bind-key "C-c O h" #'hydra-outline/body outline-minor-mode-map))

;; Org-mode
(bind-key "C-c o a" #'org-agenda)
(bind-key "C-c o c" #'org-capture)
(bind-key "C-c o t" #'org-todo-list)
(bind-key "C-c o s" #'org-search-view)
(bind-key "C-c o l" #'org-store-link)
;; Configuration
(after-load 'org
  ;; Set key binding
  (bind-key "C-c n o" #'ace-link-org org-mode-map)
  ;; Customize
  (setq org-directory (locate-user-emacs-file "org"))
  (setq org-default-notes-file (locate-user-emacs-file "org/notes.org"))
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
(bind-key "C-c a C" #'display-time-world)
;; Configuration
(after-load 'time
  ;; Configuration
  (setq display-time-world-list
        '(("Europe/Riga" "Riga")
          ("America/Los_Angeles" "Los Angeles")
          ("Canada/Eastern" "Toronto")
          ("Asia/Saigon" "Saigon")
          ("UTC" "Universal"))))

;; Ace-window
(require-package 'ace-window)
;; Configuration
(after-load 'ace-window
  ;; Shorten mode lighter
  (delight 'ace-window-mode " aW" t))

;; Anaconda mode
(require-package 'anaconda-mode)
;; Initialize mode
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'anaconda-eldoc-mode)
;; Shorten mode lighter
(after-load 'anaconda-mode
  (delight 'anaconda-mode " aC" t))

;; Company Anaconda
(require-package 'company-anaconda)
;; Initialize mode
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'company-backends #'company-anaconda)))

;; AUCTeX
(require-package 'auctex)

;; TeX configuration
(after-load 'tex
  ;; Default TeX engine
  (setq-default TeX-engine 'luatex)
  ;; Automatically save and parse style
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Insert macros with a backslash
  (setq TeX-electric-escape t)
  ;; Use PDF Tools as default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  ;; Revert PDF automatically
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; TeX external commands
(after-load 'tex-buf
  ;; Don't ask to save before processing
  (setq TeX-save-query nil))

;; LaTeX configuration
(after-load 'latex
  ;; Enable Flymake `tex-chktex' backend with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (add-hook 'flymake-diagnostic-functions #'tex-chktex nil t)))
  ;; Enable Flymake syntax checking
  (add-hook 'LaTeX-mode-hook #'flymake-mode)
  ;; Enable folding options
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  ;; Further folding options with Outline
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  ;; Add RefTeX support
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

;; RefTeX
(after-load 'reftex
  ;; Shorten mode lighter
  (delight 'reftex-mode " rF" t)
  ;; Enable AUCTeX integration
  (setq reftex-plug-into-AUCTeX t))

;; CIDER
(require-package 'cider)
;; Configuration
(after-load 'cider-common
  ;; Use the symbol at point as the default value
  (setq cider-prompt-for-symbol nil))

;; CIDER mode configuration
(after-load 'cider-mode
  ;; Enable fuzzy completion with Company
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;; CIDER REPL configuration
(after-load 'cider-repl
  ;; Enable persistent history
  (setq cider-repl-history-file (locate-user-emacs-file "cache/cider-history"))
  (setq cider-repl-wrap-history t)
  ;; Disable help banner
  (setq cider-repl-display-help-banner nil)
  ;; Display result prefix
  (setq cider-repl-result-prefix "; => ")
  ;; Enable fuzzy completion with Company
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; Enable SubWord mode
  (add-hook 'cider-repl-mode-hook #'subword-mode))

;; Clojure mode
(require-package 'clojure-mode)
;; Configuration
(after-load 'clojure-mode
  ;; Enable SubWord mode
  (add-hook 'clojure-mode-hook #'subword-mode))

;; Dash
(require-package 'dash)
;; Configuration
(after-load 'dash
  ;; Enable syntax coloring for Dash functions
  (dash-enable-font-lock))

;; Debbugs
(require-package 'debbugs)
;; Set key bindings
(bind-key "C-c d g" #'debbugs-gnu)
(bind-key "C-c d s" #'debbugs-gnu-search)
(bind-key "C-c d t" #'debbugs-gnu-usertags)
(bind-key "C-c d p" #'debbugs-gnu-patches)
(bind-key "C-c d b" #'debbugs-gnu-bugs)
(bind-key "C-c d O" #'debbugs-org)
(bind-key "C-c d S" #'debbugs-org-search)
(bind-key "C-c d P" #'debbugs-org-patches)
(bind-key "C-c d B" #'debbugs-org-bugs)

;; Dired Filter
(require-package 'dired-filter)
;; Configuration
(after-load 'dired-x
  ;; Initialize mode
  (require 'dired-filter)
  ;; Shorten mode lighter
  (delight 'dired-filter-mode " fR" t)
  ;; Set key binding
  (bind-key "\\" dired-filter-mark-map dired-mode-map))

;; Dired Rainbow
(require-package 'dired-rainbow)
;; Configuration
(after-load 'dired-filter
  ;; Initialize mode
  (require 'dired-rainbow)
  ;; Define faces by file type
  (dired-rainbow-define audio "#329EE8" ("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV"))
  (dired-rainbow-define compressed "tomato" ("zip" "bz2" "tgz" "txz" "gz" "xz"
                                             "z" "Z" "jar" "war" "ear" "rar"
                                             "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define document "peru" ("doc" "docx" "odt" "pdb" "pdf" "ps"
                                         "rtf" "djvu" "epub"))
  (dired-rainbow-define encrypted "salmon" ("gpg" "pgp" "rsa"))
  (dired-rainbow-define excel "turquise" ("xlsx"))
  (dired-rainbow-define executable "Gold" ("exe" "msi"))
  (dired-rainbow-define html "Wheat" ("htm" "html" "xhtml"))
  (dired-rainbow-define image "goldenrod" ("jpg" "png" "jpeg" "gif"))
  (dired-rainbow-define log "gray" ("log"))
  (dired-rainbow-define packaged "khaki" ("deb" "rpm"))
  (dired-rainbow-define sourcefile "SandyBrown" ("py" "c" "cc" "h" "java" "pl"
                                                 "rb" "R" "php" "el" "scm" "cpp"
                                                 "fos" "lisp" "clj" "lua" "lisp"))
  (dired-rainbow-define video "#B3CCFF" ("vob" "VOB" "mkv" "MKV" "mpe" "mpg"
                                         "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
                                         "M2TS" "avi" "AVI" "mov" "MOV" "wmv"
                                         "asf" "m2v" "m4v" "mpeg" "MPEG" "tp"))
  (dired-rainbow-define xml "RosyBrown" ("xml" "xsd" "xsl" "xslt" "wsdl"))
  ;; Define faces by file permission
  (dired-rainbow-define-chmod executable-unix "Gold" "-[rw-]+x.*")
  (dired-rainbow-define-chmod directory-unix "DeepSkyBlue" "d[rw-]+x.*")
  (dired-rainbow-define-chmod symlink-unix "SpringGreen" "l[rw-]+x.*"))

;; Dired Subtree
(require-package 'dired-subtree)
;; Configuration
(after-load 'dired-rainbow
  ;; Initialize mode
  (require 'dired-subtree)
  ;; Set key bindings
  (bind-keys :map dired-mode-map
             :prefix "C-,"
             :prefix-map dired-subtree-map
             :prefix-docstring "Dired Subtree map."
             ("C-i" . dired-subtree-insert)
             ("C-/" . dired-subtree-apply-filter)
             ("C-k" . dired-subtree-remove)
             ("C-n" . dired-subtree-next-sibling)
             ("C-p" . dired-subtree-previous-sibling)
             ("C-u" . dired-subtree-up)
             ("C-d" . dired-subtree-down)
             ("C-a" . dired-subtree-beginning)
             ("C-e" . dired-subtree-end)
             ("C-c" . dired-subtree-cycle)
             ("m" . dired-subtree-mark-subtree)
             ("u" . dired-subtree-unmark-subtree)
             ("C-o C-f" . dired-subtree-only-this-file)
             ("C-o C-d" . dired-subtree-only-this-directory)))

;; Dired Ranger
(require-package 'dired-ranger)
;; Configuration
(after-load 'dired-subtree
  ;; Initialize mode
  (require 'dired-ranger)
  ;; Set key bindings
  (bind-keys :map dired-mode-map
             :prefix "r"
             :prefix-map dired-ranger-map
             :prefix-docstring "Dired Ranger map."
             ("c" . dired-ranger-copy)
             ("p" . dired-ranger-paste)
             ("m" . dired-ranger-move))
  ;; Bookmarking
  (bind-keys :map dired-mode-map
             ("'" . dired-ranger-bookmark)
             ("`" . dired-ranger-bookmark-visit)))

;; Dired Narrow
(require-package 'dired-narrow)
;; Configuration
(after-load 'dired-ranger
  ;; Initialize mode
  (require 'dired-narrow)
  ;; Shorten mode lighter
  (delight 'dired-narrow-mode " d-N" t)
  ;; Set key binding
  (bind-key "C-." #'dired-narrow dired-mode-map))

;; Dired Collapse
(require-package 'dired-collapse)
;; Configuration
(after-load 'dired-narrow
  ;; Initialize mode
  (require 'dired-collapse)
  ;; Set key binding
  (bind-key "," #'dired-collapse-mode dired-mode-map))

;; Dired-du
(require-package 'dired-du)
;; Configuration
(after-load 'dired-collapse
  ;; Initialize mode
  (require 'dired-du)
  ;; Shorten mode lighter
  (delight 'dired-du-mode " d-U" t)
  ;; Use human readable output by default
  (setq dired-du-size-format t))

;; Dired Async
(require-package 'async)
;; Configuration
(after-load 'dired-du
  ;; Initialize mode
  (require 'dired-async)
  ;; Shorten mode lighter
  (delight 'dired-async-mode '(:eval (when (eq major-mode 'dired-mode) " aS")) t)
  ;; Set key bindings
  (bind-keys :map dired-mode-map
             ("E c" . dired-async-do-copy)
             ("E r" . dired-async-do-rename)
             ("E s" . dired-async-do-symlink)
             ("E h" . dired-async-do-hardlink)
             ("E m" . dired-async-mode)))

;; Asynchronous SMTP mail sending
(after-load 'message
  ;; Load async library
  (require 'smtpmail-async)
  ;; Change default mail sending function
  (setq message-send-mail-function #'async-smtpmail-send-it)
  ;; Enable compatibility with the Pass password manager
  (add-hook 'async-smtpmail-before-send-hook #'auth-source-pass-enable))

;; Easy-kill
(require-package 'easy-kill)
(bind-key [remap kill-ring-save] #'easy-kill)
(bind-key [remap mark-sexp] #'easy-mark)

;; Elfeed
(require-package 'elfeed)
;; Set key binding
(bind-key "C-c a f" #'elfeed)
;; Configuration
(after-load 'elfeed
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" hnews)
          ("https://lwn.net/headlines/rss" lwn)
          ("https://www.reddit.com/r/emacs/.rss" emacs)
          ("https://www.reddit.com/r/linux/.rss" linux)
          ("https://www.reddit.com/r/linux/.rss" programming)
          ("http://bljesak.info/rss" bljesak)))
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-search-date-format '("%d-%m-%Y" 10 :left))
  (setq elfeed-search-filter "@1-week-ago +unread"))

;; ERC
(defun drot|erc-init ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Connect to IRC? ")
    ;; Rizon
    (erc-tls :server "irc.rizon.net" :port 6697
             :nick "drot")
    ;; ForestNet
    (erc-tls :server "irc.forestnet.org" :port 6697
             :nick "drot")))

;; Set key binding
(bind-key "C-c a i" #'drot|erc-init)

;; ERC highlight nicknames
(require-package 'erc-hl-nicks)

;; ERC configuration
(after-load 'erc
  ;; Load ERC services mode for Rizon authentication
  (erc-services-mode)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-identify-mode 'autodetect)
  (setq erc-nickserv-passwords
        `((Rizon (("drot" . ,(auth-source-pass-get 'secret "auth-sources/drot@irc.rizon.net"))))))
  ;; Connect to specified servers
  (setq erc-prompt-for-password nil)
  (setq erc-autojoin-timing 'ident)
  (setq erc-server-reconnect-timeout 30)
  ;; Configure text filling
  (setq erc-fill-function #'erc-fill-static)
  (setq erc-fill-column 150)
  (setq erc-fill-static-center 10)
  ;; Timestap formatting
  (setq erc-insert-timestamp-function #'erc-insert-timestamp-left)
  (setq erc-timestamp-only-if-changed-flag nil)
  (setq erc-timestamp-format "[%H:%M] ")
  ;; Text formatting
  (setq erc-header-line-format "%t: %o")
  (setq erc-interpret-mirc-color t)
  (setq erc-button-buttonize-nicks nil)
  (setq erc-format-nick-function #'erc-format-@nick)
  (setq erc-nick-uniquifier "_")
  (setq erc-prompt
        (lambda () (concat (buffer-name) ">")))
  ;; Channel tracking options
  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-showcount t)
  (setq erc-track-switch-direction 'importance)
  (setq erc-track-visibility 'selected-visible)
  ;; Hide lurker activity
  (setq erc-lurker-threshold-time 3600)
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  ;; Open query buffers in the current window
  (setq erc-query-display 'buffer)
  ;; Kill all buffers upon ERC quit
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  ;; Prevent accidental paste
  (setq erc-accidental-paste-threshold-seconds 0.5)
  ;; Disable some conflicting modes
  (defun drot|erc-mode-hook ()
    "Keep prompt at bottom and disable Company and YASnippet in ERC buffers."
    (set (make-local-variable 'scroll-conservatively) 1000)
    (company-mode 0))
  (add-hook 'erc-mode-hook #'drot|erc-mode-hook)
  ;; Enable notifications
  (erc-notifications-mode)
  ;; Enable spell-checking
  (erc-spelling-mode)
  ;; Truncate buffer
  (setq erc-truncate-buffer-on-save t)
  (add-hook 'erc-insert-post-hook #'erc-truncate-buffer))

;; Expand region
(require-package 'expand-region)
;; Set key binding
(bind-key "C-=" #'er/expand-region)

;; Flx
(require-package 'flx)

;; Geiser
(require-package 'geiser)
;; Configuration
(after-load 'geiser
  (setq geiser-repl-history-filename (locate-user-emacs-file "cache/geiser-history")))

;; IEdit
(require-package 'iedit)
;; Autoload missing functions
(autoload #'iedit-mode-from-isearch "iedit"
  "Start Iedit mode using last search string as the regexp." t)
(autoload #'iedit-execute-last-modification "iedit"
  "Apply last modification in Iedit mode to the current buffer or an active region." t)
;; Set key bindings
(bind-key "C-c t i" #'iedit-mode)
(bind-key "C-;" #'iedit-mode-from-isearch isearch-mode-map)
(bind-key "C-;" #'iedit-execute-last-modification esc-map)
(bind-key "C-;" #'iedit-mode-toggle-on-function help-map)
;; Configuration
(after-load 'iedit
  (setq iedit-toggle-key-default nil))

;; JavaScript mode
(require-package 'js2-mode)
;; Initialize mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Configuration
(after-load 'js2-mode
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;; JSON mode
(require-package 'json-mode)

;; Lua mode
(require-package 'lua-mode)

;; Hydra
(require-package 'hydra)
;; Configuration
(after-load 'hydra
  ;; Enable syntax coloring for Hydra definitions
  (hydra-add-font-lock))

;; EPUB format support
(require-package 'nov)
;; Initialize package
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; Configuration
(after-load 'nov
  ;; Change default saved places file location
  (setq nov-save-place-file (locate-user-emacs-file "cache/nov-places"))
  ;; Change default font
  (defun drot|nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Noto Serif"
                             :height 1.0))
  (add-hook 'nov-mode-hook 'drot|nov-font-setup)
  ;; Text filling
  (setq nov-text-width 80))

;; Macrostep
(require-package 'macrostep)
;; Set key binding
(bind-key "C-c M-e" #'macrostep-expand emacs-lisp-mode-map)

;; Magit
(require-package 'magit)
;; Set key bindings
(bind-key "C-c v v" #'magit-status)
(bind-key "C-c v d" #'magit-dispatch-popup)
(bind-key "C-c v c" #'magit-clone)
(bind-key "C-c v b" #'magit-blame)
(bind-key "C-c v l" #'magit-log-buffer-file)
(bind-key "C-c v p" #'magit-pull)

;; Markdown mode
(require-package 'markdown-mode)
;; Avoid conflict with `which-key'
(setq markdown-enable-prefix-prompts nil)
;; Configuration
(after-load 'markdown-mode
  ;; Default markdown command
  (setq markdown-command
        (concat "pandoc"
                " -f markdown -t html"
                " -s --mathjax --highlight-style=pygments"))
  ;; Enable `visual-line-mode' in markdown buffers
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  ;; Fontify code blocks
  (setq markdown-fontify-code-blocks-natively t)
  ;; Use underscores for italic text
  (setq markdown-italic-underscore t))

;; Move-text
(require-package 'move-text)
;; Define Hydra
(defhydra hydra-move-text (:columns 2)
  "Move Text"
  ("p" move-text-up "Move Text Up")
  ("n" move-text-down "Move Text Down")
  ("q" nil "Quit"))
;; Set key binding
(bind-key "C-c x M" #'hydra-move-text/body)

;; Multiple cursors
(require-package 'multiple-cursors)
;; Change default `multiple-cursors' lists file location
(setq mc/list-file (locate-user-emacs-file "cache/mc-lists.el"))
;; Populate default `multiple-cursors' lists
(unless (file-exists-p mc/list-file)
  (setq mc/cmds-to-run-for-all
        '(backward-sexp
          downcase-region
          electric-newline-and-maybe-indent
          end-of-buffer
          forward-sexp
          indent-for-tab-command
          kill-region
          paredit-backslash
          paredit-backward
          paredit-close-round
          paredit-close-square
          paredit-comment-dwim
          paredit-convolute-sexp
          paredit-doublequote
          paredit-forward
          paredit-forward-barf-sexp
          paredit-forward-delete
          paredit-forward-down
          paredit-forward-slurp-sexp
          paredit-kill
          paredit-newline
          paredit-open-round
          paredit-open-square
          paredit-reindent-cl-defun
          paredit-semicolon
          paredit-splice-sexp-killing-backward
          paredit-backslash
          reindent-then-newline-and-indent
          scroll-other-window
          slime-autodoc-space
          slime-space
          switch-to-buffer
          upcase-region
          yank-rectangle))
  (setq mc/cmds-to-run-once
        '(down-list
          mouse-drag-mode-line)))
;; Set key bindings
(bind-key "C-c m <SPC>" #'mc/vertical-align-with-space)
(bind-key "C-c m a" #'mc/vertical-align)
(bind-key "C-c m e" #'mc/mark-more-like-this-extended)
(bind-key "C-c m m" #'mc/mark-all-like-this-dwim)
(bind-key "C-c m l" #'mc/edit-lines)
(bind-key "C-c m n" #'mc/mark-next-like-this)
(bind-key "C-c m p" #'mc/mark-previous-like-this)
(bind-key "C-c m C-a" #'mc/edit-beginnings-of-lines)
(bind-key "C-c m C-e" #'mc/edit-ends-of-lines)
(bind-key "C-c m C-s" #'mc/mark-all-in-region)
;; Define Hydra
(defhydra hydra-multiple-cursors (:columns 3)
  "Multiple Cursors"
  ("l" mc/edit-lines "Edit Lines In Region" :exit t)
  ("b" mc/edit-beginnings-of-lines "Edit Beginnings Of Lines In Region" :Exit t)
  ("e" mc/edit-ends-of-lines "Edit Ends Of Lines In Region" :exit t)
  ("a" mc/mark-all-dwim "Mark All Dwim" :exit t)
  ("S" mc/mark-all-symbols-like-this "Mark All Symbols Likes This" :exit t)
  ("w" mc/mark-all-words-like-this "Mark All Words Like This" :exit t)
  ("r" mc/mark-all-in-region "Mark All In Region" :exit t)
  ("R" mc/mark-all-in-region-regexp "Mark All In Region (regexp)" :exit t)
  ("d" mc/mark-all-like-this-in-defun "Mark All Like This In Defun" :exit t)
  ("s" mc/mark-all-symbols-like-this-in-defun "Mark All Symbols Like This In Defun" :exit t)
  ("W" mc/mark-all-words-like-this-in-defun "Mark All Words Like This In Defun" :exit t)
  ("i" mc/insert-numbers "Insert Numbers" :exit t)
  ("n" mc/mark-next-like-this "Mark Next Like This")
  ("N" mc/skip-to-next-like-this "Skip To Next Like This")
  ("M-n" mc/unmark-next-like-this "Unmark Next Like This")
  ("p" mc/mark-previous-like-this "Mark Previous Like This")
  ("P" mc/skip-to-previous-like-this "Skip To Previous Like This")
  ("M-p" mc/unmark-previous-like-this "Unmark Previous Like This")
  ("q" nil "Quit" :exit t))
;; Set key binding
(bind-key "C-c m h" #'hydra-multiple-cursors/body)

;; PDF Tools
(require-package 'pdf-tools)
;; Autoload missing function
(autoload #'pdf-view-mode "pdf-tools"
  "Major mode in PDF buffers." t)
;; Initialize mode
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;; Configuration
(after-load 'pdf-tools
  ;; Make sure the binary is always compiled
  (pdf-tools-install :no-query))

;; Rainbow mode
(require-package 'rainbow-mode)
;; Set key binding
(bind-key "C-c t r" #'rainbow-mode)
;; Configuration
(after-load 'rainbow-mode
  (delight 'rainbow-mode " rW" t))

;; Skewer
(require-package 'skewer-mode)
;; Initialize mode
(add-hook 'js2-mode-hook #'skewer-mode)
;; Set key binding
(bind-key "C-c a W" #'run-skewer)
;; Configuration
(after-load 'skewer-mode
  (delight 'skewer-mode " sK" t))

;; Skewer CSS
(add-hook 'css-mode-hook #'skewer-css-mode)
;; Configuration
(after-load 'skewer-css
  (delight 'skewer-css-mode " sKC" t))

;; Skewer HTML
(add-hook 'mhtml-mode-hook #'skewer-html-mode)
;; Configuration
(after-load 'skewer-html
  (delight 'skewer-html-mode " sKH" t))

;; SLIME
(require-package 'slime)
;; Set key bindings
(bind-key "C-c a s" #'slime)
(bind-key "C-c a S" #'slime-connect)
;; Configuration
(after-load 'slime
  ;; Set key binding
  (bind-key "C-c $" #'slime-export-symbol-at-point slime-mode-indirect-map)
  ;; Disable conflicting key binding
  (unbind-key "C-c x" slime-mode-indirect-map)
  ;; Use all accessible features and SBCL by default
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-company))
  (setq slime-protocol-version 'ignore))

;; Shorten SLIME Autodoc mode lighter
(after-load 'slime-autodoc
  (delight 'slime-autodoc-mode " aD" t))

;; SLIME REPL configuration
(after-load 'slime-repl
  ;; Set key bindings
  (bind-keys :map slime-repl-mode-map
             ("C-c M-r" . slime-repl-previous-matching-input)
             ("C-c M-s" . slime-repl-next-matching-input))
  ;; Disable conflicting key bindings
  (unbind-key "DEL" slime-repl-mode-map)
  (unbind-key "M-r" slime-repl-mode-map)
  (unbind-key "M-s" slime-repl-mode-map)
  ;; Customize
  (setq slime-repl-history-file (locate-user-emacs-file "cache/slime-history.eld"))
  (setq slime-repl-history-remove-duplicates t)
  (setq slime-repl-history-trim-whitespaces t))

;; SLIME Company
(require-package 'slime-company)
;; Configuration
(after-load 'slime-company
  (setq slime-company-completion 'fuzzy))

;; Smex
(require-package 'smex)
;; Configuration
(after-load 'smex
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

;; Systemd mode
(require-package 'systemd)

;; Visual Fill Column
(require-package 'visual-fill-column)
;; Initialize mode
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;; Wgrep
(require-package 'wgrep)

;; YAML mode
(require-package 'yaml-mode)

;; Zop-to-char
(require-package 'zop-to-char)
;; Set key bindings
(bind-key [remap zap-to-char] #'zop-to-char)
(bind-key "M-Z" #'zop-up-to-char)

;; Ace-link
(require-package 'ace-link)
;; Initialize mode
(add-hook 'after-init-hook #'ace-link-setup-default)
;; Configuration
(after-load 'ace-link
  ;; Set key binding
  (bind-key "C-c n a"  #'ace-link-addr))

;; Adaptive Wrap
(require-package 'adaptive-wrap)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; Anzu
(require-package 'anzu)
;; Initialize mode
(add-hook 'after-init-hook #'global-anzu-mode)
;; Configuration
(after-load 'anzu
  ;; Set key bindings
  (bind-key [remap query-replace] #'anzu-query-replace)
  (bind-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
  (bind-keys :map isearch-mode-map
             ([remap isearch-query-replace] . anzu-isearch-query-replace)
             ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  ;; Shorten mode lighter
  (delight 'anzu-mode " aZ" t)
  ;; Customize
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 50)
  (setq anzu-replace-to-string-separator " => "))

;; Avy
(require-package 'avy)
;; Initialize mode
(avy-setup-default)
;; Set key bindings
(bind-key "C-c j c" #'avy-goto-char)
(bind-key "C-c j k" #'avy-goto-char-2)
(bind-key "C-c j w" #'avy-goto-word-0)
(bind-key "C-c j SPC" #'avy-pop-mark)
(bind-key "C-c j l" #'avy-goto-line)
(bind-key "C-c j j" #'avy-goto-word-or-subword-1)
;; Configuration
(after-load 'avy
  (setq avy-all-windows 'all-frames)
  (setq avy-background t)
  (setq avy-highlight-first t))

;; Company mode
(require-package 'company)
;; Initialize mode
(add-hook 'after-init-hook #'global-company-mode)
;; Configuration
(after-load 'company
  ;; Shorten mode lighter
  (delight 'company-mode " cY" t)
  ;; Set key binding
  (bind-key "C-c i y" #'company-yasnippet)
  ;; Customize
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-etags company-keywords)
          company-dabbrev))
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case t)
  ;; Insert candidate as soon as TAB is pressed
  (company-tng-configure-default))

;; Company Statistics
(require-package 'company-statistics)
;; Initialize mode
(add-hook 'after-init-hook #'company-statistics-mode)
;; Configuration
(after-load 'company
  (setq company-statistics-file (locate-user-emacs-file "cache/company-statistics-cache.el")))

;; Diff-Hl
(require-package 'diff-hl)
;; Initialize mode
(add-hook 'after-init-hook #'global-diff-hl-mode)
;; Enable `diff-hl-margin-mode' when in a terminal
(add-hook 'after-make-console-frame-hooks #'diff-hl-margin-mode)
;; Disable `diff-hl-margin-mode' when in a GUI
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (diff-hl-margin-mode 0)))
;; Configuration
(after-load 'diff-hl
  ;; Update diffs immediately
  (diff-hl-flydiff-mode)
  ;; Add hooks for other packages
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Form-feed
(require-package 'form-feed)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                scheme-mode-hook
                compilation-mode-hook
                outline-mode-hook
                help-mode-hook))
  (add-hook hook #'form-feed-mode))

;; Hl-Todo
(require-package 'hl-todo)
;; Initialize mode
(add-hook 'after-init-hook #'global-hl-todo-mode)
;; Configuration
(after-load 'hl-todo
  ;; Define Hydra
  (defhydra hydra-hl-todo (:columns 2)
    "Highlight TODO"
    ("n" hl-todo-next "Next TODO")
    ("p" hl-todo-previous "Previous TODO")
    ("q" nil "Quit"))
  ;; Set key bindings
  (bind-keys :map hl-todo-mode-map
             ("C-c p t n" . hl-todo-next)
             ("C-c p t p" . hl-todo-previous)
             ("C-c p t o" . hl-todo-occur)
             ("C-c p t h" . hydra-hl-todo/body)))

;; Ivy
(require-package 'ivy)
;; Ivy Hydra support
(require-package 'ivy-hydra)
;; Initialize mode
(add-hook 'after-init-hook #'ivy-mode)
;; Configuration
(after-load 'ivy
  ;; Shorten mode lighter
  (delight 'ivy-mode " iY" t)
  ;; Set key binding
  (bind-key "C-c n i" #'ivy-resume)
  ;; Customize
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-dynamic-exhibit-delay-ms 150)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-format-function #'ivy-format-function-arrow)
  (setq ivy-wrap t)
  (setq ivy-action-wrap t))

;; Counsel
(require-package 'counsel)
;; Initialize mode
(add-hook 'after-init-hook #'counsel-mode)
;; Configuration
(after-load 'counsel
  ;; Shorten mode lighter
  (delight 'counsel-mode " cS" t)
  ;; Set key bindings
  (bind-key "C-c f g" #'counsel-git)
  (bind-key "C-c f j" #'counsel-dired-jump)
  (bind-key "C-c f r" #'counsel-recentf)
  (bind-key "C-c s G" #'counsel-git-grep)
  (bind-key "C-c s i" #'counsel-imenu)
  (bind-key "C-c s g" #'counsel-grep)
  (bind-key "C-c j m" #'counsel-mark-ring)
  (bind-key "C-c h c" #'counsel-command-history)
  (bind-key "C-c h l" #'counsel-find-library)
  (bind-key "C-c i 8" #'counsel-unicode-char)
  ;; Customize
  (setq counsel-preselect-current-file t)
  (setq counsel-find-file-at-point t))

;; Swiper
(require-package 'swiper)
;; Set key bindings
(bind-key "C-c s S" #'swiper-all)
(bind-key "C-c s s" #'swiper)
(bind-key "C-c S" #'swiper-from-isearch isearch-mode-map)
;; Configure
(after-load 'swiper
  (setq swiper-include-line-number-in-search t))

;; Hyperbole
(require-package 'hyperbole)
;; Configuration
(setq hbmap:dir-user (locate-user-emacs-file "hyperbole/"))
;; Set key bindings
(add-hook 'hyperbole-init-hook
          (lambda ()
            ;; Add ace-window support
            (hkey-ace-window-setup (kbd "M-o"))
            ;; Remap default bindings
            (bind-key "C-c ," #'hui-select-thing)
            (bind-key "C-c R" #'hui:ebut-rename)
            (bind-key "C-c |" #'hycontrol-windows-grid)
            (bind-key "C-c C-\\" #'hkey-operate)))
;; Initialize mode
(add-hook 'after-init-hook
          (lambda () (require 'hyperbole)))

;; Paredit
(require-package 'paredit)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                clojure-mode-hook
                cider-repl-mode-hook
                scheme-mode-hook
                slime-repl-mode-hook
                geiser-repl-mode-hook))
  (add-hook hook #'enable-paredit-mode))
;; Configuration
(after-load 'paredit
  ;; Shorten mode lighter
  (delight 'paredit-mode " pE" t)
  ;; Set key bindings
  (bind-keys :map paredit-mode-map
             ("M-{" . paredit-wrap-curly)
             ("M-[" . paredit-wrap-square))
  ;; Enable Paredit in the minibuffer
  (defvar drot--paredit-minibuffer-setup-commands
    '(eval-expression
      pp-eval-expression
      eval-expression-with-eldoc
      ibuffer-do-eval
      ibuffer-do-view-and-eval)
    "Interactive commands for which Paredit should be enabled in the minibuffer.")

  (defun drot|paredit-minibuffer-setup ()
    "Enable Paredit during lisp-related minibuffer commands."
    (if (memq this-command drot--paredit-minibuffer-setup-commands)
        (enable-paredit-mode)))
  (add-hook 'minibuffer-setup-hook #'drot|paredit-minibuffer-setup)
  ;; Disable Electric Pair mode when Paredit is active
  (add-hook 'paredit-mode-hook
            (lambda () (setq-local electric-pair-mode nil))))

;; Rainbow Delimiters
(require-package 'rainbow-delimiters)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                scheme-mode-hook))
  (add-hook hook #'rainbow-delimiters-mode))

;; Volatile Highlights
(require-package 'volatile-highlights)
;; Initialize mode
(add-hook 'after-init-hook #'volatile-highlights-mode)
;; Configuration
(after-load 'volatile-highlights
  (delight 'volatile-highlights-mode " vH" t))

;; Which Key
(require-package 'which-key)
;; Initialize mode
(setq which-key-idle-delay 2.0)
(setq which-key-idle-secondary-delay 1.0)
(setq which-key-allow-imprecise-window-fit t)
(setq which-key-sort-order #'which-key-prefix-then-key-order)
(add-hook 'after-init-hook #'which-key-mode)
;; Set key bindings
(bind-key "C-c h w t" #'which-key-show-top-level)
(bind-key "C-c h w m" #'which-key-show-major-mode)
(bind-key "C-c h w k" #'which-key-show-full-keymap)
;; Configuration
(after-load 'which-key
  ;; Global replacements
  (which-key-add-key-based-replacements
    "C-c !" "flymake"
    "C-c &" "yasnippet"
    "C-c @" "hide-show"
    "C-c O" "outline"
    "C-c a w" "eww"
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c d" "debbugs"
    "C-c f v" "variables"
    "C-c f" "files"
    "C-c h 4" "help-other-window"
    "C-c h w" "which-key"
    "C-c h" "help-extended"
    "C-c i" "insertion"
    "C-c j" "jump"
    "C-c l" "language-and-spelling"
    "C-c m" "multiple-cursors"
    "C-c n" "navigation"
    "C-c o" "organization"
    "C-c p t" "hl-todo"
    "C-c p" "project"
    "C-c s" "search-and-symbols"
    "C-c t" "toggles"
    "C-c v" "version-control"
    "C-c w" "windows"
    "C-c x a" "align"
    "C-c x" "text"
    "C-x C-a" "edebug"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x r" "register"
    "C-x w" "highlight")
  ;; Dired mode replacements
  (which-key-add-major-mode-key-based-replacements 'dired-mode
    "C-, C-o" "dired-subtree-only"
    "C-," "dired-subtree"
    "\\" "dired-filter"
    "r" "dired-ranger")
  ;; AUCTeX replacements
  (which-key-add-major-mode-key-based-replacements 'latex-mode
    "C-c C-o" "TeX-fold-mode"
    "C-c C-p" "preview-latex"
    "C-c C-q" "LaTeX-fill"
    "C-c C-t" "TeX-toggle")
  ;; Markdown mode replacements
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c TAB" "markdown-images"
    "C-c C-a" "markdown-links"
    "C-c C-c" "markdown-commands"
    "C-c C-s" "markdown-styles"
    "C-c C-t" "markdown-header"
    "C-c C-x" "markdown-toggles"))

;; YASnippet
(require-package 'yasnippet)
;; Initialize mode
(add-hook 'after-init-hook #'yas-global-mode)
;; Configuration
(after-load 'yasnippet
  (delight 'yas-minor-mode " yS" t))

;; Toggle debug on error
(bind-key "C-c t d" #'toggle-debug-on-error)

;; Ruler mode
(bind-key "C-c t R" #'ruler-mode)

;; Variable pitch mode
(bind-key "C-c t v" #'variable-pitch-mode)

;; Ediff
(bind-key "C-c f e" #'ediff)
(bind-key "C-c f 3" #'ediff3)

;; ANSI Term
(bind-key "C-c a T" #'ansi-term)

;; Hexl mode
(bind-key "C-c t h" #'hexl-mode)
(bind-key "C-c f h" #'hexl-find-file)

;; Replace string immediately
(bind-key "C-c s r" #'replace-string)
(bind-key "C-c s R" #'replace-regexp)

;; Grep results as a dired buffer
(bind-key "C-c s d" #'find-grep-dired)

;; Project
(bind-key "C-c p f" #'project-find-file)
(bind-key "C-c p r" #'project-find-regexp)

;; Find function and variable definitions
(bind-key "C-c h f" #'find-function)
(bind-key "C-c h 4 f" #'find-function-other-window)
(bind-key "C-c h k" #'find-function-on-key)
(bind-key "C-c h v" #'find-variable)
(bind-key "C-c h 4 v" #'find-variable-other-window)

;; Find library
(bind-key "C-c h 4 l" #'find-library-other-window)
(bind-key "C-c h 4 L" #'find-library-other-frame)

;; List packages
(bind-key "C-c a p" #'package-list-packages)

;; Cycle spacing
(bind-key [remap just-one-space] #'cycle-spacing)

;; Sort lines alphabetically
(bind-key "C-c x l" #'sort-lines)

;; Auto Fill mode
(bind-key "C-c t f" #'auto-fill-mode)

;; Align
(bind-key "C-c x a a" #'align)
(bind-key "C-c x a c" #'align-current)
(bind-key "C-c x a r" #'align-regexp)

;; Auto Insert
(bind-key "C-c i a" #'auto-insert)

;; Commenting
(bind-key "C-c c d" #'comment-dwim)
(bind-key "C-c c r" #'comment-or-uncomment-region)

;; Hydra for various text marking operations
(defhydra hydra-mark-text (:exit t :columns 4)
  "Mark Text"
  ("e" mark-sexp "S-Expression")
  ("f" er/mark-defun "Function")
  ("w" er/mark-word "Word")
  ("u" er/mark-url "URL")
  ("E" er/mark-email "Email")
  ("p" er/mark-text-paragraph "Paragraph")
  ("s" er/mark-symbol "Symbol")
  ("S" er/mark-symbol-with-prefix "Prefixed Symbol")
  ("q" er/mark-inside-quotes "Inside Quotes")
  ("Q" er/mark-outside-quotes "Outside Quotes")
  ("(" er/mark-inside-pairs "Inside Pairs")
  ("[" er/mark-inside-pairs "Inside Pairs")
  ("{" er/mark-inside-pairs "Inside Pairs")
  (")" er/mark-outside-pairs "Outside Pairs")
  ("]" er/mark-outside-pairs "Outside Pairs")
  ("}" er/mark-outside-pairs "Outside Pairs")
  ("c" er/mark-comment "Comment")
  ("." er/expand-region "Expand Region" :exit nil)
  ("," er/contract-region "Contract Region" :exit nil))
;; Set key binding
(bind-key "C-c x m" #'hydra-mark-text/body)

;; Matching lines operation
(bind-key "C-c s l" #'delete-matching-lines)
(bind-key "C-c s L" #'delete-non-matching-lines)

;; Local variable insertion
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v f" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

;; Extended buffer operation key bindings
(bind-key "C-c b DEL" #'erase-buffer)
(bind-key "C-c b b" #'bury-buffer)
(bind-key "C-c b u" #'unbury-buffer)
(bind-key "C-c b e" #'eval-buffer)
(bind-key "C-c b k" #'kill-this-buffer)
(bind-key "C-c b g" #'revert-buffer)
(bind-key "C-c b i" #'insert-buffer)

;; Replace dabbrev-expand with hippie-expand
(bind-key [remap dabbrev-expand] #'hippie-expand)

;; Load changes from the customize interface
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
