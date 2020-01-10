;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2020 drot

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

;;; Create directories for backups and cache files
(make-directory (locate-user-emacs-file "backup") t)
(make-directory (locate-user-emacs-file "cache") t)

;;; Disable needless GUI elements
(dolist (mode '(tool-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Color theme
(require-package 'color-theme-sanityinc-tomorrow)
;; Load theme explicitly
(load-theme 'sanityinc-tomorrow-night t)

;;; Don't show the startup welcome messages
(setq inhibit-startup-screen t)
(fset 'display-startup-echo-area-message #'ignore)

;;; Disable scratch buffer info text
(setq initial-scratch-message nil)

;;; Show column number and buffer size on the mode line
(column-number-mode)
(size-indication-mode)

;;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p #'y-or-n-p)

;;; Use a shorter alias for this commonly used macro
(defalias 'after-load 'with-eval-after-load)

;;; Show unfinished keystrokes early
(setq echo-keystrokes 0.01)

;;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;;; Show window dividers
(setq window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      window-divider-default-places t)
;; Initialize mode
(window-divider-mode)

;;; Move point all the way to buffer boundary before signaling an error
(setq scroll-error-top-bottom t)

;;; Always scroll evenly with the mouse
(setq mouse-wheel-progressive-speed nil)

;;; Enable faster scrolling
(setq fast-but-imprecise-scrolling t)

;;; Use visual bell instead
(setq visible-bell t)

;;; Don't use dialogs for minibuffer input
(setq use-dialog-box nil)

;;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;;; Cycle completion on smaller number of candidates
(setq completion-cycle-threshold 5)

;;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;;; Indicate minibuffer recursion depth
(minibuffer-depth-indicate-mode)

;;; Enable all disabled commands
(setq disabled-command-function nil)

;;; No length limit when printing values
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;;; Use spaces instead of tabs and set default tab width
(setq-default indent-tabs-mode nil
              tab-width 4)

;;; Sentence ends with single space
(setq sentence-end-double-space nil)

;;; Increase default fill width
(setq-default fill-column 80)

;;; Require a final new line
(setq require-final-newline t)

;;; Set Text mode as the default major mode
(setq-default major-mode #'text-mode)

;;; Enable Auto Fill mode for Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)

;;; Visual Line mode configuration
(setq visual-line-fringe-indicators '(nil right-triangle))

;;; Use Gnus as the default mail program
(setq mail-user-agent 'gnus-user-agent
      read-mail-command #'gnus)

;;; Put underline below the font bottom line
(setq x-underline-at-descent-line t)

;;; Resize windows proportionally
(setq window-combination-resize t)

;;; Prompt for buffer switch in strongly dedicated windows
(setq switch-to-buffer-in-dedicated-window 'prompt)

;;; Display read-only buffers in view mode
(setq view-read-only t
      view-inhibit-help-message t)

;;; Kill and yank clipboard saving
(setq save-interprogram-paste-before-kill t)

;;; Mouse yank at point instead of click
(setq mouse-yank-at-point t)

;;; Increase maximum size of the mark ring
(setq mark-ring-max 30)

;;; Repeat mark popping
(setq set-mark-command-repeat-pop t)

;;; Rotate the clipboard when rotating the kill ring
(setq yank-pop-change-selection t)

;;; Do not save duplicates
(setq history-delete-duplicates t
      kill-do-not-save-duplicates t)

;;; Configuration for backup files
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backup/")))
      version-control t
      kept-new-versions 6
      delete-old-versions t
      backup-by-copying t)

;;; Auto save file configuration
(setq auto-save-list-file-prefix (locate-user-emacs-file "backup/auto-save/.saves-")
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backup/auto-save/") t)))

;;; Save minibuffer history
(setq savehist-file (locate-user-emacs-file "cache/saved-history")
      savehist-autosave-interval 60
      savehist-additional-variables '(search-ring regexp-search-ring))
;; Initialize mode
(savehist-mode)

;;; Save recent files list
(setq recentf-save-file (locate-user-emacs-file "cache/recent-files")
      recentf-max-saved-items 100
      recentf-max-menu-items 20
      recentf-auto-cleanup 600)
;; Exclude certain files
(setq recentf-exclude
      '("/\\.git/.*\\'"
        "/elpa/.*\\'"
        "/image-dired/.*\\'"
        "/backup/.*\\'"
        "/elfeed/.*\\'"
        "/cache/.*\\'"
        "/dev/shm/.*\\'"
        ".*\\.gz\\'"
        "newsrc"
        "TAGS"))
;; Initialize mode
(recentf-mode)

;;; Remember point position in files
(setq save-place-file (locate-user-emacs-file "cache/saved-places"))
;; Initialize mode
(save-place-mode)

;;; Line numbers display
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute nil)
;; Maximum width reserved for line numbers
(setq-default display-line-numbers-width 2)
;; Display line numbers only in relevant modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;;; Display fill column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)

;;; Highlight current line
(global-hl-line-mode)
;; Disable `hl-line-mode' in special buffers
(dolist (hook '(artist-mode-hook
                comint-mode-hook
                ediff-mode-hook
                eshell-mode-hook
                nov-mode-hook
                rcirc-mode-hook
                term-mode-hook
                undo-tree-visualizer-mode-hook
                vterm-mode-hook
                cider-repl-mode-hook))
  (add-hook hook
            (lambda () (setq-local global-hl-line-mode nil))))

;;; Highlight matching parentheses
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
;; Initialize mode
(show-paren-mode)

;;; Highlight regexps interactively
(setq hi-lock-auto-select-face t)
;; Initialize mode
(global-hi-lock-mode)

;;; Abbrev mode
(setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
;; Don't ask for save confirmation
(setq save-abbrevs 'silently)
;; Load abbrevs if they exist
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
;; Initialize mode
(setq-default abbrev-mode t)

;;; Electric pair mode
(electric-pair-mode)

;;; Prettify-Symbols mode
(setq prettify-symbols-unprettify-at-point t)
;; Initialize mode
(global-prettify-symbols-mode)

;;; Which function mode
(setq which-func-unknown "n/a")
;; Initialize mode
(which-function-mode)

;;; Wind Move fast window switching
(windmove-default-keybindings)
;; Set key bindings to pick next window display position
(windmove-display-default-keybindings)
;; Set key bindings to delete windows
(windmove-delete-default-keybindings)
;; Configuration
(after-load 'windmove
  ;; Cycle windows
  (setq windmove-wrap-around t))

;;; Undo and redo the window configuration
(setq winner-dont-bind-my-keys t)
;; Set key bindings
(global-set-key (kbd "<C-s-left>") #'winner-undo)
(global-set-key (kbd "<C-s-right>") #'winner-redo)
;; Initialize mode
(winner-mode)

;;; Hide Show mode
(dolist (hook '(c-mode-common-hook
                emacs-lisp-mode-hook
                python-mode-hook))
  (add-hook hook #'hs-minor-mode))
;; Configuration
(after-load 'hideshow
  ;; Custom overlay function
  (defun drot/hs-display-code-line-counts (ov)
    "Unique overlay function to be applied with `hs-minor-mode'."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize
                    (format " ... / %d"
                            (count-lines (overlay-start ov)
                                         (overlay-end ov)))
                    'face 'font-lock-comment-face))))
  ;; Use nesting
  (setq hs-allow-nesting t)
  ;; Unfold when search is active and apply custom overlay
  (setq hs-set-up-overlay #'drot/hs-display-code-line-counts
        hs-isearch-open t))

;;; Bug Reference mode
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
(add-hook 'gnus-article-mode-hook #'bug-reference-mode)
;; Configuration
(after-load 'bug-reference
  ;; GNU bug tracker
  (setq bug-reference-url-format "https://debbugs.gnu.org/%s"))

;;; Goto Address mode
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;;; Fly Spell mode configuration
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; Set global key bindings
(global-set-key (kbd "C-c l b") #'flyspell-buffer)
(global-set-key (kbd "C-c l r") #'flyspell-region)
;; Configuration
(after-load 'flyspell
  ;; Disable conflicting key binding
  (define-key flyspell-mode-map (kbd "C-M-i") nil)
  ;; Correct some annoying defaults
  (setq flyspell-use-meta-tab nil
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        flyspell-consider-dash-as-word-delimiter-flag t)
  ;; Don't slowdown looking for duplicates
  (setq flyspell-duplicate-distance 12000))

;;; Ispell
(global-set-key (kbd "C-c l d") #'ispell-change-dictionary)
;; Configuration
(after-load 'ispell
  ;; Issue warning if missing program
  (unless ispell-program-name
    (warn "No spell checker available."))
  ;; Extra switches
  (setq ispell-extra-args '("--sug-mode=ultra"))
  ;; Default dictionary
  (setq ispell-dictionary "english"))

;;; Isearch
(setq isearch-allow-scroll t)
;; Match similar chars
(setq search-default-mode #'char-fold-to-regexp)
;; Display number of matches
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) ")

;;; Ediff window split
(after-load 'ediff-wind
  ;; Two windows, side by side
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally)
  ;; Don't touch the mouse
  (setq ediff-grab-mouse nil))

;; Ediff restore previous window configuration
(after-load 'ediff-util
  ;; Clever hack using `winner-undo'
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;;; Uniquify buffer names
(after-load 'uniquify
  ;; Configuration
  (setq uniquify-buffer-name-style 'forward
        uniquify-trailing-separator-p t
        uniquify-ignore-buffers-re "^\\*"))

;;; Tab bar
(after-load 'tab-bar
  ;; Don't show on single tab
  (setq tab-bar-show 1)
  ;; Display numbers on tabs
  (setq tab-bar-tab-hints t))

;;; Use Ibuffer for buffer list
(global-set-key [remap list-buffers] #'ibuffer)
;; Configuration
(after-load 'ibuffer
  ;; Use a default buffer filter
  (setq ibuffer-saved-filter-groups
        '(("primary"
           ("Calculator" (or (mode . calc-mode)
                             (mode . calc-trail-mode)))
           ("Code" (and (or (derived-mode . prog-mode)
                            (derived-mode . conf-mode)
                            (derived-mode . sgml-mode))
                        (not (name . "*scratch*"))))
           ("Compilation" (derived-mode . compilation-mode))
           ("Custom" (derived-mode . Custom-mode))
           ("Dired" (mode . dired-mode))
           ("Document" (or (derived-mode . latex-mode)
                           (derived-mode . markdown-mode)))
           ("IRC" (mode . rcirc-mode))
           ("Ediff" (or (mode . ediff-mode)
                        (mode . ediff-meta-mode)
                        (name . "*ediff-errors*")
                        (name . "*ediff-diff*")
                        (name . "*ediff-fine-diff*")
                        (name . "*ediff-custom-diff*")))
           ("Emacs" (or (name . "*scratch*")
                        (mode . messages-buffer-mode)))
           ("Git" (derived-mode . magit-mode))
           ("Gnus" (or (derived-mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (derived-mode . gnus-article-mode)))
           ("Help" (or (derived-mode . help-mode)
                       (derived-mode . apropos-mode)
                       (derived-mode . Info-mode)))
           ("Image" (mode . image-mode))
           ("Log" (or (derived-mode . TeX-output-mode)
                      (derived-mode . ivy-occur-mode)
                      (derived-mode . geiser-messages-mode)
                      (mode . tags-table-mode)
                      (name . "*nrepl-server")
                      (name . "*Flymake log*")
                      (name . "*sly-inferior-lisp for sbcl*")
                      (name . "*sly-events for sbcl*")
                      (name . "*inferior-lisp*")
                      (name . "*Warnings*")))
           ("Mail" (or (derived-mode . message-mode)
                       (derived-mode . mail-mode)))
           ("Org" (derived-mode . org-mode))
           ("Packages" (derived-mode . package-menu-mode))
           ("PDF" (derived-mode . pdf-view-mode))
           ("REPL" (or (derived-mode . cider-repl-mode)
                       (derived-mode . geiser-repl-mode)
                       (derived-mode . sly-mrepl-mode)
                       (derived-mode . skewer-repl-mode)
                       (name . "*Python*")))
           ("Shell" (or (derived-mode . eshell-mode)
                        (derived-mode . shell-mode)))
           ("Terminal" (or (derived-mode . term-mode)
                           (derived-mode . vterm-mode)))
           ("Text" (mode . text-mode))
           ("TRAMP" (name . "*tramp")))))
  ;; Load the default buffer filter
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "primary")))
  ;; Don't show empty filter groups and jump only to visible buffers
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-jump-offer-only-visible-buffers t)
  ;; Split window instead for buffer display
  (setq ibuffer-use-other-window t))

;;; Version control
(after-load 'vc-hooks
  ;; TRAMP speedup
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; Don't ask to follow symlinks
  (setq vc-follow-symlinks t)
  ;; Make backups even under version control
  (setq vc-make-backup-files t))

;;; Customize interface
(global-set-key (kbd "<C-f9>") #'customize-group)
;; Configuration
(after-load 'cus-edit
  ;; Kill buffer when done and shorten help
  (setq custom-buffer-done-kill t
        custom-buffer-verbose-help nil)
  ;; Display entries as words
  (setq custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil))

;;; Custom theme configuration
(after-load 'custom
  ;; Treat themes as safe
  (setq custom-safe-themes t))

;;; MIME decoding configuration
(after-load 'mm-decode
  ;; Fit images to buffer
  (setq mm-inline-large-images 'resize))

;;; Image mode
(after-load 'image-mode
  ;; Show image dimension function
  (defun drot/image-dimensions-minor-mode ()
    "Display image dimensions in the mode line."
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format " (%dx%d)" width height))))
  ;; Apply the custom hook
  (add-hook 'image-mode-hook #'drot/image-dimensions-minor-mode)
  ;; Loop animated images forever
  (setq image-animate-loop t)
  ;; Loop animated image automatically
  (add-hook 'image-mode-hook #'image-toggle-animation))

;;; Change default print command
(setq lpr-command "lp")
;; Configuration
(after-load 'lpr
  ;; Don't add extra switches
  (setq lpr-add-switches nil))

;;; Imenu configuration
(after-load 'imenu
  ;; Always rescan buffers
  (setq imenu-auto-rescan t))

;;; Pcomplete configuration
(after-load 'pcomplete
  ;;Ignore case sensitivity with Pcomplete
  (setq pcomplete-ignore-case t))

;;; Apropos configuration
(after-load 'apropos
  ;; Search more extensively
  (setq apropos-do-all t))

;;; ElDoc mode
(after-load 'eldoc
  ;; Make compatible with Paredit
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round))

;;; Python mode configuration
(after-load 'python
  ;; Use Python 3 as default
  (setq python-shell-interpreter "python3")
  ;; Disable indent offset guessing
  (setq python-indent-guess-indent-offset nil)
  ;; PEP8 conformance
  (add-hook 'python-mode-hook
            (lambda () (setq fill-column 79)))
  ;; Enable SubWord mode
  (add-hook 'python-mode-hook #'subword-mode))

;;; CC mode
(add-to-list 'auto-mode-alist '("\\.fos\\'" . c++-mode))
;; Configuration
(after-load 'cc-mode
  ;; Default indentation
  (setq c-basic-offset 4)
  ;; Indentation of supported modes
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "k&r")))
  ;; Enable Auto Fill
  (add-hook 'c-mode-common-hook #'auto-fill-mode))

;;; Etags
(after-load 'etags
  ;; Default filename
  (setq tags-file-name "TAGS"))

;;; Scheme mode
(after-load 'scheme
  ;; Use Guile as the default interpreter
  (setq scheme-program-name "guile"))

;;; CSS mode
(after-load 'css-mode
  ;; Indent level
  (setq css-indent-offset 2))

;;; NXML mode
(after-load 'nxml-mode
  ;; Configuration
  (setq nxml-slash-auto-complete-flag t))

;;; Doc View mode
(after-load 'doc-view
  ;; Better document browsing
  (setq doc-view-resolution 300
        doc-view-continuous t))

;;; Woman
(after-load 'woman
  ;; Use global fill column
  (setq woman-fill-column fill-column))

;;; Interact with GnuPG directly
(after-load 'epa
  ;; Configuration
  (setq epg-pinentry-mode 'loopback))

;;; Enable Pass integration
(after-load 'auth-source
  ;; Initialize mode
  (auth-source-pass-enable))

;;; Mail sending
(after-load 'message
  ;; Set main directory
  (setq message-directory "~/.mail/")
  ;; Configuration
  (setq message-confirm-send t
        message-kill-buffer-on-exit t))

;;; Outgoing mail server
(after-load 'smtpmail
  ;; Configuration
  (setq smtpmail-smtp-server "mail.cock.li"
        smtpmail-smtp-user "drot"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl))

;;; Smileys
(after-load 'smiley
  ;; Resize smileys
  (setq smiley-style 'medium))

;;; Network Security Manager
(after-load 'nsm
  ;; Change default settings file location
  (setq nsm-settings-file
        (locate-user-emacs-file "cache/network-security.data")))

;;; Prevent GnuTLS warnings
(after-load 'gnutls
  ;; Don't use default values
  (setq gnutls-min-prime-bits nil))

;;; Dired configuration
(after-load 'dired
  ;; Load Dired Extra library for additional features
  (require 'dired-x)
  ;; Default `ls' switches
  (setq dired-listing-switches "-alhF")
  ;; If we are on a GNU system add some more `ls' switches
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches
          (concat dired-listing-switches "G --group-directories-first")))
  ;; Use conservative switches when dealing with remote systems
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (file-remote-p dired-directory)
                (setq-local dired-actual-switches "-alhF"))))
  ;; Do certain operations recursively
  (setq dired-recursive-deletes 'top
        dired-recursive-copies 'always)
  ;; Imitate orthodox file managers with two buffers open
  (setq dired-dwim-target t))

;; Dired Extra
(autoload #'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload #'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
;; Set global key bindings
(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-x 4 C-j") #'dired-jump-other-window)
;; Dired Extra Omit configuration
(after-load 'dired-x
  ;; Omit dotfiles as well
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$")))

;; Wdired movement and editable parts
(after-load 'wdired
  ;; Allow changing of permissions too
  (setq wdired-allow-to-change-permissions t)
  ;; Make movement work the same as in regular Dired buffers
  (setq wdired-use-dired-vertical-movement 'sometimes))

;; Dired auxiliary features
(after-load 'dired-aux
  ;; Ask to create directories if they don't exist
  (setq dired-create-destination-dirs 'ask))

;; Image-Dired
(after-load 'image-dired
  ;; Change default external viewer
  (when (executable-find "feh")
    (setq image-dired-external-viewer "feh")))

;; Gnus Dired
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;;; TRAMP
(after-load 'tramp
  ;; Configuration
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (locate-user-emacs-file "cache/tramp"))
  ;; Use temp directory for save files
  (setq tramp-backup-directory-alist `(("." . ,temporary-file-directory))
        tramp-auto-save-directory temporary-file-directory))

;;; Bookmarks
(after-load 'bookmark
  ;; Configuration
  (setq bookmark-default-file (locate-user-emacs-file "cache/bookmark")
        bookmark-save-flag 1))

;;; Copyright insertion
(global-set-key (kbd "C-c i c") #'copyright)
(global-set-key (kbd "C-c i C-c") #'copyright-update)
;; Configuration
(after-load 'copyright
  ;; Change default format
  (setq copyright-year-ranges t
        copyright-names-regexp (regexp-quote user-login-name)))

;;; Whitespace mode
(global-set-key (kbd "C-c x w") #'whitespace-cleanup)
(global-set-key (kbd "C-c t w") #'whitespace-mode)

;;; Tildify mode
(global-set-key (kbd "C-c x t") #'tildify-region)
(global-set-key (kbd "C-c t ~") #'tildify-mode)
;; Initialize in LaTeX buffers
(add-hook 'LaTeX-mode-hook
          (lambda () (setq-local tildify-space-string "~")))

;;; Regexp builder
(global-set-key (kbd "C-c s b") #'re-builder)
;; Configuration
(after-load 're-builder
  ;; Default regex syntax
  (setq reb-re-syntax 'string))

;;; Proced
(global-set-key (kbd "C-x p") #'proced)
;; Configuration
(after-load 'proced
  ;; Sort by start time
  (setq-default proced-sort 'start)
  ;; Use tree view
  (setq-default proced-tree-flag t))

;;; GDB
(after-load 'gdb-mi
  ;; Multiple window layout
  (setq gdb-many-windows t))

;;; EWW
(global-set-key (kbd "C-c u w") #'eww)
(global-set-key (kbd "C-c u C-b") #'eww-list-bookmarks)
;; Configuration
(after-load 'eww
  ;; Set bookmarks directory
  (setq eww-bookmarks-directory (locate-user-emacs-file "cache/")))

;;; Browse URL
(global-set-key (kbd "C-c u b") #'browse-url)
;; Configuration
(after-load 'browse-url
  ;;  Open URLs with the specified browser
  (setq browse-url-browser-function #'browse-url-firefox))

;;; SHR
(after-load 'shr
  ;; Use specified browser instead of searching for it
  (setq shr-external-browser browse-url-browser-function))

;;; URL history
(after-load 'url-history
  ;; Save visited URLs
  (setq url-history-track t
        url-history-file (locate-user-emacs-file "url/history")))

;;; Speedbar
(global-set-key (kbd "C-c p s") #'speedbar)
;; Configuration
(after-load 'speedbar
  ;; Set local key binding
  (define-key speedbar-mode-map (kbd "a") #'speedbar-toggle-show-all-files)
  ;; Emulate NERDTree behavior
  (setq speedbar-use-images nil
        speedbar-show-unknown-files t
        speedbar-directory-unshown-regexp "^$")
  ;; Don't ignore the following extensions
  (speedbar-add-supported-extension
   '(".lisp" ".clj" ".lua" ".css" ".patch"
     ".conf" ".diff" ".sh" ".org" ".md" ".deb")))

;;; Eshell
(global-set-key (kbd "<f6>") #'eshell)
;; Configuration
(after-load 'eshell
  ;; Ignore duplicates and case
  (setq eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t)
  ;; Custom hook to avoid conflicts
  (defun drot/eshell-mode-setup ()
    "Integrate with Counsel and disable Company in Eshell buffers."
    (define-key eshell-mode-map
      [remap eshell-previous-matching-input-from-input] #'counsel-esh-history)
    ;; Disable Company since we use `completion-at-point'
    (company-mode 0))
  ;; Apply the custom hook
  (add-hook 'eshell-mode-hook #'drot/eshell-mode-setup))

;; Eshell smart display
(after-load 'eshell
  ;; Initialize mode
  (require 'em-smart)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  ;; Jump to end when `counsel-esh-history' is used
  (add-to-list 'eshell-smart-display-navigate-list #'counsel-esh-history))

;;; Shell mode
(after-load 'shell
  ;; Custom hook to avoid conflicts
  (defun drot/shell-mode-setup ()
    "Disable Company and enable clickable file paths."
    (compilation-shell-minor-mode)
    (company-mode 0))
  ;; Apply the custom hook
  (add-hook 'shell-mode-hook #'drot/shell-mode-setup))

;;; IELM
(global-set-key (kbd "C-c r i") #'ielm)
;; Configuration
(after-load 'ielm
  ;; Change default prompt
  (setq ielm-prompt "(>) "))

;;; Hydra
(require-package 'hydra)
;; Configuration
(after-load 'hydra
  ;; Enable syntax coloring for Hydra definitions
  (hydra-add-font-lock))

;;; lv - other echo area
(require-package 'lv)
;; Configuration
(after-load 'lv
  ;; Center commands
  (setq lv-use-padding t))

;;; Flymake
(global-set-key (kbd "C-c ! t") #'flymake-mode)
;; Configuration
(after-load 'flymake
  ;; Define Hydra
  (defhydra hydra-flymake
    (:pre (flyspell-mode 0) :post (flyspell-mode))
    ;; Go to errors
    ("n" flymake-goto-next-error "Next" :column "Errors")
    ("p" flymake-goto-prev-error "Previous")
    ;; List errors
    ("d" flymake-show-diagnostics-buffer "Show" :column "List Errors" :exit t)
    ;; Quit
    ("q" nil "Quit"))
  ;; Set local key bindings
  (dolist (bind '(("C-c ! n" . flymake-goto-next-error)
                  ("C-c ! p" . flymake-goto-prev-error)
                  ("C-c ! C-r" . flymake-reporting-backends)
                  ("C-c ! r" . flymake-running-backends)
                  ("C-c ! d" . flymake-show-diagnostics-buffer)
                  ("C-c ! l" . flymake-switch-to-log-buffer)
                  ("C-c ! h" . hydra-flymake/body)))
    (define-key flymake-mode-map (kbd (car bind)) (cdr bind))))

;;; Comint mode
(after-load 'comint
  ;; Ignore duplicate commands
  (setq comint-input-ignoredups t))

;;; Compilation
(global-set-key (kbd "<C-f5>") #'recompile)
;; Configuration
(after-load 'compile
  ;; Colorize ANSI escape sequences
  (require 'ansi-color)
  ;; Colorization function
  (defun drot/ansi-color-compilation-buffer ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  ;; Apply colorization
  (add-hook 'compilation-filter-hook #'drot/ansi-color-compilation-buffer)
  ;; Change default behavior
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error
        compilation-context-lines 3))

;;; Gnus
(global-set-key (kbd "<f8>") #'gnus)
;; Configuration
(after-load 'gnus
  ;; Set local key bindings
  (define-key gnus-summary-mode-map (kbd "C-c M-o") #'ace-link-gnus)
  (define-key gnus-article-mode-map (kbd "C-c M-o") #'ace-link-gnus)
  ;; Main Gnus directory
  (setq gnus-directory "~/.news/")
  ;; Configure mail server
  (setq gnus-select-method
        '(nnimap "mail.cock.li"
                 (nnimap-address "mail.cock.li")
                 (nnimap-user "drot")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  ;; Configure news server
  (add-to-list 'gnus-secondary-select-methods
               '(nntp "news.gmane.org"))
  ;; Article fetching options
  (setq gnus-article-browse-delete-temp t
        gnus-treat-strip-trailing-blank-lines 'last
        gnus-mime-display-multipart-related-as-mixed t)
  ;; Don't auto select first article
  (setq gnus-auto-select-first nil)
  ;; Group by topics
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  ;; Configure visible headers
  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
  ;; Display gravatars
  (setq gnus-treat-from-gravatar 'head)
  ;; Use notifications
  (add-hook 'gnus-after-getting-new-news-hook #'gnus-notifications)
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
  (setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-23,23f  %B (%c) %s%)\n"
        gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
        gnus-group-line-format "%M%S%p%P%5y:%B %G\n"
        gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
  ;; Display of message threading
  (setq gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "■ "
        gnus-sum-thread-tree-false-root "□ "
        gnus-sum-thread-tree-single-indent "▣ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-vertical "│"
        gnus-sum-thread-tree-single-leaf "└─▶ "))

;;; Mail folder access for Gnus
(after-load 'nnfolder
  ;; Set main directory
  (setq nnfolder-directory "~/.mail/archive"))

;;; Calendar
(global-set-key (kbd "<f12>") #'calendar)
;; Configuration
(after-load 'calendar
  ;; Calendar defaults
  (setq calendar-week-start-day 1
        calendar-date-style 'european
        calendar-latitude 43.33
        calendar-longitude 17.81)
  ;; Holiday defaults
  (setq calendar-mark-holidays-flag t
        calendar-christian-all-holidays-flag t)
  ;; Disable other holidays
  (setq holiday-general-holidays nil
        holiday-solar-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        holiday-islamic-holidays nil
        holiday-hebrew-holidays nil))

;;; Outline mode
(add-hook 'prog-mode-hook #'outline-minor-mode)
;; Set key binding
(global-set-key (kbd "C-c t o") #'outline-minor-mode)
;; Set default prefix
(setq outline-minor-mode-prefix (kbd "C-c C-o"))
;; Configuration
(after-load 'outline
  ;; Define Hydra
  (defhydra hydra-outline ()
    ;; Hide
    ("q" hide-sublevels "Sublevels" :column "Hide") ; Hide everything but the top-level headings
    ("t" hide-body "Body") ; Hide everything but headings (all body lines)
    ("o" hide-other "Other") ; Hide other branches
    ("c" hide-entry "Entry") ; Hide this entry's body
    ("l" hide-leaves "Leaves") ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree "Subtree") ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all "All" :column "Show") ; Show (expand) everything
    ("e" show-entry "Entry") ; Show this heading's body
    ("i" show-children "Children") ; Show this heading's immediate child sub-headings
    ("k" show-branches "Branches") ; Show all sub-headings under this heading
    ("s" show-subtree "Subtree") ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading "Up" :column "Move") ; Up
    ("n" outline-next-visible-heading "Next Visible") ; Next
    ("p" outline-previous-visible-heading "Previous Visible") ; Previous
    ("f" outline-forward-same-level "Forward Same Level") ; Forward - same level
    ("b" outline-backward-same-level "Backward Same Level") ; Backward - same level
    ;; Quit
    ("z" nil "Quit"))
  ;; Set local key binding
  (define-key outline-minor-mode-map (kbd "C-c o h") #'hydra-outline/body))

;;; Org-mode
(dolist (bind '(("C-c o a" . org-agenda)
                ("C-c o c" . org-capture)
                ("C-c o t" . org-todo-list)
                ("C-c o s" . org-search-view)
                ("C-c o l" . org-store-link)
                ("C-c t t" . orgtbl-mode)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Configuration
(after-load 'org
  ;; Set local key binding
  (define-key org-mode-map (kbd "C-c M-o") #'ace-link-org)
  ;; Default directory and file location
  (setq org-directory "~/Documents/org"
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files '("~/Documents/org"))
  ;; Default modules to load
  (setq org-modules
        '(org-bibtex
          org-docview
          org-eshell
          org-eww
          org-gnus
          org-info
          org-id
          org-mhe))
  ;; Add replacements for task symbols
  (defun drot/org-prettify-task-symbols-setup ()
    "Prettify `org-mode' task list symbols."
    (dolist (symbol '(("TODO"     . ?⚑)
                      ("DOING"    . ?⚐)
                      ("CANCELED" . ?✘)
                      ("DONE"     . ?✔)))
      (cl-pushnew symbol prettify-symbols-alist :test #'equal)))
  ;; Apply the custom hook
  (add-hook 'org-mode-hook #'drot/org-prettify-task-symbols-setup)
  ;; Record time when a task is done
  (setq org-log-done 'time)
  ;; Indent headings by default
  (setq org-startup-indented t)
  ;; Smart avoidance for collapsed heading edits
  (setq org-catch-invisible-edits 'smart)
  ;; Change fontification for done headings
  (setq org-fontify-done-headline t)
  ;; Make movement behavior special and use speed keys
  (setq org-special-ctrl-a/e t
        org-M-RET-may-split-line nil
        org-use-speed-commands t)
  ;; Default `org-goto' interface
  (setq org-goto-interface 'outline-path-completion)
  ;; Default LaTeX compiler
  (setq org-latex-compiler "lualatex")
  ;; LaTeX syntax highlight
  (setq org-highlight-latex-and-related '(entities latex script))
  ;; LaTeX preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ;; Store LaTeX preview images in a single directory
  (setq org-preview-latex-image-directory (locate-user-emacs-file "ltximg/"))
  ;; Native source code behavior
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
  ;; Make Wind Move work in Org mode
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

;; Org time clocking
(after-load 'org-clock
  ;; Change default persist file location
  (setq org-clock-persist-file
        (locate-user-emacs-file "cache/org-clock-save.el")))

;;; Time display
(global-set-key (kbd "<C-f12>") #'display-time-world)
;; Configuration
(after-load 'time
  ;; Use custom mode line format
  (setq display-time-string-forms
        '(" ["
          (propertize
           (format-time-string "%H:%M" now)
           'face '(:inherit font-lock-keyword-face :weight bold)
           'help-echo (format-time-string "%A, %d-%m-%Y" now))
          ;; Load average
          load
          ;; Keep default mail notification format
          (if mail
              (concat
               " "
               (propertize
                display-time-mail-string
                'display `(when (and display-time-use-mail-icon
                                     (display-graphic-p))
                            ,@display-time-mail-icon
                            ,@(if (and display-time-mail-face
                                       (memq (plist-get (cdr display-time-mail-icon)
                                                        :type)
                                             '(pbm xbm)))
                                  (let ((bg (face-attribute display-time-mail-face
                                                            :background)))
                                    (if (stringp bg)
                                        (list :background bg)))))
                'face display-time-mail-face
                'help-echo "You have new mail; mouse-2: Read mail"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-2
                                                     read-mail-command)))
            "")
          "] "))
  ;; Time zones we are interested in
  (setq display-time-world-list
        '(("Europe/Riga" "Riga")
          ("America/Los_Angeles" "Los Angeles")
          ("Canada/Eastern" "Toronto")
          ("Asia/Saigon" "Saigon")
          ("UTC" "Universal"))))

;;; 0x0 paste support
(require-package '0x0)
;; Set global key bindings
(dolist (bind '(("C-c b y" . 0x0-upload)
                ("C-c f u" . 0x0-upload-file)
                ("C-c x y" . 0x0-upload-string)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Ace-window
(require-package 'ace-window)
;; Set global key binding
(global-set-key (kbd "M-o") #'ace-window)
;; Configuration
(after-load 'ace-window
  ;; Use keys on the home row
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; AUCTeX
(require-package 'auctex)
;; TeX configuration
(after-load 'tex
  ;; Default TeX engine
  (setq-default TeX-engine 'luatex)
  ;; Automatically save and parse style
  (setq TeX-auto-save t
        TeX-parse-self t)
  ;; Use PDF Tools as default viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; Revert PDF automatically
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; TeX external commands
(after-load 'tex-buf
  ;; Don't ask to save before processing
  (setq TeX-save-query nil))

;; LaTeX configuration
(after-load 'latex
  ;; Enable Flymake syntax checking
  (add-hook 'LaTeX-mode-hook #'flymake-mode)
  ;; Enable folding options
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  ;; Further folding options with Outline
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  ;; Add RefTeX support
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  ;; Add SyncTeX support
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode))

;;; RefTeX configuration
(after-load 'reftex
  ;; Enable AUCTeX integration
  (setq reftex-plug-into-AUCTeX t))

;;; CIDER
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
  ;; Enable persistent history and cycle through it
  (setq cider-repl-history-file (locate-user-emacs-file "cache/cider-history")
        cider-repl-wrap-history t)
  ;; Disable help banner
  (setq cider-repl-display-help-banner nil)
  ;; Display result prefix
  (setq cider-repl-result-prefix ";; => ")
  ;; Enable fuzzy completion with Company
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; Enable SubWord mode
  (add-hook 'cider-repl-mode-hook #'subword-mode))

;; CIDER ElDoc configuration
(after-load 'cider-eldoc
  ;; Display context dependent info
  (setq cider-eldoc-display-context-dependent-info t))

;;; Clojure mode
(require-package 'clojure-mode)
;; Configuration
(after-load 'clojure-mode
  ;; Enable CIDER mode
  ;;(add-hook 'clojure-mode-hook #'cider-mode)
  ;; Enable SubWord mode
  (add-hook 'clojure-mode-hook #'subword-mode))

;;; Dash
(require-package 'dash)
;; Configuration
(after-load 'dash
  ;; Enable syntax coloring for Dash functions
  (dash-enable-font-lock))

;;; Debbugs browser
(require-package 'debbugs)
;; Set global key bindings
(dolist (bind '(("C-c d g" . debbugs-gnu)
                ("C-c d s" . debbugs-gnu-search)
                ("C-c d t" . debbugs-gnu-usertags)
                ("C-c d p" . debbugs-gnu-patches)
                ("C-c d b" . debbugs-gnu-bugs)
                ("C-c d C-o" . debbugs-org)
                ("C-c d C-s" . debbugs-org-search)
                ("C-c d C-p" . debbugs-org-patches)
                ("C-c d C-b" . debbugs-org-bugs)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Dired Filter
(require-package 'dired-filter)
;; Configuration
(after-load 'dired-x
  ;; Initialize mode
  (require 'dired-filter)
  ;; Set local key binding
  (define-key dired-mode-map (kbd "\\") dired-filter-mark-map))

;;; Dired Rainbow
(require-package 'dired-rainbow)
;; Configuration
(after-load 'dired-filter
  ;; Initialize mode
  (require 'dired-rainbow)
  ;; Define faces by file type
  (dired-rainbow-define audio "DeepPink" ("mp3" "MP3" "ogg" "OGG"
                                          "flac" "FLAC" "wav" "WAV"))
  (dired-rainbow-define compressed "tomato" ("zip" "bz2" "tgz" "txz" "gz"
                                             "xz" "z" "Z" "jar" "war"
                                             "ear" "rar" "sar" "xpi"
                                             "apk" "xz" "tar"))
  (dired-rainbow-define document "bisque" ("doc" "docx" "odt" "pdb" "pdf"
                                           "ps" "rtf" "djvu" "epub" "md"
                                           "tex" "org" "txt"))
  (dired-rainbow-define encrypted "salmon" ("gpg" "pgp" "rsa"))
  (dired-rainbow-define excel "turquoise" ("xlsx"))
  (dired-rainbow-define executable (:foreground "gold" :italic t) ("exe" "msi"))
  (dired-rainbow-define html "Wheat" ("htm" "html" "xhtml"))
  (dired-rainbow-define image "goldenrod" ("jpg" "png" "jpeg" "gif"))
  (dired-rainbow-define log "gray" ("log"))
  (dired-rainbow-define config (:foreground "cadet blue" :italic t) ("conf" "ini" "yml"))
  (dired-rainbow-define packaged "khaki" ("deb" "rpm"))
  (dired-rainbow-define sourcefile "SandyBrown" ("py" "c" "cc" "h" "java"
                                                 "pl" "rb" "R" "php" "el"
                                                 "scm" "cpp" "fos" "lisp" "clj"
                                                 "lua" "lisp" "sh"))
  (dired-rainbow-define video "firebrick2" ("vob" "VOB" "mkv" "MKV" "mpe"
                                            "mpg" "MPG" "mp4" "MP4" "ts"
                                            "TS" "m2ts" "M2TS" "avi" "AVI"
                                            "mov" "MOV" "wmv" "asf" "m2v"
                                            "m4v" "mpeg" "MPEG" "tp"))
  (dired-rainbow-define xml "RosyBrown" ("xml" "xsd" "xsl" "xslt" "wsdl"))
  ;; Define faces by file permission
  (dired-rainbow-define-chmod executable-unix (:foreground "gold" :bold t) "-.*x.*")
  (dired-rainbow-define-chmod directory-unix (:foreground "DeepSkyBlue" :bold t) "d.*")
  (dired-rainbow-define-chmod symlink-unix (:foreground "violet" :underline t) "l.*"))

;;; Dired Subtree
(require-package 'dired-subtree)
;; Configuration
(after-load 'dired-rainbow
  ;; Initialize mode
  (require 'dired-subtree)
  ;; Create local key map
  (defvar dired-subtree-map nil "Dired Subtree map.")
  (setq dired-subtree-map (make-sparse-keymap))
  (define-key dired-mode-map (kbd "C-,") dired-subtree-map)
  ;; Set local key bindings
  (dolist (bind '(("C-i" . dired-subtree-insert)
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
    (define-key dired-subtree-map (kbd (car bind)) (cdr bind))))

;;; Dired Ranger
(require-package 'dired-ranger)
;; Configuration
(after-load 'dired-subtree
  ;; Initialize mode
  (require 'dired-ranger)
  ;; Create local keymap
  (defvar dired-ranger-map nil "Dired Ranger map.")
  (setq dired-ranger-map (make-sparse-keymap))
  (define-key dired-mode-map (kbd "r") dired-ranger-map)
  ;; Set local key bindings
  (dolist (bind '(("c" . dired-ranger-copy)
                  ("p" . dired-ranger-paste)
                  ("m" . dired-ranger-move)))
    (define-key dired-ranger-map (kbd (car bind)) (cdr bind)))
  ;; Bookmarking
  (define-key dired-mode-map (kbd "'") #'dired-ranger-bookmark)
  (define-key dired-mode-map (kbd "`") #'dired-ranger-bookmark-visit))

;;; Dired Narrow
(require-package 'dired-narrow)
;; Configuration
(after-load 'dired-ranger
  ;; Initialize mode
  (require 'dired-narrow)
  ;; Set local key binding
  (define-key dired-mode-map (kbd "C-.") #'dired-narrow)
  ;; Exit on single match
  (setq dired-narrow-exit-when-one-left t))

;;; Dired Collapse
(require-package 'dired-collapse)
;; Configuration
(after-load 'dired-narrow
  ;; Initialize mode
  (require 'dired-collapse)
  ;; Set local key binding
  (define-key dired-mode-map (kbd ",") #'dired-collapse-mode))

;;; Dired-du
(require-package 'dired-du)
;; Configuration
(after-load 'dired-collapse
  ;; Initialize mode
  (require 'dired-du)
  ;; Use human readable output by default
  (setq dired-du-size-format t))

;;; Dired Async
(require-package 'async)
;; Configuration
(after-load 'dired-du
  ;; Initialize mode
  (require 'dired-async)
  ;; Set local key bindings
  (dolist (bind '(("E c" . dired-async-do-copy)
                  ("E r" . dired-async-do-rename)
                  ("E s" . dired-async-do-symlink)
                  ("E h" . dired-async-do-hardlink)
                  ("E m" . dired-async-mode)))
    (define-key dired-mode-map (kbd (car bind)) (cdr bind))))

;;; Asynchronous SMTP mail sending
(after-load 'message
  ;; Load async library
  (require 'smtpmail-async)
  ;; Change default mail sending function
  (setq message-send-mail-function #'async-smtpmail-send-it)
  ;; Enable compatibility with the Pass password manager
  (add-hook 'async-smtpmail-before-send-hook #'auth-source-pass-enable))

;;; Eglot
(require-package 'eglot)
;; Set global key binding
(global-set-key (kbd "C-c e t") #'eglot)
;; Configuration
(after-load 'eglot
  ;; Set local key bindings
  (dolist (bind '(("C-c e c" . eglot-reconnect)
                  ("C-c e s" . eglot-shutdown)
                  ("C-c e r" . eglot-rename)
                  ("C-c e a" . eglot-code-actions)
                  ("C-c e h" . eglot-help-at-point)
                  ("C-c e b" . eglot-events-buffer)
                  (C-c e e . eglot-stderr-buffer)))
    (define-key eglot-mode-map (kbd (car bind)) (cdr bind)))
  ;; Add the Lua language server
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-lsp"))))

;;; Easy-kill
(require-package 'easy-kill)
;; Set global key bindings
(global-set-key [remap kill-ring-save] #'easy-kill)
(global-set-key [remap mark-sexp] #'easy-mark)

;;; Elfeed
(require-package 'elfeed)
;; Set global key binding
(global-set-key (kbd "<C-f8>") #'elfeed)
;; Configuration
(after-load 'elfeed
  ;; Default feeds
  (setq elfeed-feeds
        '(("https://news.ycombinator.com/rss" hnews)
          ("https://lwn.net/headlines/rss" lwn)
          ("https://www.reddit.com/r/emacs/.rss" emacs)
          ("https://www.reddit.com/r/linux/.rss" linux)
          ("https://www.reddit.com/r/linux/.rss" programming)
          ("http://bljesak.info/rss" bljesak)))
  ;; Change default database location and search defaults
  (setq elfeed-db-directory (locate-user-emacs-file "elfeed")
        elfeed-search-date-format '("%d-%m-%Y" 10 :left)
        elfeed-search-filter "@1-week-ago +unread")
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))

;;; rcirc
(global-set-key (kbd "<f7>") #'irc)
;; Configuration
(after-load 'rcirc
  ;; User defaults
  (setq rcirc-default-user-name "drot"
        rcirc-reconnect-delay 10)
  ;; Connect to the specified servers and channels
  (setq rcirc-server-alist
        '(("irc.rizon.net"
           :port 6697
           :encryption tls
           :channels ("#/g/technology" "#rice"))
          ("irc.forestnet.org"
           :port 6697
           :encryption tls
           :channels ("#rawhide" "#sq"))))
  ;; Authentication
  (setq rcirc-authinfo
        `(("rizon" nickserv "drot"
           ,(auth-source-pass-get 'secret "IRC/drot@irc.rizon.net"))
          ("forestnet" nickserv "drot"
           ,(auth-source-pass-get 'secret "IRC/drot@irc.forestnet.org"))))
  ;; Truncate buffer output
  (setq rcirc-buffer-maximum-lines 2048)
  ;; Set fill column value to frame width
  (setq rcirc-fill-column #'window-text-width)
  ;; Enable logging
  (setq rcirc-log-flag t)
  ;; Enable additional modes
  (add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (add-hook 'rcirc-mode-hook #'flyspell-mode)
  ;; Disable company mode in rcirc buffers
  (add-hook 'rcirc-mode-hook
            (lambda () (company-mode 0)))
  ;; Exclude text properties when yanking text in rcirc buffers
  (add-to-list 'yank-excluded-properties 'rcirc-text)
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
(require-package 'rcirc-styles)
;; Configuration
(after-load 'rcirc
  ;; Initialize mode
  (require 'rcirc-styles)
  ;; Set local key bindings
  (dolist (bind '(("C-c C-e p" . rcirc-styles-toggle-preview)
                  ("C-c C-e a" . rcirc-styles-insert-attribute)
                  ("C-c C-e c" . rcirc-styles-insert-color)))
    (define-key rcirc-mode-map (kbd (car bind)) (cdr bind)))
  ;; Use custom colors
  (setq rcirc-styles-color-vector
        ["#515151"
         "#cc6666"
         "#b5bd68"
         "#f0c674"
         "#81a2be"
         "#b294bb"
         "#8abeb7"
         "#c5c8c6"
         "#969896"
         "#cc6666"
         "#b5bd68"
         "#f0c674"
         "#81a2be"
         "#b294bb"
         "#8abeb7"
         "#ffffff"]))

;; rcirc colored nicknames
(require-package 'rcirc-color)
;; Configuration
(after-load 'rcirc
  ;; Initialize mode
  (require 'rcirc-color)
  ;; Inherit nick colors from rcirc-styles colors
  (setq rcirc-colors (append rcirc-styles-color-vector nil)))

;; rcirc notifications
(require-package 'rcirc-notify)
;; Configuration
(after-load 'rcirc
  ;; Initialize mode
  (rcirc-notify-add-hooks))

;;; Expand region
(require-package 'expand-region)
;; Set global key binding
(global-set-key (kbd "C-=") #'er/expand-region)

;;; Geiser
(require-package 'geiser)
;; Configuration
(after-load 'geiser-repl
  ;; Change default history file location
  (setq geiser-repl-history-filename
        (locate-user-emacs-file "cache/geiser-history")))

;;; Iedit
(require-package 'iedit)
;; Autoload missing functions
(autoload #'iedit-mode-from-isearch "iedit"
  "Start Iedit mode using last search string as the regexp." t)
(autoload #'iedit-execute-last-modification "iedit"
  "Apply last modification in Iedit mode to the current buffer or an active region." t)
;; Set global key binding
(global-set-key (kbd "C-c i e") #'iedit-mode)
;; Set local key bindings
(define-key isearch-mode-map (kbd "C-;") #'iedit-mode-from-isearch)
(define-key esc-map (kbd "C-;") #'iedit-execute-last-modification)
(define-key help-map (kbd "C-;") #'iedit-mode-toggle-on-function)
;; Configuration
(after-load 'iedit
  ;; Disable conflicting key binding
  (setq iedit-toggle-key-default nil))

;;; JavaScript mode
(require-package 'js2-mode)
;; Initialize mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Configuration
(after-load 'js2-mode
  ;; Syntax defaults
  (setq js2-basic-offset 2
        js2-highlight-level 3)
  ;; Highlight unused variables
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;;; JSON mode
(require-package 'json-mode)

;;; Lua mode
(require-package 'lua-mode)
;; Configuration
(after-load 'lua-mode
  ;; Lua 5.3 as the default interpreter
  (setq lua-default-application "lua5.3"))

;;; EPUB format support
(require-package 'nov)
;; Initialize package
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; Configuration
(after-load 'nov
  ;; Change default saved places file location
  (setq nov-save-place-file (locate-user-emacs-file "cache/nov-places"))
  ;; Change default font
  (defun drot/nov-font-setup ()
    "Apply custom variable pitch font for `nov.el'."
    (face-remap-add-relative 'variable-pitch :family "Noto Serif" :height 1.2))
  ;; Apply the custom hook
  (add-hook 'nov-mode-hook #'drot/nov-font-setup)
  ;; Text filling
  (setq nov-text-width 80))

;;; Macrostep
(require-package 'macrostep)
;; Set local key binding
(define-key emacs-lisp-mode-map (kbd "C-c M-e") #'macrostep-expand)

;;; Magit
(require-package 'magit)
;; Set global key bindings
(dolist (bind '(("C-x g" . magit-status)
                ("C-x M-g" . magit-dispatch-popup)
                ("C-c g c" . magit-clone)
                ("C-c g b" . magit-blame)
                ("C-c g l" . magit-log-buffer-file)
                ("C-c g p" . magit-pull)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Markdown mode
(require-package 'markdown-mode)
;; Configuration
(after-load 'markdown-mode
  ;; Default markdown command
  (setq markdown-command
        (concat "pandoc"
                " -f markdown -t html"
                " -s --mathjax --highlight-style=pygments"))
  ;; Import table creation from `org-mode'
  (require 'org-table)
  ;; Make table format compatible with Markdown
  (defun drot/markdown-org-table-align-advice ()
    "Replace \"+\" sign with \"|\" in tables."
    (when (member major-mode '(markdown-mode gfm-mode))
      (save-excursion
        (save-restriction
          (narrow-to-region (org-table-begin) (org-table-end))
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-"))))))
  ;; Add advice for table alignment
  (advice-add 'org-table-align :after #'drot/markdown-org-table-align-advice)
  ;; Enable `visual-line-mode' in Markdown buffers and disable `auto-fill-mode'
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  ;; Additional fontification
  (setq markdown-fontify-code-blocks-natively t
        markdown-header-scaling t)
  ;; Don't insert spaces after a code fence
  (setq markdown-spaces-after-code-fence 0))

;;; Move-text
(require-package 'move-text)
;; Define Hydra
(defhydra hydra-move-text ()
  ("p" move-text-up "Move Up" :column "Move Text")
  ("n" move-text-down "Move Down")
  ("q" nil "Quit"))
;; Set global key binding
(global-set-key (kbd "C-c x m") #'hydra-move-text/body)

;;; Multiple cursors
(require-package 'multiple-cursors)
;; Change default `multiple-cursors' lists file location
(setq mc/list-file (locate-user-emacs-file "cache/mc-lists.el"))
;; Populate default `multiple-cursors' lists
(unless (file-exists-p mc/list-file)
  ;; Commands to run always
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
          paredit-wrap-round
          paredit-wrap-square
          paredit-reindent-defun
          paredit-semicolon
          paredit-splice-sexp
          paredit-splice-sexp-killing-backward
          paredit-backslash
          reindent-then-newline-and-indent
          scroll-other-window
          switch-to-buffer
          upcase-region
          yank-rectangle))
  ;; Commands to run only once
  (setq mc/cmds-to-run-once
        '(down-list
          hydra-multiple-cursors/mc/edit-beginnings-of-lines
          hydra-multiple-cursors/mc/edit-ends-of-lines-and-exit
          hydra-multiple-cursors/mc/edit-lines-and-exit
          mouse-drag-mode-line
          swiper-mc)))
;; Set global key bindings
(dolist (bind '(("C-c m <SPC>" . mc/vertical-align-with-space)
                ("C-c m a" . mc/vertical-align)
                ("C-c m e" . mc/mark-more-like-this-extended)
                ("C-c m m" . mc/mark-all-like-this-dwim)
                ("C-c m l" . mc/edit-lines)
                ("C-c m n" . mc/mark-next-like-this)
                ("C-c m p" . mc/mark-previous-like-this)
                ("C-c m C-a" . mc/edit-beginnings-of-lines)
                ("C-c m C-e" . mc/edit-ends-of-lines)
                ("C-c m C-s" . mc/mark-all-in-region)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Define Hydra
(defhydra hydra-multiple-cursors ()
  ;; Lines
  ("l" mc/edit-lines "Edit Lines" :column "Lines" :exit t)
  ("b" mc/edit-beginnings-of-lines "Beginnings of Lines" :exit t)
  ("e" mc/edit-ends-of-lines "Ends of Lines" :exit t)
  ;; Mark Like This
  ("a" mc/mark-all-dwim "All DWIM" :column "Mark Like This" :exit t)
  ("s" mc/mark-all-symbols-like-this "All Symbols" :exit t)
  ("w" mc/mark-all-words-like-this "All Words" :exit t)
  ("r" mc/mark-all-in-region "All Region" :exit t)
  ;; Up
  ("n" mc/mark-next-like-this "Next" :column "Up")
  ("N" mc/skip-to-next-like-this "Skip")
  ("M-n" mc/unmark-next-like-this "Unmark")
  ;; Down
  ("p" mc/mark-previous-like-this "Previous" :column "Down")
  ("P" mc/skip-to-previous-like-this "Skip")
  ("M-p" mc/unmark-previous-like-this "Unmark")
  ;; Other
  ("i" mc/insert-numbers "Insert Numbers" :column "Other" :exit t)
  ("R" mc/mark-all-in-region-regexp "Mark All Region Regexp" :exit t)
  ("f" mc/mark-all-like-this-in-defun "Mark All Region Defun" :exit t)
  ("S" mc/mark-all-symbols-like-this-in-defun "Mark All Symbols Defun" :exit t)
  ("W" mc/mark-all-words-like-this-in-defun "Mark All Words Defun" :exit t)
  ("q" nil "Quit"))
;; Set global key binding
(global-set-key (kbd "C-c m h") #'hydra-multiple-cursors/body)

;;; PDF Tools
(require-package 'pdf-tools)
;; Initialize mode
(pdf-loader-install t)
;; Enable SyncTeX support
(add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode)
;; Enable link following
(add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)

;; PDF Tools annotations
(after-load 'pdf-annot
  ;; Activate annotations automatically
  (setq pdf-annot-activate-created-annotations t))

;; PDF Tools printing
(after-load 'pdf-misc
  ;; Use lp when possible
  (when (executable-find "lp")
    (setq pdf-misc-print-programm "/usr/bin/lp")))

;;; Polymode Markdown
(require-package 'poly-markdown)
;; Initialize mode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; Rainbow mode
(require-package 'rainbow-mode)
;; Set global key binding
(global-set-key (kbd "C-c t r") #'rainbow-mode)

;;; Skewer
(require-package 'skewer-mode)
;; Initialize mode
(add-hook 'js2-mode-hook #'skewer-mode)
;; Set global key binding
(global-set-key (kbd "C-c r w") #'skewer-repl)

;; Skewer CSS
(add-hook 'css-mode-hook #'skewer-css-mode)

;; Skewer HTML
(add-hook 'mhtml-mode-hook #'skewer-html-mode)

;;; SLY
(require-package 'sly)
;; Set global key bindings
(global-set-key (kbd "C-c r s") #'sly)
(global-set-key (kbd "C-c r c") #'sly-connect)
;; Configuration
(after-load 'sly
  ;; Use SBCL by default
  (setq inferior-lisp-program "sbcl"))

;; SLY REPL
(after-load 'sly-mrepl
  ;; Change history file location
  (setq sly-mrepl-history-file-name
        (locate-user-emacs-file "cache/sly-mrepl-history")))

;;; SLY macrostep
(require-package 'sly-macrostep)

;;; SQL indentation
(require-package 'sql-indent)

;;; Systemd mode
(require-package 'systemd)

;;; VTerm
(require-package 'vterm)
;; Set global key binding
(global-set-key (kbd "<C-f6>") #'vterm)

;;; Wgrep
(require-package 'wgrep)

;;; YAML mode
(require-package 'yaml-mode)
;; Enable SubWord mode
(add-hook 'yaml-mode-hook #'subword-mode)

;;; Ace-link
(require-package 'ace-link)
;; Initialize mode
(add-hook 'after-init-hook #'ace-link-setup-default)
;; Set global key binding
(global-set-key (kbd "C-c u a") #'ace-link-addr)

;;; Avy
(require-package 'avy)
;; Initialize mode
(add-hook 'after-init-hook #'avy-setup-default)
;; Define Hydra
(defhydra hydra-avy-cycle ()
  ("n" avy-next "Next" :column "Cycle avy Candidates")
  ("p" avy-prev "Previous")
  ("q" nil "Quit"))
;; Set global key bindings
(dolist (bind '(("C-:" . avy-goto-char)
                ("C-'" . avy-goto-char-timer)
                ("M-g f" . avy-goto-line)
                ("M-g w" . avy-goto-word-1)
                ("M-g e" . avy-goto-word-0)
                ("C-M-'" . hydra-avy-cycle/body)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Configuration
(after-load 'avy
  ;; Work across all frames
  (setq avy-all-windows 'all-frames)
  ;; Follow indentation with the overlay
  (setq avy-indent-line-overlay t)
  ;; Dim background during selection
  (setq avy-background t
        avy-highlight-first t))

;;; Company mode
(require-package 'company)
;; Initialize mode
(add-hook 'after-init-hook #'global-company-mode)
;; Set global key binding
(global-set-key (kbd "C-c i y") #'company-yasnippet)
;; Configuration
(after-load 'company
  ;; Change default backends
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-etags company-keywords)
          company-dabbrev))
  ;; More eager completion and cycle candidates
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t)
  ;; Allow non-matching input
  (setq company-require-match nil)
  ;; Show numbers on candidates
  (setq company-show-numbers t)
  ;; Tooltip behavior
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  ;; Dabbrev completion behavior
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

;;; Diff-Hl
(require-package 'diff-hl)
;; Initialize mode
(add-hook 'after-init-hook #'global-diff-hl-mode)
;; Update diffs immediately
(add-hook 'after-init-hook #'diff-hl-flydiff-mode)
;; Set global key binding
(global-set-key (kbd "C-c t v") #'diff-hl-margin-mode)
;; Add hooks for `dired' and `magit'
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

;;; Form-feed
(require-package 'form-feed)
;; Same line width as `fill-column' width
(setq form-feed-line-width fill-column)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                scheme-mode-hook
                compilation-mode-hook
                outline-mode-hook
                help-mode-hook))
  (add-hook hook #'form-feed-mode))
;; Configuration
(after-load 'form-feed
  ;; Make `form-feed-line-line' color equal to comment color
  (set-face-attribute 'form-feed-line nil
                      :strike-through t
                      :inherit font-lock-comment-face))

;;; Hl-Todo
(require-package 'hl-todo)
;; Initialize mode
(add-hook 'after-init-hook #'global-hl-todo-mode)
;; Configuration
(after-load 'hl-todo
  ;; Define Hydra
  (defhydra hydra-hl-todo ()
    ("n" hl-todo-next "Next TODO" :column "Highlight TODO")
    ("p" hl-todo-previous "Previous TODO")
    ("q" nil "Quit"))
  ;; Set local key bindings
  (dolist (bind '(("M-s t" . hl-todo-occur)
                  ("C-c p t" . hydra-hl-todo/body)
                  ("C-c p i" . hl-todo-insert-keyword)))
    (define-key hl-todo-mode-map (kbd (car bind)) (cdr bind))))

;;; Amx
(require-package 'amx)
;; Initialize mode
(add-hook 'after-init-hook #'amx-mode)
;; Set global key bindings
(global-set-key (kbd "M-X") #'amx-major-mode-commands)
(global-set-key (kbd "C-c h u") #'amx-show-unbound-commands)
;; Configuration
(after-load 'amx
  ;; Change save file location
  (setq amx-save-file (locate-user-emacs-file "cache/amx-items")))

;;; Ivy
(require-package 'ivy)
;; Ivy Hydra support
(require-package 'ivy-hydra)
;; Initialize mode
(add-hook 'after-init-hook #'ivy-mode)
;; Set global key binding
(global-set-key (kbd "<C-f4>") #'ivy-resume)
;; Configuration
(after-load 'ivy
  ;; Optimize completion
  (setq ivy-dynamic-exhibit-delay-ms 150)
  ;; Prompt format
  (setq ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) ")
  ;; Use arrow display
  (setq ivy-format-functions-alist '((t . ivy-format-function-arrow)))
  ;; Virtual buffer usage
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbreviate)
  ;; Wrap by default
  (setq ivy-wrap t
        ivy-action-wrap t))

;;; Counsel
(require-package 'counsel)
;; Initialize mode
(add-hook 'after-init-hook #'counsel-mode)
;; Set global key bindings
(dolist (bind '(("C-c s C-s" . counsel-rg)
                ("C-c f g" . counsel-git)
                ("C-c f d" . counsel-dired-jump)
                ("C-c f r" . counsel-buffer-or-recentf)
                ("C-c s v" . counsel-git-grep)
                ("C-c s g" . counsel-grep)
                ("C-c s i" . counsel-imenu)
                ("C-c h c" . counsel-command-history)
                ("C-c h l" . counsel-find-library)
                ("C-c i u" . counsel-unicode-char)
                ("C-c f j" . counsel-file-jump)
                ("C-c o j" . counsel-outline)
                ("C-c v s" . counsel-set-variable)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Remap the rest
(global-set-key [remap org-goto] #'counsel-org-goto)
(global-set-key [remap org-set-tags-command] #'counsel-org-tag)
(global-set-key [remap menu-bar-open] #'counsel-tmm)
;; Configuration
(after-load 'counsel
  ;; Preselect files
  (setq counsel-preselect-current-file t)
  ;; Change `counsel-org' defaults
  (setq counsel-org-goto-face-style 'verbatim
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))

;;; Swiper
(require-package 'swiper)
;; Set global key bindings
(global-set-key (kbd "C-c s s") #'swiper-all)
(global-set-key (kbd "M-s s") #'swiper)
;; Set local key binding
(define-key isearch-mode-map (kbd "M-s s") #'swiper-from-isearch)
;; Configuration
(after-load 'swiper
  ;; Include line numbers
  (setq swiper-include-line-number-in-search t)
  ;; Always go to the beginning of a match
  (setq swiper-goto-start-of-match t))

;;; Prescient
(require-package 'prescient)
;; Configuration
(after-load 'prescient
  ;; Change save file location
  (setq prescient-save-file (locate-user-emacs-file "cache/prescient-save.el"))
  ;; Aggressively save history
  (setq prescient-aggressive-file-save t)
  ;; Use fuzzy matching by default
  (setq prescient-filter-method 'fuzzy)
  ;; Enable persistent history
  (prescient-persist-mode))

;;; Ivy Prescient
(require-package 'ivy-prescient)
;; Configuration
(after-load 'counsel
  ;; Initialize mode
  (ivy-prescient-mode))

;;; Company Prescient
(require-package 'company-prescient)
;; Initialize mode
(add-hook 'after-init-hook #'company-prescient-mode)

;;; Minions
(require-package 'minions)
;; Initialize mode
(minions-mode)
;; Configuration
(after-load 'minions
  ;; Change mode lighter and color
  (setq minions-mode-line-lighter "≡"
        minions-mode-line-face 'shadow)
  ;; Don't hide the following minor modes
  (setq minions-direct
        '(ace-window-mode
          artist-mode
          auto-fill-function
          auto-revert-mode
          cider-mode
          flymake-mode
          geiser-autodoc-mode
          geiser-mode
          isearch-mode
          overwrite-mode
          poly-markdown-mode
          sqlind-minor-mode
          subword-mode
          visual-line-mode)))

;;; Paredit
(require-package 'paredit)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                clojure-mode-hook
                cider-repl-mode-hook
                scheme-mode-hook
                sly-mrepl-mode-hook
                geiser-repl-mode-hook))
  (add-hook hook #'enable-paredit-mode))
;; Configuration
(after-load 'paredit
  ;; Define extra commands
  (defun paredit-mark-containing-sexp ()
    (interactive)
    (paredit-backward-up)
    (mark-sexp))

  (defun paredit-barf-all-the-way-backward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-backward-down)
    (paredit-splice-sexp))

  (defun paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))

  (defun paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))

  (defun paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))

  (defun paredit--is-at-start-of-sexp ()
    (and (looking-at "(\\|\\[")
         (not (nth 3 (syntax-ppss))) ;; Inside string
         (not (nth 4 (syntax-ppss))))) ;; Inside comment

  (defun paredit-duplicate-closest-sexp ()
    (interactive)
    ;; Skips to start of current sexp
    (while (not (paredit--is-at-start-of-sexp))
      (paredit-backward))
    (set-mark-command nil)
    ;; While we find sexps we move forward on the line
    (while (and (bounds-of-thing-at-point 'sexp)
                (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                (not (= (point) (line-end-position))))
      (forward-sexp)
      (while (looking-at " ")
        (forward-char)))
    (kill-ring-save (mark) (point))
    ;; Go to the next line and copy the sexprs we encountered
    (paredit-newline)
    (yank)
    (exchange-point-and-mark))

  ;; Add command descriptions
  (nconc paredit-commands
         '("Extreme Barfage & Slurpage"
           (("C-M-)")
            paredit-slurp-all-the-way-forward
            ("(foo (bar |baz) quux zot)"
             "(foo (bar |baz quux zot))")
            ("(a b ((c| d)) e f)"
             "(a b ((c| d)) e f)"))
           (("C-M-}")
            paredit-barf-all-the-way-forward
            ("(foo (bar |baz quux) zot)"
             "(foo (bar|) baz quux zot)"))
           (("C-M-(")
            paredit-slurp-all-the-way-backward
            ("(foo bar (baz| quux) zot)"
             "((foo bar baz| quux) zot)")
            ("(a b ((c| d)) e f)"
             "(a b ((c| d)) e f)"))
           (("C-M-{")
            paredit-barf-all-the-way-backward
            ("(foo (bar baz |quux) zot)"
             "(foo bar baz (|quux) zot)"))
           (("C-M->")
            paredit-duplicate-closest-sexp
            ("(foo | bar)"
             "(foo bar)(foo bar)"))))

  ;; Initialize extra key bindings
  (paredit-define-keys)
  (paredit-annotate-mode-with-examples)
  (paredit-annotate-functions-with-examples)

  ;; Disable conflicting key binding
  (define-key paredit-mode-map (kbd "M-s") nil)
  ;; Set local key bindings
  (dolist (bind '(("M-s M-s" . paredit-splice-sexp)
                  ("M-{" . paredit-wrap-curly)
                  ("M-[" . paredit-wrap-square)
                  ("C-c C-M-s" . paredit-mark-containing-sexp)))
    (define-key paredit-mode-map (kbd (car bind)) (cdr bind)))

  ;; Enable Paredit in the minibuffer
  (defvar drot/paredit-minibuffer-setup-commands
    '(eval-expression
      pp-eval-expression
      eval-expression-with-eldoc
      ibuffer-do-eval
      ibuffer-do-view-and-eval)
    "Interactive commands for which Paredit should be enabled in the minibuffer.")

  (defun drot/paredit-minibuffer-setup ()
    "Enable Paredit during lisp-related minibuffer commands."
    (if (memq this-command drot/paredit-minibuffer-setup-commands)
        (enable-paredit-mode)))

  (add-hook 'minibuffer-setup-hook #'drot/paredit-minibuffer-setup)
  ;; Disable Electric Pair mode when Paredit is active
  (add-hook 'paredit-mode-hook
            (lambda () (setq-local electric-pair-mode nil))))

;;; Rainbow Delimiters
(require-package 'rainbow-delimiters)
;; Initialize mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                scheme-mode-hook))
  (add-hook hook #'rainbow-delimiters-mode))

;;; Undo Tree
(require-package 'undo-tree)
;; Initialize mode
(add-hook 'after-init-hook #'global-undo-tree-mode)
;; Configuration
(after-load 'undo-tree
  ;; Enable undo history saving
  (setq undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo/"))))
  (setq undo-tree-auto-save-history t)
  ;; More detailed visualizers
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t))

;;; Volatile Highlights
(require-package 'volatile-highlights)
;; Initialize mode
(add-hook 'after-init-hook #'volatile-highlights-mode)

;;; YASnippet
(require-package 'yasnippet)
;; Initialize mode
(add-hook 'after-init-hook #'yas-global-mode)

;;; Artist mode
(global-set-key (kbd "C-c t a") #'artist-mode)

;;; Toggle debug on error
(global-set-key (kbd "C-c t d") #'toggle-debug-on-error)

;;; Ediff
(global-set-key (kbd "C-c f e") #'ediff)
(global-set-key (kbd "C-c f 3") #'ediff3)

;;; Hexl mode
(global-set-key (kbd "C-c t h") #'hexl-mode)
(global-set-key (kbd "C-c f h") #'hexl-find-file)

;;; Replace strings
(global-set-key (kbd "C-c s r") #'replace-string)
(global-set-key (kbd "C-c s C-r") #'replace-regexp)

;;; Grep results as a dired buffer
(global-set-key (kbd "C-c s d") #'find-grep-dired)

;;; Project
(dolist (bind '(("C-c p f" . project-find-file)
                ("C-c p r" . project-find-regexp)
                ("C-c p s" . project-search)
                ("C-c p q" . project-query-replace)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Find function and variable definitions
(dolist (bind '(("C-c h f" . find-function)
                ("C-c h 4 f" . find-function-other-window)
                ("C-c h k" . find-function-on-key)
                ("C-c h v" . find-variable)
                ("C-c h 4 v" . find-variable-other-window)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Find library
(global-set-key (kbd "C-c h 4 l") #'find-library-other-window)
(global-set-key (kbd "C-c h 4 L") #'find-library-other-frame)

;;; List packages
(global-set-key (kbd "<f9>") #'package-list-packages)

;;; Cycle spacing
(global-set-key [remap just-one-space] #'cycle-spacing)

;;; Sort lines alphabetically
(global-set-key (kbd "C-c x l") #'sort-lines)

;;; Sort fields with regular expressions
(global-set-key (kbd "C-c x f") #'sort-regexp-fields)

;;; Word capitalization operations
(global-set-key [remap capitalize-word] #'capitalize-dwim)
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)

;;; Auto Fill mode
(global-set-key (kbd "C-c t f") #'auto-fill-mode)

;;; Align
(dolist (bind '(("C-c x a" . align)
                ("C-c x c" . align-current)
                ("C-c x r" . align-regexp)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Auto Insert
(global-set-key (kbd "C-c i a") #'auto-insert)

;;; Table insertion
(global-set-key (kbd "C-c i t") #'table-insert)

;;; Hydra for various text marking operations
(defhydra hydra-mark-text (:exit t)
  ;; Lisp
  ("e" mark-sexp "S-Expression" :column "Lisp")
  ("f" er/mark-defun "Function")
  ("s" er/mark-symbol "Symbol")
  ("S" er/mark-symbol-with-prefix "Prefixed Symbol")
  ;; Text
  ("w" er/mark-word "Word" :column "Text")
  ("p" er/mark-text-paragraph "Paragraph")
  ("c" er/mark-comment "Comment")
  ("u" er/mark-url "URL")
  ("E" er/mark-email "Email")
  ;; Quotes
  ("q" er/mark-inside-quotes "Inside Quotes" :column "Quotes")
  ("Q" er/mark-outside-quotes "Outside Quotes")
  ;; Pairs
  ("(" er/mark-inside-pairs "Inside Pairs" :column "Pairs")
  ("[" er/mark-inside-pairs "Inside Pairs")
  ("{" er/mark-inside-pairs "Inside Pairs")
  (")" er/mark-outside-pairs "Inside Pairs")
  ("]" er/mark-outside-pairs "Inside Pairs")
  ("}" er/mark-outside-pairs "Inside Pairs")
  ;; Region
  ("." er/expand-region "Expand Region" :column "Region" :exit nil)
  ("," er/contract-region "Contract Region" :exit nil)
  ;; Quit
  ("q" nil "Quit"))
;; Set global key binding
(global-set-key (kbd "C-c x C-SPC") #'hydra-mark-text/body)

;;; Matching lines operation
(global-set-key (kbd "C-c s l") #'delete-matching-lines)
(global-set-key (kbd "C-c s C-l") #'delete-non-matching-lines)

;;; Local variable insertion
(dolist (bind '(("C-c v d" . add-dir-local-variable)
                ("C-c v f" . add-file-local-variable)
                ("C-c v p" . add-file-local-variable-prop-line)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Extended buffer operation key bindings
(dolist (bind '(("C-c b DEL" . erase-buffer)
                ("C-z" . bury-buffer)
                ("C-S-z" . unbury-buffer)
                ("C-c b e" . eval-buffer)
                ("C-c b k" . kill-this-buffer)
                ("C-c b i" . insert-buffer)
                ("<f5>" . revert-buffer)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Replace dabbrev-expand with hippie-expand
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;;; Load changes from the customize interface
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
