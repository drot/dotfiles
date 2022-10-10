;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022 drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/work-arch/emacs/.config/emacs
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

;;; Keep main Emacs directory clean from various cache and save files

;;; Color theme
(setup ef-themes (:elpaca t)
  ;; Load theme
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-deuteranopia-dark))

;;; Don't show the startup welcome messages
(setq inhibit-startup-screen t)
(fset 'display-startup-echo-area-message #'ignore)

;;; Hide async compilation warnings buffer
(setq native-comp-async-report-warnings-errors 'silent)

;;; Disable scratch buffer info text
(setq initial-scratch-message nil)

;;; Show column number and buffer size on the mode line
(column-number-mode +1)
(size-indication-mode +1)

;;; Compress mode line on buffer splits
(setq mode-line-compact 'long)

;;; Answer y or n instead of yes or no at prompts
(setq use-short-answers t)

;;; Use a shorter alias for this commonly used macro
(defalias 'after-load #'with-eval-after-load)

;;; Show unfinished keystrokes early
(setq echo-keystrokes 0.01)

;;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;;; Show window dividers
(setq window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      window-divider-default-places t)
;; Enable mode
(window-divider-mode +1)

;;; Enable pixel scrolling
(pixel-scroll-precision-mode +1)

;;; Move point all the way to buffer boundary before signaling an error
(setq scroll-error-top-bottom t)

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

;;; Don't show help for completions
(setq completion-show-help nil)

;;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;;; Indicate minibuffer recursion depth
(minibuffer-depth-indicate-mode +1)

;;; Maximum completions buffer window height
(setq completions-max-height 12)

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

;;; Visual Line mode configuration
(setq visual-line-fringe-indicators '(nil right-triangle))

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
(setq version-control t
      kept-new-versions 6
      delete-old-versions t
      backup-by-copying t)

;;; Auto save file configuration
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Save minibuffer history
(setq savehist-autosave-interval nil
      savehist-additional-variables '(search-ring regexp-search-ring))
;; Initialize mode
(savehist-mode +1)

;;; Save recent files list
(setq recentf-max-saved-items nil
      recentf-max-menu-items 20
      recentf-auto-cleanup 600)
;; Exclude certain files
(setq recentf-exclude
      '("/\\.git/.*\\'"
        "/dev/shm/.*\\'"
        ".*\\.gz\\'"
        "/lib/.*\\'"
        "newsrc"
        "TAGS"))
;; Enable mode
(recentf-mode +1)
;; Exclude `no-littering' directories from recent files list
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

;;; Remember point position in files
(save-place-mode +1)

;;; Line numbers display
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute nil)
;; Maximum width reserved for line numbers
(setq display-line-numbers-width-start t)
;; Display line numbers only in relevant modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;;; Display fill column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)

;;; Highlight current line
(global-hl-line-mode +1)
;; Disable `hl-line-mode' in special buffers
(dolist (hook '(artist-mode-hook
                comint-mode-hook
                ediff-mode-hook
                elpher-mode-hook
                Custom-mode-hook
                Info-mode-hook
                eshell-mode-hook
                help-mode-hook
                nov-mode-hook
                rcirc-mode-hook
                term-mode-hook
                vterm-mode-hook
                cider-repl-mode-hook))
  (add-hook hook
            (lambda () (hl-line-mode -1))))
;; Configuration
(after-load 'hl-line
  ;; Don't display line highlight in inactive windows
  (setq global-hl-line-sticky-flag nil))

;;; Highlight matching parentheses configuration
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay)

;;; Highlight regexps interactively
(setq hi-lock-auto-select-face t)
;; Enable mode
(global-hi-lock-mode +1)

;;; Abbrev mode
(setq save-abbrevs 'silently)
;; Load abbrevs if they exist
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
;; Enable mode
(setq-default abbrev-mode t)

;;; Electric pair mode
(electric-pair-mode +1)
;; Configuration
(after-load 'electric
  ;; Watch out for context
  (setq electric-quote-context-sensitive t))

;;; Prettify-Symbols mode
(setq prettify-symbols-unprettify-at-point t)
;; Enable mode
(global-prettify-symbols-mode +1)

;;; Which function mode
(which-function-mode +1)

;;; Wind Move fast window switching
(windmove-default-keybindings)
;; Set key bindings to pick next window display position
(windmove-display-default-keybindings)
;; Set key bindings to delete windows
(windmove-delete-default-keybindings)
;; Configuration
(after-load 'windmove
  ;; Cycle windows and create them if needed
  (setq windmove-wrap-around t
        windmove-create-window t))

;;; Undo and redo the window configuration
(setq winner-dont-bind-my-keys t)
;; Set key bindings
(keymap-global-set "C-s-<left>" 'winner-undo)
(keymap-global-set "C-s-<right>" 'winner-redo)
;; Initialize mode
(winner-mode +1)

;;; Hide Show mode
(dolist (hook '(c-mode-common-hook
                emacs-lisp-mode-hook
                python-mode-hook))
  (add-hook hook #'hs-minor-mode))
;; Configuration
(after-load 'hideshow
  ;; Custom overlay function
  (defun site/hs-display-code-line-counts (ov)
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
  (setq hs-set-up-overlay #'site/hs-display-code-line-counts
        hs-isearch-open t))

;;; Bug Reference mode
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
(add-hook 'gnus-article-mode-hook #'bug-reference-mode)

;;; Goto Address mode
(global-goto-address-mode +1)

;;; Fly Spell mode
(setup flyspell
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; Set global key bindings
  (:global "C-c c b" flyspell-buffer
           "C-c c r" flyspell-region)
  ;; Disable conflicting key binding
  (:unbind "C-M-i")
  ;; Correct some annoying defaults
  (:option flyspell-use-meta-tab nil
           flyspell-issue-message-flag nil
           flyspell-issue-welcome-flag nil
           flyspell-consider-dash-as-word-delimiter-flag t
           ;; Don't slowdown looking for duplicates
           flyspell-duplicate-distance 12000))

;;; Ispell
(setup ispell
  ;; Set global key binding
  (:global "C-c c d" 'ispell-change-dictionary)
  ;; Ensure spell checking program is available
  (:option ispell-program-name (executable-find "aspell")
           ;; Extra switches
           ispell-extra-args '("--sug-mode=ultra")
           ;; Default dictionary
           ispell-dictionary "english"))

;;; Isearch
(setq isearch-allow-scroll t)
;; Match similar chars
(setq search-default-mode #'char-fold-to-regexp)
;; Display number of matches
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) ")
;; Allow search string extension with motion commands
(setq isearch-yank-on-move 'shift)
;; Add local key binding for `isearch-occur'
(keymap-set isearch-mode-map "C-o" 'isearch-occur)

;;; Diff mode
(after-load 'diff-mode
  ;; More prettier diff format
  (setq diff-font-lock-prettify t))

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
(keymap-global-set "C-x C-b" 'ibuffer)
;; Configuration
(after-load 'ibuffer
  ;; Use a default buffer filter
  (setq ibuffer-saved-filter-groups
        '(("primary"
           ("Async Compilation" (or (name . "*Async-native-compile-log*")
                                    (mode . native-comp-limple-mode)))
           ("Calculator" (or (mode . calc-mode)
                             (mode . calc-trail-mode)))
           ("Code" (and (or (derived-mode . prog-mode)
                            (derived-mode . conf-mode)
                            (derived-mode . yaml-mode)
                            (derived-mode . sgml-mode))
                        (not (name . "*scratch*"))))
           ("Compilation" (derived-mode . compilation-mode))
           ("Custom" (derived-mode . Custom-mode))
           ("Dired" (mode . dired-mode))
           ("Diff" (derived-mode . diff-mode))
           ("Document" (or (derived-mode . latex-mode)
                           (derived-mode . markdown-mode)
                           (derived-mode . pdf-view-mode)
                           (derived-mode . nov-mode)))
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
                      (derived-mode . geiser-messages-mode)
                      (mode . tags-table-mode)
                      (name . "*nrepl-server")
                      (name . "*Flymake log*")
                      (name . "*sly-inferior-lisp for sbcl*")
                      (name . "*sly-events for sbcl*")
                      (name . "*inferior-lisp*")
                      (name . "*Warnings*")))
           ("Eglot" (name . "*EGLOT"))
           ("Mail" (or (derived-mode . message-mode)
                       (derived-mode . mail-mode)))
           ("Newsticker" (derived-mode . newsticker-treeview-mode))
           ("Org" (derived-mode . org-mode))
           ("Packages" (derived-mode . package-menu-mode))
           ("Proced" (derived-mode . proced-mode))
           ("REPL" (or (derived-mode . cider-repl-mode)
                       (derived-mode . geiser-repl-mode)
                       (derived-mode . sly-mrepl-mode)
                       (derived-mode . skewer-repl-mode)
                       (name . "*ruby*")
                       (name . "*Python*")))
           ("Shell" (or (derived-mode . eshell-mode)
                        (derived-mode . shell-mode)))
           ("Terminal" (or (derived-mode . term-mode)
                           (derived-mode . vterm-mode)))
           ("Text" (or (mode . text-mode)
                       (derived-mode . reb-mode)))
           ("TRAMP" (name . "*tramp"))
           ("Web" (or (derived-mode . eww-mode)
                      (derived-mode . elpher-mode)))
           ("Ztree" (mode . ztree-mode)))))
  ;; Load the default buffer filter
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "primary")))
  ;; Don't show empty filter groups and jump only to visible buffers
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-jump-offer-only-visible-buffers t)
  ;; Split window instead for buffer display
  (setq ibuffer-use-other-window t))

;;; Find file at point
(after-load 'ffap
  ;; Require prefix
  (setq ffap-require-prefix t
        dired-at-point-require-prefix t)
  ;; Disable pinging to avoid slowdowns
  (setq ffap-machine-p-known 'reject)
  ;; Default RFC path
  (setq ffap-rfc-path "https://ietf.org/rfc/rfc%s.txt"))
;; Initialize mode
(ffap-bindings)

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

;;; Image mode
(after-load 'image-mode
  ;; Show image dimension function
  (defun site/image-dimensions-minor-mode ()
    "Display image dimensions in the mode line."
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format " (%dx%d)" width height))))
  ;; Apply the custom hook
  (add-hook 'image-mode-hook #'site/image-dimensions-minor-mode)
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

;;; Apropos configuration
(after-load 'apropos
  ;; Search more extensively
  (setq apropos-do-all t))

;;; Python mode configuration
(after-load 'python
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
  (setq scheme-program-name "guile3.0"))

;;; CSS mode
(after-load 'css-mode
  ;; Indent level
  (setq css-indent-offset 2))

;;; NXML mode
(after-load 'nxml-mode
  ;; Auto complete closing tags
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
  ;; Prompt in minibuffer
  (setq epg-pinentry-mode 'loopback))

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
  ;; Search filenames only when point is on them
  (setq dired-isearch-filenames 'dwim)
  ;; Revert buffer automatically after file operations
  (setq dired-do-revert-buffer t)
  ;; If we are on a GNU system add some more `ls' switches
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first")))
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
(after-load 'dired-x
  ;; Omit dotfiles as well
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  ;; Use xdg-open for every file
  (setq dired-guess-shell-alist-user '(("" "xdg-open"))))

;; Wdired movement and editable parts
(after-load 'wdired
  ;; Allow changing of permissions and symbolic links too
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-links t)
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

;; Dired Find
(after-load 'dired-find
  ;; Faster search
  (setq find-ls-option '("-exec ls -ld {} \\+" . "-ld")))

;;; TRAMP
(after-load 'tramp
  ;; Change default method
  (setq tramp-default-method "ssh"))

;;; Bookmarks
(after-load 'bookmark
  ;; Enable mode
  (setq bookmark-save-flag 1))

;;; Copyright insertion
(keymap-global-set "C-c i c" 'copyright)
(keymap-global-set "C-c i C-c" 'copyright-update)
;; Configuration
(after-load 'copyright
  ;; Change default format
  (setq copyright-year-ranges t
        copyright-names-regexp (regexp-quote user-login-name)))

;;; Regexp builder
(keymap-global-set "C-c s b" 're-builder)
;; Configuration
(after-load 're-builder
  ;; Default regex syntax
  (setq reb-re-syntax 'string))

;;; Proced
(keymap-global-set "C-x p" 'proced)
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
(keymap-global-set "C-c w w" 'eww)
(keymap-global-set "C-c w C-b" 'eww-list-bookmarks)

;;; Browse URL
(keymap-global-set "C-c w b" 'browse-url)
;; Configuration
(after-load 'browse-url
  ;;  Open URLs with the specified browser
  (setq browse-url-browser-function #'browse-url-xdg-open))

;;; Speedbar
(keymap-global-set "C-c p s" 'speedbar)
;; Configuration
(after-load 'speedbar
  ;; Set local key binding
  (keymap-set speedbar-mode-map "a" 'speedbar-toggle-show-all-files)
  ;; Emulate NERDTree behavior
  (setq speedbar-show-unknown-files t
        speedbar-directory-unshown-regexp "^$")
  ;; Don't ignore the following extensions
  (speedbar-add-supported-extension
   '(".lisp" ".clj" ".lua" ".css" ".patch"
     ".conf" ".diff" ".sh" ".org" ".md" ".deb")))

;;; Eshell
(keymap-global-set "<f6>" 'eshell)
;; Configuration
(after-load 'eshell
  ;; Ignore duplicates and case
  (setq eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t)
  (defun site/eshell-mode-setup ()
    "Custom hook to run for my Eshell buffers."
    ;; Disable Company since we use `completion-at-point'
    (company-mode -1)
    ;; Add Outline support for Eshell prompts
    (setq-local outline-regexp eshell-prompt-regexp))
  ;; Apply the custom hook
  (add-hook 'eshell-mode-hook #'site/eshell-mode-setup))

;; Eshell smart display
(after-load 'eshell
  ;; Load library
  (require 'em-smart)
  ;; Initialize mode
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize))

;;; Shell mode
(after-load 'shell
  ;; Custom hook to avoid conflicts
  (defun site/shell-mode-setup ()
    "Custom hook to run for my Shell buffers."
    ;; Enable clickable file paths and disable Company
    (compilation-shell-minor-mode +1)
    (company-mode -1))
  ;; Apply the custom hook
  (add-hook 'shell-mode-hook #'site/shell-mode-setup))

;;; Enhanced shell command completion
(setup pcmpl-args (:elpaca t))

;;; IELM
(keymap-global-set "C-c r i" 'ielm)
;; Configuration
(after-load 'ielm
  ;; Change default prompt
  (setq ielm-prompt "(>) "))

;;; Flymake
(keymap-global-set "C-c ! t" 'flymake-mode)
;; Configuration
(after-load 'flymake
  ;; Define Transient command
  (transient-define-prefix site/flymake-transient ()
    "Transient for Flymake commands."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Errors"
     ("n" "Next Error" flymake-goto-next-error)
     ("p" "Previous Error" flymake-goto-prev-error)])
  ;; Set local key bindings
  (dolist (bind '(("C-c ! n" . flymake-goto-next-error)
                  ("C-c ! p" . flymake-goto-prev-error)
                  ("C-c ! C-r" . flymake-reporting-backends)
                  ("C-c ! r" . flymake-running-backends)
                  ("C-c ! d" . flymake-show-diagnostics-buffer)
                  ("C-c ! l" . flymake-switch-to-log-buffer)
                  ("C-c ! c" . consult-flymake)
                  ("C-c ! h" . site/flymake-transient)))
    (keymap-set flymake-mode-map (car bind) (cdr bind))))

;;; Comint mode
(after-load 'comint
  ;; Ignore duplicate commands
  (setq comint-input-ignoredups t)
  ;; Process OSC escape sequences
  (add-to-list 'comint-output-filter-functions 'comint-osc-process-output))

;;; Compilation
(keymap-global-set "<f5>" 'recompile)
;; Configuration
(after-load 'compile
  ;; Apply ANSI colorization
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Change default behavior
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        compilation-scroll-output 'first-error
        compilation-context-lines 3))

;;; Mail sending
(after-load 'message
  ;; Set main directory
  (setq message-directory "~/.mail/")
  ;; Confirm sending and kill buffer on message send success
  (setq message-confirm-send t
        message-kill-buffer-on-exit t))

;; Send mail configuration
(after-load 'sendmail
  ;; Use `msmtp' if possible
  (when (executable-find "msmtp")
    (setq sendmail-program "/usr/bin/msmtp"))
  ;; Change default send mail function
  (setq send-mail-function #'message-send-mail-with-sendmail)
  ;; Make compatible with `msmtp'
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header))

;; Mail composing configuration
(after-load 'message
  ;; Make compatible with `msmtp'
  (setq message-sendmail-envelope-from 'header))

;;; Smileys
(after-load 'smiley
  ;; Resize smileys
  (setq smiley-style 'emoji))

;;; MIME decoding configuration
(after-load 'mm-decode
  ;; Fit images to buffer
  (setq mm-inline-large-images 'resize))

;;; Gnus
(keymap-global-set "<f9>" 'gnus)
;; Configuration
(after-load 'gnus
  ;; Main Gnus directory
  (setq gnus-directory "~/.news/")
  ;; Configure news server
  (setq gnus-select-method
        '(nntp "news.gmane.io"))
  ;; Article fetching options
  (setq gnus-article-browse-delete-temp t
        gnus-treat-strip-trailing-blank-lines 'last
        gnus-mime-display-multipart-related-as-mixed t)
  ;; Don't auto select first article
  (setq gnus-auto-select-first nil)
  ;; Group by topics
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  ;; Ignore certain newsgroups
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
  ;; Configure visible headers
  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
  ;; Show the article headers in this order
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))
  ;; Specify article age format
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ 86400 (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))
  ;; Display gravatars
  (setq gnus-treat-from-gravatar 'head)
  ;; Use notifications
  (add-hook 'gnus-after-getting-new-news-hook #'gnus-notifications)
  ;; Set return email address based on incoming email address
  (setq gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com"))))
  ;; Group format
  (setq gnus-group-line-format "%M%S%p%P%5y:%B %G\n")
  ;; Summary buffer format
  (setq gnus-summary-line-format "%U%R%z %-16,16&user-date; │%4L ― %-20,20f  %B%S\n")
  ;; Summary buffer sorting
  (setq gnus-summary-thread-gathering-function #'gnus-gather-threads-by-subject
        ;; Sort threads
        gnus-thread-sort-functions
        '((not gnus-thread-sort-by-number)
          (not gnus-thread-sort-by-date))
        ;; Sort subthreads
        gnus-subthread-sort-functions #'gnus-thread-sort-by-date)
  ;; Message threading format
  (setq gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "■ "
        gnus-sum-thread-tree-false-root "□ "
        gnus-sum-thread-tree-single-indent "▣ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-vertical "│"
        gnus-sum-thread-tree-single-leaf "└─▶ "))

;;; Use Gnus as the default mail program
(setq mail-user-agent 'gnus-user-agent
      read-mail-command #'gnus)

;;; Newsticker
(keymap-global-set "C-<f9>" 'newsticker-treeview)
;; Configuration
(after-load 'newst-backend
  ;; Purge default sources
  (setq newsticker-url-list-defaults nil)
  ;; Add my feeds
  (setq newsticker-url-list
        '(("Bljesak.info" "http://bljesak.info/rss")
          ("Klix.ba" "https://www.klix.ba/rss/naslovnica")
          ("Hacker News" "https://news.ycombinator.com/rss")
          ("LWN" "https://lwn.net/headlines/rss")
          ("Reddit Emacs" "https://www.reddit.com/r/emacs/.rss")
          ("Reddit Linux" "https://www.reddit.com/r/linux/.rss")
          ("Reddit Programming" "https://www.reddit.com/r/programming/.rss")))
  ;; Retrieve feeds manually
  (setq newsticker-retrieval-interval 0))
;; Enable Imenu for Plainview mode
(add-hook 'newsticker-mode-hook #'imenu-add-menubar-index)

;;; Calendar
(keymap-global-set "<f12>" 'calendar)
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
(keymap-global-set "C-c t o" 'outline-minor-mode)
;; Set default prefix
(setq outline-minor-mode-prefix (kbd "C-c C-o"))
;; Configuration
(after-load 'outline
  ;; Define Transient command
  (transient-define-prefix site/outline-transient ()
    "Transient for Outline commands."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Outline Control"
     ["Hide"
      ("q" "Sublevels" outline-hide-sublevels)
      ("t" "Body" outline-hide-body)
      ("o" "Other" outline-hide-other)
      ("c" "Entry" outline-hide-entry)
      ("l" "Leaves" outline-hide-leaves)
      ("d" "Subtree" outline-hide-subtree)]
     ["Show"
      ("a" "All" outline-show-all)
      ("e" "Entry" outline-show-entry)
      ("i" "Children" outline-show-children)
      ("k" "Branches" outline-show-branches)
      ("s" "Subtree" outline-show-subtree)]
     ["Move"
      ("u" "Up" outline-up-heading)
      ("n" "Next Visible" outline-next-visible-heading)
      ("p" "Previous Visible" outline-previous-visible-heading)
      ("f" "Forward Same Level" outline-forward-same-level)
      ("b" "Backward Same Level" outline-backward-same-level)]])
  ;; Set local key binding
  (keymap-set outline-minor-mode-map "C-c o h" 'site/outline-transient))

;;; Org-mode
(defun site/toggle-table-mode ()
  "Initialize Org Table mode."
  (interactive)
  ;; Rather hacky mode, needs to be manually loaded
  (require 'org-table)
  ;; And manually checked to be enabled/disabled
  (if (bound-and-true-p orgtbl-mode)
      (orgtbl-mode -1)
    (orgtbl-mode +1)))
;; Set global key bindings
(dolist (bind '(("C-c o a" . org-agenda)
                ("C-c o c" . org-capture)
                ("C-c o t" . org-todo-list)
                ("C-c o s" . org-search-view)
                ("C-c o l" . org-store-link)
                ("C-c t t" . site/toggle-table-mode)))
  (keymap-global-set (car bind) (cdr bind)))
;; Configuration
(after-load 'org
  ;; Default directory and file location
  (setq org-directory "~/Documents/org"
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files '("~/Documents/org"))
  ;; Add replacements for task symbols
  (defun site/org-prettify-task-symbols-setup ()
    "Prettify `org-mode' task list symbols."
    (dolist (symbol '(("TODO"     . ?⚑)
                      ("DOING"    . ?⚐)
                      ("CANCELED" . ?✘)
                      ("DONE"     . ?✔)))
      (cl-pushnew symbol prettify-symbols-alist :test #'equal)))
  ;; Apply the custom hook
  (add-hook 'org-mode-hook #'site/org-prettify-task-symbols-setup)
  ;; Make Wind Move work in Org mode
  (add-hook 'org-shiftup-final-hook #'windmove-up)
  (add-hook 'org-shiftleft-final-hook #'windmove-left)
  (add-hook 'org-shiftdown-final-hook #'windmove-down)
  (add-hook 'org-shiftright-final-hook #'windmove-right)
  ;; Display agenda in current window
  (setq org-agenda-window-setup 'current-window)
  ;; Record time when a task is done
  (setq org-log-done 'time)
  ;; Smart avoidance for collapsed heading edits
  (setq org-catch-invisible-edits 'smart)
  ;; Change fontification for done headings
  (setq org-fontify-done-headline t)
  ;; Movement options
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-use-speed-commands t
        org-yank-adjusted-subtrees t)
  ;; Native source code behavior
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)
  ;; Default `org-goto' interface
  (setq org-goto-interface 'outline-path-completion)
  ;; Default LaTeX compiler
  (setq org-latex-compiler "lualatex")
  ;; LaTeX syntax highlight
  (setq org-highlight-latex-and-related '(entities latex script))
  ;; LaTeX preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ;; Store LaTeX preview images in /tmp
  (setq org-preview-latex-image-directory (concat temporary-file-directory "ltximg/")))

;; Org time clocking
(after-load 'org-clock
  ;; Start from the last closed clock
  (setq org-clock-continuously t))

;;; Convert to HTML
(setup htmlize (:elpaca t))

;;; Time display
(keymap-global-set "C-<f12>" 'world-clock)
;; Configuration
(after-load 'time
  ;; Use custom mode line format
  (setq display-time-string-forms
        '(" {"
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
          "} "))
  ;; Change default mail monitoring directory
  (setq display-time-mail-directory "~/.mail/Inbox/new")
  ;; Use icon for mail notifications
  (setq display-time-use-mail-icon t)
  ;; Time zones we are interested in
  (setq world-clock-list
        '(("Europe/Riga" "Riga")
          ("America/Los_Angeles" "Los Angeles")
          ("Canada/Eastern" "Toronto")
          ("Asia/Saigon" "Saigon")
          ("UTC" "Universal"))))

;;; 0x0 paste support
(setup 0x0
       (:elpaca t)
       ;; Set global key bindings
       (:global "C-c b y" 0x0-upload-kill-ring
                "C-c f u" 0x0-upload-file
                "C-c x y" 0x0-upload-text))

;;; Ace-window
(setup ace-window
  (:elpaca t)
  ;; Set global key binding
  (:global "M-o" ace-window))
;; Configuration
(after-load 'ace-window
  ;; Use keys on the home row
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; CIDER
(setup cider (:elpaca t))
;; Configuration
(after-load 'cider-mode
  ;; More rich syntax highlight
  (setq cider-font-lock-dynamically '(macro core function var))
  ;; Enable fuzzy completion with Company
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;; CIDER REPL
(keymap-global-set "C-c r c" 'cider-connect-clj)
;; Configuration
(after-load 'cider-repl
  ;; Cycle through history
  (setq cider-repl-wrap-history t)
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
(after-load 'clojure-mode
  ;; Enable CIDER mode
  (add-hook 'clojure-mode-hook #'cider-mode)
  ;; Enable SubWord mode
  (add-hook 'clojure-mode-hook #'subword-mode))

;;; CSV mode
(setup csv-mode (:elpaca csv-mode))

;;; Dash
(setup dash (:elpaca t))
(after-load 'dash
  ;; Enable syntax coloring for Dash functions
  (global-dash-fontify-mode +1))

;;; Debbugs browser
(setup debbugs
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-c d g" debbugs-gnu
           "C-c d s" debbugs-gnu-search
           "C-c d t" debbugs-gnu-usertags
           "C-c d p" debbugs-gnu-patches
           "C-c d b" debbugs-gnu-bugs
           "C-c d C-o" debbugs-org
           "C-c d C-s" debbugs-org-search
           "C-c d C-p" debbugs-org-patches
           "C-c d C-b" debbugs-org-bugs))

;;; Dired Filter
(setup dired-filter (:elpaca t))
;; Configuration
(after-load 'dired-x
  ;; Load library
  (require 'dired-filter)
  ;; Set local key binding
  (keymap-set dired-mode-map "\\" dired-filter-mark-map))

;;; Dired Rainbow
(setup dired-rainbow (:elpaca t))
;; Configuration
(after-load 'dired-filter
  ;; Load library
  (require 'dired-rainbow)
  ;; Define faces by file type
  (defvar dired-rainbow-ext-to-face nil)
  (dired-rainbow-define html "#eb5286"
                        ("css" "less" "sass" "scss" "htm" "html"
                         "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define key-value "#f2d024"
                        ("xml" "xsd" "xsl" "xslt" "wsdl" "bib"
                         "json" "msg" "pgn" "rss" "yaml" "yml"
                         "rdata" "sls"))
  (dired-rainbow-define document "#9561e2"
                        ("docm" "doc" "docx" "odb" "odt" "pdb"
                         "pdf" "ps" "rtf" "djvu" "epub" "odp"
                         "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a"
                        ("org" "etx" "info" "markdown" "md"
                         "mkd" "nfo" "pod" "rst" "tex"
                         "textfile" "txt"))
  (dired-rainbow-define database "#6574cd"
                        ("xlsx" "xls" "csv"
                         "accdb" "db" "mdb"
                         "sqlite" "nc"))
  (dired-rainbow-define media "#de751f"
                        ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg"
                         "mpg" "flv" "ogg" "mov" "mid" "midi"
                         "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b"
                        ("tiff" "tif" "cdr" "gif" "ico" "jpeg"
                         "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define patch (:foreground "#e3342f" :bold t :inverse-video t) ".*\\.patch")
  (dired-rainbow-define shell "#f6993f"
                        ("awk" "bash" "bat"
                         "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172"
                        ("py" "ipynb" "rb" "pl" "t" "msql"
                         "mysql" "pgsql" "sql" "r" "clj"
                         "cljs" "scala" "js" "scm"))
  (dired-rainbow-define compiled "#4dc0b5"
                        ("asm" "cl" "lisp" "el" "c" "h" "c++"
                         "h++" "hpp" "hxx" "m" "cc" "cs" "cp"
                         "cpp" "go" "f" "for" "ftn" "f90"
                         "f95" "f03" "f08" "s" "rs" "hi" "hs"
                         "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a"
                        ("7z" "zip" "bz2" "tgz" "txz" "gz"
                         "xz" "z" "Z" "jar" "war" "ear"
                         "rar" "sar" "xpi" "apk" "xz"
                         "tar"))
  (dired-rainbow-define packaged "#faad63"
                        ("deb" "rpm" "apk" "jad" "jar" "cab"
                         "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a"
                        ("gpg" "pgp" "asc" "bfe" "enc"
                         "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb"
                        ("afm" "fon" "fnt"
                         "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f"
                        ("dmg" "iso" "bin" "nrg" "qcow"
                         "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9"
                        ("git" "gitignore" "gitattributes" "gitmodules"))
  ;; Define faces by file permission
  (dired-rainbow-define-chmod executable-unix (:foreground "gold" :bold t) "-.*x.*")
  (dired-rainbow-define-chmod directory-unix (:foreground "DeepSkyBlue" :bold t) "d.*")
  (dired-rainbow-define-chmod symlink-unix (:foreground "violet" :underline t) "l.*"))

;;; Extra miscellaneous colorization
(setup diredfl (:elpaca t))
;; Configuration
(after-load 'dired-x
  ;; Enable mode
  (diredfl-global-mode +1)
  ;; Use `dired-rainbow' directory face
  (setq diredfl-dir-name 'dired-rainbow-directory-unix-face))

;;; Dired Subtree
(setup dired-subtree (:elpaca t))
;; Configuration
(after-load 'diredfl
  ;; Enable mode
  (require 'dired-subtree)
  ;; Create local key map
  (defvar-keymap dired-subtree-map
    :doc "Keymap for Dired Subtree"
    "C-i" #'dired-subtree-insert
    "C-/" #'dired-subtree-apply-filter
    "C-k" #'dired-subtree-remove
    "C-n" #'dired-subtree-next-sibling
    "C-p" #'dired-subtree-previous-sibling
    "C-u" #'dired-subtree-up
    "C-d" #'dired-subtree-down
    "C-a" #'dired-subtree-beginning
    "C-e" #'dired-subtree-end
    "C-c" #'dired-subtree-cycle
    "m" #'dired-subtree-mark-subtree
    "u" #'dired-subtree-unmark-subtree
    "C-o C-f" #'dired-subtree-only-this-file
    "C-o C-d" #'dired-subtree-only-this-directory)
  ;; Set local key binding for the key map
  (keymap-set dired-mode-map "C-," dired-subtree-map))

;;; Dired Ranger
(setup dired-ranger (:elpaca t))
;; Configuration
(after-load 'dired-subtree
  ;; Enable mode
  (require 'dired-ranger)
  ;; Create local keymap
  (defvar-keymap dired-ranger-map
    :doc "Keymap for Dired Ranger"
    "c" #'dired-ranger-copy
    "p" #'dired-ranger-paste
    "m" #'dired-ranger-move)
  ;; Set local key binding for the key map
  (keymap-set dired-mode-map "r" dired-ranger-map)
  ;; Bookmarking
  (keymap-set dired-mode-map "'" 'dired-ranger-bookmark)
  (keymap-set dired-mode-map "`" 'dired-ranger-bookmark-visit))

;;; Dired Narrow
(setup dired-narrow (:elpaca t))
;; Configuration
(after-load 'dired-ranger
  ;; Enable mode
  (require 'dired-narrow)
  ;; Set local key binding
  (keymap-set dired-mode-map "C-." 'dired-narrow)
  ;; Exit on single match
  (setq dired-narrow-exit-when-one-left t))

;;; Dired Collapse
(setup dired-collapse (:elpaca t))
;; Configuration
(after-load 'dired-narrow
  ;; Enable mode
  (require 'dired-collapse)
  ;; Set local key binding
  (keymap-set dired-mode-map "," 'dired-collapse-mode))

;;; Dired-du
(setup dired-du (:elpaca t))
;; Configuration
(after-load 'dired-collapse
  ;; Enable mode
  (require 'dired-du)
  ;; Use human readable output by default
  (setq dired-du-size-format t))

;;; Dired Async
(setup async (:elpaca t))
;; Configuration
(after-load 'dired-du
  ;; Enable mode
  (require 'dired-async)
  ;; Set local key bindings
  (dolist (bind '(("E c" . dired-async-do-copy)
                  ("E r" . dired-async-do-rename)
                  ("E s" . dired-async-do-symlink)
                  ("E h" . dired-async-do-hardlink)
                  ("E m" . dired-async-mode)))
    (keymap-set dired-mode-map (car bind) (cdr bind))))

;;; Dired rsync
(setup dired-rsync (:elpaca t))
;; Configuration
(after-load 'dired-async
  ;; Set local key binding
  (keymap-set dired-mode-map "C-c C-r" 'dired-rsync))

;;; Docker
(setup docker
  (:elpaca t)
  ;; Set key binding
  (:global "C-c r d" docker))

;;; Dockerfile mode
(setup dockerfile-mode (:elpaca t))

;;; Docker Compose mode
(setup docker-compose-mode
  (:elpaca t)
  ;; Enable mode
  (add-to-list 'auto-mode-alist '("/docker-compose.yml\\'" . docker-compose-mode)))

;;; Elpher Gopher browser
(setup elpher (:elpaca t))
;; Set global key binding
(keymap-global-set "C-c w e" 'elpher)
;; Configuration
(after-load 'elpher
  ;; Colorize escape sequences by default
  (setq elpher-filter-ansi-from-text t))

;;; Expand region
(setup expand-region (:elpaca t))
;; Add missing autoloads
(autoload #'er/mark-defun "er-basic-expansions"
  "Mark defun around or in front of point." t)
(autoload #'er/mark-text-paragraph "text-mode-expansions"
  "Marks one paragraph." t)
;; Define Transient for text marking operations.
(transient-define-prefix site/mark-text-transient ()
  "Transient for text marking commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Expand Region"
   ["Lisp"
    ("e" "S-Expression" mark-sexp)
    ("f" "Function" er/mark-defun)
    ("s" "Symbol" er/mark-symbol)
    ("S" "Prefixed Symbol" er/mark-symbol-with-prefix)]
   ["Text"
    ("w" "Word" er/mark-word)
    ("p" "Paragraph" er/mark-text-paragraph)
    ("c" "Comment" er/mark-comment)
    ("u" "URL" er/mark-url)
    ("E" "Email" er/mark-email)]
   ["Region"
    ("." "Expand Region" er/expand-region)
    ("," "Contract Region" er/contract-region)]
   ["Quotes"
    ("q" "Inside Quotes" er/mark-inside-quotes)
    ("Q" "Outside Quotes" er/mark-outside-quotes)]
   ["Parentheses"
    ("(" "Opening Pair" er/mark-inside-pairs)
    ("[" "Opening Pair" er/mark-inside-pairs)
    ("{" "Opening Pair" er/mark-inside-pairs)
    (")" "Closing Pair" er/mark-outside-pairs)
    ("]" "Closing Pair" er/mark-outside-pairs)
    ("}" "Closing Pair" er/mark-outside-pairs)]])
;; Set global key bindings
(keymap-global-set "C-c x C-SPC" 'site/mark-text-transient)
(keymap-global-set "C-=" 'er/expand-region)

;;; Flymake ShellCheck support
(setup flymake-shellcheck (:elpaca t)
  ;; Enable mode
  (when (executable-find "shellcheck")
    (add-hook 'sh-mode-hook #'flymake-shellcheck-load)))

;;; Geiser Guile
;; (setup geiser
;;   (:elpaca t)
;;   (:option geiser-default-implementation
;;            (when (executable-find "guile")
;;                 (:elpaca geiser-guile)
;;                 'guile))
;;   (:hook-into scheme-mode))

;;; Go mode
(setup go-mode (:elpaca t))

;;; Goggles
(setup goggles
  (:elpaca t)
  ;; Enable mode
  (:hook-into prog-mode
              text-mode
              conf-mode))
;; Configuration
(after-load 'goggles
  ;; Don't pulse just highlight
  (setq-default goggles-pulse nil))

;;; Iedit
(setup iedit (:elpaca t))
;; Add missing autoloads
(autoload #'iedit-mode-from-isearch "iedit"
  "Start Iedit mode using last search string as the regexp." t)
(autoload #'iedit-execute-last-modification "iedit"
  "Apply last modification in Iedit mode to the current buffer or an active region." t)
;; Set global key binding
(keymap-global-set "C-c i e" 'iedit-mode)
;; Set local key bindings
(keymap-set isearch-mode-map "C-;" 'iedit-mode-from-isearch)
(keymap-set esc-map "C-;" 'iedit-execute-last-modification)
(keymap-set help-map "C-;" 'iedit-mode-toggle-on-function)
;; Configuration
(after-load 'iedit
  ;; Disable conflicting key binding
  (setq iedit-toggle-key-default nil))

;;; JavaScript 2 mode
(setup js2-mode
  (:elpaca t)
  ;; Enable mode
  (add-hook 'js-mode-hook #'js2-minor-mode))
;; Configuration
(after-load 'js2-mode
  ;; Syntax defaults
  (setq js2-basic-offset 2
        js2-highlight-level 3)
  ;; Highlight unused variables
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;;; Eglot
(setup eglot (:elpaca t))
(setup consult-eglot (:elpaca t))
;; Set global key binding
(keymap-global-set "C-c t e" 'eglot)
;; Configuration
(after-load 'eglot
  ;; Set local key bindings
  (dolist (bind '(("C-c e c" . eglot-reconnect)
                  ("C-c e s" . eglot-shutdown)
                  ("C-c e r" . eglot-rename)
                  ("C-c e f" . eglot-format)
                  ("C-c e a" . eglot-code-actions)
                  ("C-c e b" . eglot-events-buffer)
                  ("C-c e e" . eglot-stderr-buffer)))
    (keymap-set eglot-mode-map (car bind) (cdr bind)))
  ;; Consult Eglot
  (keymap-set eglot-mode-map "C-M-." 'consult-eglot-symbols))

;;; EPUB format support
(setup nov
  (:elpaca t)
  ;; Enable mode
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
;; Configuration
(after-load 'nov
  ;; Change default font
  (defun site/nov-font-setup ()
    "Apply custom variable pitch font for `nov.el'."
    (face-remap-add-relative 'variable-pitch :family "Noto Serif"))
  ;; Apply the custom hook
  (add-hook 'nov-mode-hook #'site/nov-font-setup)
  ;; Text filling
  (setq nov-text-width 80))

;;; Macrostep
(setup macrostep
  (:elpaca t)
  ;; Set key binding
  (dolist (mode '(emacs-lisp-mode-map
                  lisp-interaction-mode-map))
    (keymap-set (symbol-value mode) "C-c M-e" 'macrostep-expand)))

;;; Magit
(setup magit
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-c g c" magit-clone
           "C-c g b" magit-blame
           "C-c g l" magit-log-buffer-file
           "C-c g p"  magit-pull)
  ;; Configuration
  (after-load 'magit
    ;; Insert submodule sections
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append)))

;;; git additional modes
(setup git-modes (:elpaca t))

;;; Groovy mode
(setup groovy-mode (:elpaca t))

;;; i3 WM configuration mode
(setup i3wm-config-mode (:elpaca t))

;;; Markdown mode
(setup markdown-mode (:elpaca t))
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
  (defun site/markdown-org-table-align-advice ()
    "Replace \"+\" sign with \"|\" in tables."
    (when (member major-mode '(markdown-mode gfm-mode))
      (save-excursion
        (save-restriction
          (narrow-to-region (org-table-begin) (org-table-end))
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-"))))))
  ;; Add advice for table alignment
  (advice-add 'org-table-align :after #'site/markdown-org-table-align-advice)
  ;; Enable `visual-line-mode' in Markdown buffers and disable `auto-fill-mode'
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  ;; Additional fontification
  (setq markdown-fontify-code-blocks-natively t
        markdown-header-scaling t)
  ;; Don't insert spaces after a code fence
  (setq markdown-spaces-after-code-fence 0))

;;; Move-text
(setup move-text (:elpaca t))
;; Configuration
(transient-define-prefix site/move-text-transient ()
  "Transient for Move-text commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Move Text"
   ("p" "Move Up" move-text-up)
   ("n" "Move Down" move-text-down)])

;; Add advice for indent after moving
(defun site/indent-region-advice (&rest ignored)
  "Indent region after certain action."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after #'site/indent-region-advice)
(advice-add 'move-text-down :after #'site/indent-region-advice)

;; Set global key binding
(keymap-global-set "C-c x m" 'site/move-text-transient)

;;; Multiple cursors
(setup multiple-cursors
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-c m <SPC>" mc/vertical-align-with-space
           "C-c m a" mc/vertical-align
           "C-c m e" mc/mark-more-like-this-extended
           "C-c m m" mc/mark-all-like-this-dwim
           "C-c m l" mc/edit-lines
           "C-c m n" mc/mark-next-like-this
           "C-c m p" mc/mark-previous-like-this
           "C-c m C-a" mc/edit-beginnings-of-lines
           "C-c m C-e" mc/edit-ends-of-lines
           "C-c m C-s" mc/mark-all-in-region)
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
            yank-rectangle)))
  ;; Commands to run only once
  (setq mc/cmds-to-run-once
        '(down-list
          mouse-drag-mode-line)))
;; Define Transient command
(transient-define-prefix site/multiple-cursors-transient ()
  "Transient for Multiple Cursors commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Multiple Cursors"
   ["Lines"
    ("l" "Edit Lines" mc/edit-lines)
    ("b" "Beginnings of Lines" mc/edit-beginnings-of-lines)
    ("e" "Ends of Lines" mc/edit-ends-of-lines)]
   ["Mark Like This"
    ("a" "All DWIM" mc/mark-all-dwim)
    ("s" "All Symbols" mc/mark-all-symbols-like-this)
    ("w" "All Words" mc/mark-all-words-like-this)
    ("r" "All Region" mc/mark-all-in-region)]
   ["Up"
    ("n" "Next" mc/mark-next-like-this)
    ("N" "Skip" mc/skip-to-next-like-this)
    ("M-n" "Unmark" mc/unmark-next-like-this)]
   ["Down"
    ("p" "Previous" mc/mark-previous-like-this)
    ("P" "Skip" mc/skip-to-previous-like-this)
    ("M-p" "Unmark" mc/unmark-previous-like-this)]
   ["Other"
    ("i" "Insert Numbers" mc/insert-numbers)
    ("R" "Mark All Region Regexp" mc/mark-all-in-region-regexp)
    ("f" "Mark All Region Defun" mc/mark-all-like-this-in-defun)
    ("S" "Mark All Symbols Defun" mc/mark-all-symbols-like-this-in-defun)
    ("W" "Mark All Words Defun" mc/mark-all-words-like-this-in-defun)]])
;; Set global key binding
(keymap-global-set "C-c m h" 'site/multiple-cursors-transient)

;;; PDF Tools
(setup pdf-tools
  (:elpaca t)
  ;; Enable mode
  (pdf-loader-install t))
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
    (setq pdf-misc-print-program "/usr/bin/lp")))

;; PDF Tools Midnight mode colors
(after-load 'pdf-view
  ;; Tomorrow Night palette
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#1d1f21")))

;; Display PDF files to the right always
(add-to-list 'display-buffer-alist
             '("\\.pdf\\(<[^>]+>\\)?$"
               display-buffer-in-direction
               (direction . right)))

;;; PHP mode
(setup php-mode (:elpaca t))

;;; PKGBUILD mode
(setup pkgbuild-mode (:elpaca t))

;;; Polymode Markdown
(setup poly-markdown (:elpaca t)
  ;; Enable mode
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

;;; Rainbow mode
(setup rainbow-mode (:elpaca t))
;; Set global key binding
(keymap-global-set "C-c t r" 'rainbow-mode)

;;; rcirc
(setup rcirc-color (:elpaca t))
;; Set global key binding
(keymap-global-set "<f8>" 'irc)
;; Configuration
(after-load 'rcirc
  ;; User defaults
  (setq rcirc-default-user-name "drot"
        rcirc-default-nick "drot")
  ;; Default server
  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :port 6697
           :encryption tls
           :channels ("#archlinux" "#emacs" "#bash" "#jenkins"))))
  ;; Use cert authorization
  (setq rcirc-authinfo
        `(("Libera.Chat" certfp
           ,(expand-file-name "~/Documents/Keys/libera-cert/libera.key")
           ,(expand-file-name "~/Documents/Keys/libera-cert/libera.crt"))))
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
  ;; Load `rcirc-color'
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (require 'rcirc-color)))
  ;; Disable company mode in rcirc buffers
  (add-hook 'rcirc-mode-hook
            (lambda () (company-mode -1)))
  ;; Exclude text properties when yanking text in rcirc buffers
  (add-to-list 'yank-excluded-properties 'rcirc-text))

;; rcirc color codes support
(setup rcirc-styles (:elpaca t))
;; Configuration
(after-load 'rcirc
  ;; Enable mode
  (require 'rcirc-styles)
  ;; Set local key bindings
  (dolist (bind '(("C-c C-e p" . rcirc-styles-toggle-preview)
                  ("C-c C-e a" . rcirc-styles-insert-attribute)
                  ("C-c C-e c" . rcirc-styles-insert-color)))
    (keymap-set rcirc-mode-map (car bind) (cdr bind)))
  ;; Use colors from theme
  (setq rcirc-styles-color-vector
        (vconcat (ef-themes-with-colors
                   (list fg-main
                         bg-cyan-subtle
                         blue
                         green
                         red
                         bg-removed-refine
                         magenta
                         cursor
                         yellow
                         green-cooler
                         cyan
                         cyan-faint
                         blue-faint
                         cyan-graph-1-bg
                         border
                         fg-dim))))
  ;; Apply the same for `rcirc-color'
  (setq rcirc-colors rcirc-styles-color-vector))

;;; Ruby inferior mode
(setup inf-ruby
  (:elpaca t)
  ;; Configuration
  (:option inf-ruby-default-implementation "pry"))

;;; Salt mode
(setup salt-mode (:elpaca t))

;;; Skewer
(setup skewer-mode
  (:elpaca t)
  ;; Set global key binding
  (:global "C-c r w" skewer-repl)
  ;; Enable mode
  (:hook-into js2-mode)
  ;; Skewer CSS
  (:with-mode css-mode
    (:hook skewer-css-mode))
  ;; Skewer HTML
  (:with-mode mhtml-mode
    (:hook skewer-html-mode)))

;;; SLY
(setup sly
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-c r s" sly
           "C-c r C-s" sly-connect))
;; Configuration
(after-load 'sly
  ;; Use SBCL by default
  (setq inferior-lisp-program (executable-find "sbcl"))
  ;; Disable conflicting key bindings
  (defun site/sly-sanitize-bindings ()
    "Removes SLY's conflicting keybindings."
    (cond ((boundp 'sly-mode-map)
           (keymap-unset sly-mode-map "C-c i")
           (keymap-unset sly-mode-map "C-c x"))
          ('t (message "SLY keybindings not sanitized!"))))
  ;; Apply the custom hook
  (add-hook 'sly-mode-hook #'site/sly-sanitize-bindings)
  ;; Set local key bindings
  (keymap-set sly-mode-map "C-c M-s i" 'sly-import-symbol-at-point)
  (keymap-set sly-mode-map "C-c M-s x" 'sly-export-symbol-at-point))

;;; SQL indentation mode
(setup sql-indent (:elpaca t))

;;; Tree-sitter
(setup tree-sitter-langs
  (:elpaca t)
  ;; Enable mode
  (global-tree-sitter-mode +1)
  ;; Enable syntax highlight
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; Unfill
(setup unfill
  (:elpaca t)
  ;; Set global key binding
  (:global "M-q" unfill-toggle))

;;; Systemd mode
(setup (:elpaca systemd-mode :host github :repo "drot/systemd-mode"))

;;; VTerm
(setup vterm
  (:elpaca t)
  ;; Set global key binding
  (:global "<f7>" vterm))
;; Configuration
(after-load 'vterm
  ;; Set buffer name
  (setq vterm-buffer-name-string "vterm - %s"
        vterm-max-scrollback 10000))

;;; YAML mode
(setup yaml-mode
  (:elpaca t)
  (:hook subword-mode))

;;; Ztree
(setup ztree
  (:elpaca t)
  ;; Set global key binding
  (:global "C-c f d" ztree-diff))
;; Configuration
(after-load 'ztree-view
  ;; Use unicode lines
  (setq ztree-draw-unicode-lines t)
  ;; Show subdirectories
  (setq ztree-show-number-of-children t))

;;; Avy
(setup avy
  (:elpaca t)
  ;; Enable mode
  (avy-setup-default))
;; Define Transient command
(transient-define-prefix site/avy-transient ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Cycle avy Candidates"
   ("n" "Next" avy-next)
   ("p" "Previous" avy-prev)])
;; Set global key bindings
(dolist (bind '(("C-:" . avy-goto-char)
                ("C-'" . avy-goto-char-timer)
                ("M-g f" . avy-goto-line)
                ("M-g w" . avy-goto-word-1)
                ("M-g e" . avy-goto-word-0)
                ("C-M-'" . site/avy-transient)))
  (keymap-global-set (car bind) (cdr bind)))
;; Configuration
(after-load 'avy
  ;; Work across all frames
  (setq avy-all-windows 'all-frames)
  ;; Follow indentation with the overlay
  (setq avy-indent-line-overlay t)
  ;; Dim background during selection
  (setq avy-background t
        avy-highlight-first t))

;;; Link-hint
(setup link-hint
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-c w o" link-hint-open-link
           "C-c w c" link-hint-copy-link))

;;; Company mode
(setup company
  (:elpaca t)
  ;; Set global key binding
  (:global "C-c i y" company-yasnippet)
  ;; Enable mode
  (global-company-mode +1))
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
  (setq company-show-quick-access t)
  ;; Tooltip behavior
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  ;; Dabbrev completion behavior
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

;; ;;; Company Statistics
(setup company-statistics
  (:elpaca t)
  ;; Enable mode
  (company-statistics-mode +1))

;;; Diff-Hl
(setup diff-hl
  (:elpaca t)
  ;; Set global key binding
  (:global "C-c t v" diff-hl-margin-mode)
  ;; Enable mode
  (global-diff-hl-mode +1)
  ;; Update diffs immediately
  (diff-hl-flydiff-mode +1))
;; Add hooks for `dired' and `magit'
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;; Add hook for terminal
(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (window-system)
              (diff-hl-margin-local-mode))))

;; ;;; Hl-Todo
(setup hl-todo
  (:elpaca t)
  ;; Enable mode
  (global-hl-todo-mode +1))
;; Configuration
(after-load 'hl-todo
  ;; Define Transient command
  (transient-define-prefix site/hl-todo-transient ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Highlight TODO"
     ("n" "Next" hl-todo-next)
     ("p" "Previous" hl-todo-previous)])
  ;; Set local key bindings
  (dolist (bind '(("M-s t" . hl-todo-occur)
                  ("C-c p t" . site/hl-todo-transient)
                  ("C-c p i" . hl-todo-insert-keyword)))
    (keymap-set hl-todo-mode-map (car bind) (cdr bind))))

;;; Page break lines
(setup page-break-lines
  (:elpaca t)
  ;; Enable mode
  (global-page-break-lines-mode +1))
;; Configuration
(after-load 'page-break-lines
  ;; Adhere to `fill-column' width
  (setq page-break-lines-max-width fill-column)
  ;; Add lines to NEWS buffer as well
  (add-to-list 'page-break-lines-modes 'emacs-news-view-mode))

;;; VERTical Interactive COmpletion
(setup vertico
  (:elpaca t)
  ;; Enable mode
  (vertico-mode +1)
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Hide commands in M-x which do not work in the current mode
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Hide completions buffer for menu items
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  ;; Enable mouse mode
  ;;(vertico-mouse-mode +1)
  ;; Use default TAB completion behavior
  (keymap-set vertico-map "?" 'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" 'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" 'minibuffer-complete)
  ;; Enable quick keys
  (keymap-set vertico-map "M-q" 'vertico-quick-insert)
  (keymap-set vertico-map "C-q" 'vertico-quick-exit)
  ;; Enable cycling for `vertico-next' and `vertico-previous'
  (setq vertico-cycle t))

;;; Orderless
(setup orderless
  (:elpaca t)
  ;; Load library
  (:require orderless)
  ;; Set completion style explicitly
  (setq completion-styles '(substring orderless))
  ;; Use `orderless' where possible
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;; Consult
(setup consult
  (:elpaca t)
  ;; C-c bindings (mode-specific-map)
  (:global 
   "C-c h c" consult-history
   "C-c h m" consult-mode-command
   "C-c f b" consult-bookmark
   "C-c k" consult-kmacro
   ;; C-x bindings (ctl-x-map)
   "C-x M-:" consult-complex-command         ;; orig. `repeat-complex-command'
   "C-x b" consult-buffer                    ;; orig. `switch-to-buffer'
   "C-x 4 b" consult-buffer-other-window ;; orig. `switch-to-buffer-other-window'
   "C-x 5 b" consult-buffer-other-frame  ;; orig. `switch-to-buffer-other-frame'
   ;; Custom M-# bindings for fast register access
   "M-#" consult-register-load
   "M-'" consult-register-store ;; orig. `abbrev-prefix-mark' (unrelated)
   "C-M-#" consult-register
   ;; Other custom bindings
   "<help> a" consult-apropos ;; orig. `apropos-command'
   ;; M-g bindings (goto-map)
   "M-g e" consult-compile-error
   "M-g g" consult-goto-line       ;; orig. `goto-line'
   "M-g M-g" consult-goto-line     ;; orig. `goto-line'
   "M-g o" consult-outline
   "M-g m" consult-mark
   "M-g k" consult-global-mark
   "M-g i" consult-imenu
   "M-g I" consult-imenu-multi
   ;; M-s bindings (search-map)
   "M-s f" consult-find
   "M-s L" consult-locate
   "M-s g" consult-grep
   "M-s G" consult-git-grep
   "M-s r" consult-ripgrep
   "M-s l" consult-line
   "M-s m" consult-multi-occur
   "M-s k" consult-keep-lines
   "M-s u" consult-focus-lines))

;; Isearch integration
(keymap-global-set "M-s e" 'consult-isearch-history)
;; Set local key bindings
(dolist (bind '(("M-e" . consult-isearch-history) ;; orig. `isearch-edit-string'
                ("M-s e" . consult-isearch-history) ;; orig. `isearch-edit-string'
                ("M-s l" . consult-line) ;; required by `consult-line' to detect isearch
                ("M-s L" . consult-line-multi))) ;; required by `consult-line' to detect isearch
  (keymap-set isearch-mode-map (car bind) (cdr bind)))

;; Minibuffer integration
(keymap-set minibuffer-local-map "M-s" 'consult-history)
(keymap-set minibuffer-local-map "M-r" 'consult-history)

;; Integrate with `register'
(setq register-preview-delay 0
      register-preview-function #'consult-register-preview)

;; Tweak the register preview window
(advice-add #'register-preview :override #'consult-register-window)

;; Integrate with `xref'
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Configuration
(after-load 'consult
  ;; Don't preview buffers eagerly
  (consult-customize
   consult-theme consult-buffer
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key (kbd "M-."))
  ;; Set narrowing key binding
  (setq consult-narrow-key (kbd "<"))
  ;; Blink after jumping
  (setq consult-after-jump-hook '(xref-pulse-momentarily))
  ;; Add Org support for `consult-buffer'
  (autoload 'org-buffer-list "org")

  (defvar org-buffer-source
    `(:name "Org"
            :narrow ?o
            :category buffer
            :state ,#'consult--buffer-state
            :items ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))

  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append))

;;; Marginalia in the minibuffer
(setup marginalia
  (:elpaca t)
  ;; Set key binding
  (:with-map minibuffer-local-map
    (:bind "M-A" marginalia-cycle))
  ;; Enable Mode
  (marginalia-mode +1)
  ;; Add `tab-bar-mode' support
  (add-to-list 'marginalia-prompt-categories '("tab by name" . tab)))

;;; Embark
(setup embark
  (:elpaca t)
  ;; Set global key bindings
  (:global "C-S-a" embark-act
           "C-S-d" embark-dwim
           "C-h B" embark-bindings)
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))
;; Configuration
(after-load 'embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Embark Consult integration
(setup embark-consult (:elpaca t))
;; Configuration
(after-load 'embark
  ;; Load library
  (require 'embark-consult)
  ;; Automatically preview entry at point in Embark Collect buffers
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Embark avy integration
(setup avy-embark-collect
  (:elpaca t)
  (after-load 'embark
    ;; Load library
    (require 'avy-embark-collect)
    ;; Set local key bindings
    (dolist (bind '(("C-'" . avy-embark-collect-choose)
                    ("C-\"" . avy-embark-collect-act)))
      (keymap-set embark-collect-mode-map (car bind) (cdr bind)))))

;;; Minions
(setup minions
  (:elpaca t)
  ;; Enable mode
  (minions-mode +1))
;; Configuration
(after-load 'minions
  ;; Change mode lighter and color
  (setq minions-mode-line-lighter "M+"
        minions-mode-line-face 'button)
  ;; Don't hide the following minor modes
  (setq minions-prominent-modes
        '(ace-window-mode
          artist-mode
          auto-fill-function
          auto-revert-mode
          cider-mode
          dired-omit-mode
          flymake-mode
          geiser-autodoc-mode
          geiser-mode
          isearch-mode
          iedit-mode
          js2-minor-mode
          multiple-cursors-mode
          orgtbl-mode
          overwrite-mode
          poly-markdown-mode
          sqlind-minor-mode
          subword-mode
          tree-sitter-mode
          visual-line-mode
          ztreediff-mode
          ztreedir-mode)))

;;; Paredit
(setup (:elpaca paredit :host github :repo "emacsmirror/paredit" :protocol ssh)
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  ielm-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook
                  scheme-mode-hook
                  sly-mrepl-mode-hook
                  geiser-repl-mode-hook))
    (add-hook hook #'enable-paredit-mode)))
;; Add extra functions via `paredit-ext'
(setup (:elpaca paredit-ext :host github :repo "drot/paredit-ext" :protocol ssh))
;; Configuration
(after-load 'paredit
  ;; Enable integration with ElDoc
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round)

  ;; Load extra functions
  (require 'paredit-ext)

  ;; Disable conflicting key binding
  (keymap-unset paredit-mode-map "M-s")
  ;; Set local key bindings
  (dolist (bind '(("M-s M-s" . paredit-splice-sexp)
                  ("M-{" . paredit-wrap-curly)
                  ("M-[" . paredit-wrap-square)
                  ("C-c C-M-s" . mark-containing-sexp)))
    (keymap-set paredit-mode-map (car bind) (cdr bind)))

  ;; Enable Paredit in the minibuffer
  (defvar site/paredit-minibuffer-setup-commands
    '(eval-expression
      pp-eval-expression
      eval-expression-with-eldoc
      ibuffer-do-eval
      ibuffer-do-view-and-eval)
    "Interactive commands for which Paredit should be enabled in the minibuffer.")

  (defun site/paredit-minibuffer-setup ()
    "Enable Paredit during lisp-related minibuffer commands."
    (if (memq this-command site/paredit-minibuffer-setup-commands)
        (paredit-mode +1)))

  (add-hook 'minibuffer-setup-hook #'site/paredit-minibuffer-setup)
  ;; Disable Electric Pair mode when Paredit is active
  (add-hook 'paredit-mode-hook
            (lambda () (setq-local electric-pair-mode nil))))

;;; Rainbow Delimiters
(setup rainbow-delimiters
  (:elpaca t)
  ;; Enable mode
  (:hook-into emacs-lisp-mode
              lisp-mode
              clojure-mode
              scheme-mode))

;;; YASnippet
(setup yasnippet
  (:elpaca t)
  ;; Enable mode
  (yas-global-mode +1))

;;; Artist mode
(keymap-global-set "C-c t a" 'artist-mode)

;;; Toggle debug on error
(keymap-global-set "C-c t d" 'toggle-debug-on-error)

;;; Ediff
(keymap-global-set "C-c f e" 'ediff)
(keymap-global-set "C-c f 3" 'ediff3)

;;; Hexl mode
(keymap-global-set "C-c t h" 'hexl-mode)
(keymap-global-set "C-c f h" 'hexl-find-file)

;;; Find sibling files
(setq find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")))
(keymap-global-set "C-c f s" 'find-sibling-file)

;;; Replace strings
(keymap-global-set "C-c s r" 'replace-string)
(keymap-global-set "C-c s C-r" 'replace-regexp)

;;; Grep results as a dired buffer
(keymap-global-set "C-c s d" 'find-grep-dired)

;;; Project
(dolist (bind '(("C-c p f" . project-find-file)
                ("C-c p r" . project-find-regexp)
                ("C-c p s" . project-search)))
  (keymap-global-set (car bind) (cdr bind)))

;;; Find function and variable definitions
(dolist (bind '(("C-c h f" . find-function)
                ("C-c h 4 f" . find-function-other-window)
                ("C-c h k" . find-function-on-key)
                ("C-c h v" . find-variable)
                ("C-c h 4 v" . find-variable-other-window)))
  (keymap-global-set (car bind) (cdr bind)))

;;; Find library
(keymap-global-set "C-c h l" 'find-library)
(keymap-global-set "C-c h 4 l" 'find-library-other-window)
(keymap-global-set "C-c h 4 L" 'find-library-other-frame)

;;; Menu bar
(keymap-global-set "M-`" 'tmm-menubar)

;;; Sort lines alphabetically
(keymap-global-set "C-c x l" 'sort-lines)

;;; Sort fields with regular expressions
(keymap-global-set "C-c x f" 'sort-regexp-fields)

;;; Word capitalization operations
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)

;;; Whitespace mode
(keymap-global-set "C-c x w" 'whitespace-cleanup)
(keymap-global-set "C-c t w" 'whitespace-mode)

;;; Auto Fill mode
(keymap-global-set "C-c t f" 'auto-fill-mode)

;;; Align
(dolist (bind '(("C-c x a" . align)
                ("C-c x c" . align-current)
                ("C-c x r" . align-regexp)))
  (keymap-global-set (car bind) (cdr bind)))

;;; Auto Insert
(keymap-global-set "C-c i a" 'auto-insert)

;;; Table insertion
(keymap-global-set "C-c i t" 'table-insert)

;;; Transpose regions
(keymap-global-set "C-c x t" 'transpose-regions)

;;; Matching lines operation
(keymap-global-set "C-c s l" 'delete-matching-lines)
(keymap-global-set "C-c s C-l" 'delete-non-matching-lines)

;;; Local variable insertion
(dolist (bind '(("C-c v d" . add-dir-local-variable)
                ("C-c v f" . add-file-local-variable)
                ("C-c v p" . add-file-local-variable-prop-line)))
  (keymap-global-set (car bind) (cdr bind)))

;;; Zap up to char
(defun site/zap-back-to-char (char)
  "Like `zap-up-to-char' but goes backwards."
  (interactive "c")
  (zap-up-to-char -1 char))
;; Set global key binding
(keymap-global-set "M-Z" 'site/zap-back-to-char)

;;; Customize interface
(keymap-global-set "<f11>" 'customize-group)
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

;;; Load changes from the customize interface
(after-load 'no-littering
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; Load unconditionally
  (load custom-file 'noerror))

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
