;;; init.el --- drot Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2021 drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/master/emacs/.config/emacs
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
(straight-use-package 'no-littering)
;; Initialize mode
(require 'no-littering)

;;; Disable needless GUI elements
(dolist (mode '(tool-bar-mode
                menu-bar-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Color theme
(straight-use-package 'modus-themes)
;; Configuration
(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle
      modus-themes-mode-line '3d
      modus-themes-syntax 'alt-syntax
      modus-themes-paren-match 'intense-bold
      modus-themes-links 'faint
      modus-themes-prompts 'subtle
      modus-themes-completions 'opinionated
      modus-themes-diffs 'deuteranopia
      modus-themes-org-blocks 'rainbow
      modus-themes-scale-headings t)
;; Load themes
(modus-themes-load-themes)
;; Enable dark theme
(modus-themes-load-vivendi)

;;; Don't show the startup welcome messages
(setq inhibit-startup-screen t)
(fset 'display-startup-echo-area-message #'ignore)

;;; Disable scratch buffer info text
(setq initial-scratch-message nil)

;;; Show column number and buffer size on the mode line
(column-number-mode +1)
(size-indication-mode +1)

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
;; Enable mode
(window-divider-mode +1)

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

;;; Don't show help for completions
(setq completion-show-help nil)

;;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;;; Indicate minibuffer recursion depth
(minibuffer-depth-indicate-mode +1)

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
(setq savehist-autosave-interval 60
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
        "/elpa/.*\\'"
        "/image-dired/.*\\'"
        "/newsticker/.*\\'"
        "/straight/.*\\'"
        "/url/.*\\'"
        "/dev/shm/.*\\'"
        ".*\\.gz\\'"
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
                nov-mode-hook
                rcirc-mode-hook
                term-mode-hook
                vterm-mode-hook
                undo-tree-visualizer-mode-hook
                cider-repl-mode-hook))
  (add-hook hook
            (lambda () (setq-local global-hl-line-mode nil))))
;; Configuration
(after-load 'hl-line
  ;; Don't display line highlight in inactive windows
  (setq hl-line-sticky-flag nil))

;;; Highlight matching parentheses
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
;; Enable mode
(show-paren-mode +1)

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
  ;; Use more conservative pairing
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  ;; Watch out for context
  (setq electric-quote-context-sensitive t))

;;; Prettify-Symbols mode
(setq prettify-symbols-unprettify-at-point t)
;; Enable mode
(global-prettify-symbols-mode +1)

;;; Which function mode
(setq which-func-unknown "n/a")
;; Enable mode
(which-function-mode +1)

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
(winner-mode +1)

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
(global-set-key (kbd "C-c c b") #'flyspell-buffer)
(global-set-key (kbd "C-c c r") #'flyspell-region)
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
(global-set-key (kbd "C-c c d") #'ispell-change-dictionary)
;; Configuration
(after-load 'ispell
  ;; Ensure spell checking program is available
  (setq ispell-program-name (executable-find "aspell"))
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
;; Allow search string extension with motion commands
(setq isearch-yank-on-move 'shift)
;; Add local key binding for `isearch-occur'
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

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
                      (name . "*EGLOT*")
                      (name . "*sly-inferior-lisp for sbcl*")
                      (name . "*sly-events for sbcl*")
                      (name . "*inferior-lisp*")
                      (name . "*Warnings*")))
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
                       (name . "*Python*")))
           ("Shell" (or (derived-mode . eshell-mode)
                        (derived-mode . shell-mode)))
           ("Terminal" (or (derived-mode . term-mode)
                           (derived-mode . vterm-mode)))
           ("Text" (or (mode . text-mode)
                       (derived-mode . reb-mode)))
           ("TRAMP" (name . "*tramp"))
           ("Web" (or (derived-mode . eww-mode)
                      (derived-mode . elpher-mode))))))
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
  ;; Ignore case sensitivity
  (setq pcomplete-ignore-case t))

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
  (setq scheme-program-name "guile"))

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

;;; Enable Pass integration
(straight-use-package 'auth-source-pass)
;; Configuration
(after-load 'auth-source
  ;; Enable mode
  (auth-source-pass-enable)
  ;; Don't ask to save authentication info
  (setq auth-source-save-behavior nil))

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
(global-set-key (kbd "C-c i c") #'copyright)
(global-set-key (kbd "C-c i C-c") #'copyright-update)
;; Configuration
(after-load 'copyright
  ;; Change default format
  (setq copyright-year-ranges t
        copyright-names-regexp (regexp-quote user-login-name)))

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
(global-set-key (kbd "C-c w w") #'eww)
(global-set-key (kbd "C-c w C-b") #'eww-list-bookmarks)

;;; Browse URL
(global-set-key (kbd "C-c w b") #'browse-url)
;; Configuration
(after-load 'browse-url
  ;;  Open URLs with the specified browser
  (setq browse-url-browser-function #'browse-url-xdg-open))

;;; SHR
(after-load 'shr
  ;; Use specified browser instead of searching for it
  (setq shr-external-browser browse-url-browser-function))

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
  (defun drot/eshell-mode-setup ()
    "Custom hook to run for my Eshell buffers."
    ;; Disable Company since we use `completion-at-point'
    (company-mode -1))
  ;; Apply the custom hook
  (add-hook 'eshell-mode-hook #'drot/eshell-mode-setup)
  ;; Add Outline support for Eshell prompts
  (add-hook 'eshell-mode-hook
            (lambda () (setq outline-regexp eshell-prompt-regexp))))

;; Eshell smart display
(after-load 'eshell
  ;; Load library
  (require 'em-smart)
  ;; Initialize mode
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize))

;;; Shell mode
(after-load 'shell
  ;; Custom hook to avoid conflicts
  (defun drot/shell-mode-setup ()
    "Custom hook to run for my Shell buffers."
    ;; Enable clickable file paths and disable Company
    (compilation-shell-minor-mode +1)
    (company-mode -1))
  ;; Apply the custom hook
  (add-hook 'shell-mode-hook #'drot/shell-mode-setup))

;;; IELM
(global-set-key (kbd "C-c r i") #'ielm)
;; Configuration
(after-load 'ielm
  ;; Change default prompt
  (setq ielm-prompt "(>) "))

;;; Transient
(straight-use-package 'transient)
;; Initialize library
(require 'transient)

;;; Flymake
(global-set-key (kbd "C-c ! t") #'flymake-mode)
;; Configuration
(after-load 'flymake
  ;; Define Transient command
  (transient-define-prefix drot/flymake-transient ()
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
                  ("C-c ! h" . drot/flymake-transient)))
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
    "Apply ANSI color codes in compilation buffers."
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  ;; Apply colorization
  (add-hook 'compilation-filter-hook #'drot/ansi-color-compilation-buffer)
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
  (setq smiley-style 'medium))

;;; MIME decoding configuration
(after-load 'mm-decode
  ;; Fit images to buffer
  (setq mm-inline-large-images 'resize))

;;; Gnus
(global-set-key (kbd "<f9>") #'gnus)
;; Configuration
(after-load 'gnus
  ;; Set local key bindings
  (define-key gnus-summary-mode-map (kbd "C-c M-o") #'ace-link-gnus)
  (define-key gnus-article-mode-map (kbd "C-c M-o") #'ace-link-gnus)
  ;; Main Gnus directory
  (setq gnus-directory "~/.news/")
  ;; Configure mail server
  (setq gnus-select-method
        '(nnmaildir "firemail"
                    (directory "~/.mail/")))
  ;; Configure news server
  (add-to-list 'gnus-secondary-select-methods
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
(global-set-key (kbd "<C-f9>") #'newsticker-treeview)
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
  ;; Define Transient command
  (transient-define-prefix drot/outline-transient ()
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
  (define-key outline-minor-mode-map (kbd "C-c o h") #'drot/outline-transient))

;;; Org-mode
(defun drot/toggle-table-mode ()
  "Initialize Org Table mode."
  (interactive)
  ;; Rather hacky mode, needs to be manually loaded
  (require 'org-table)
  (orgtbl-mode +1))
;; Set global key bindings
(dolist (bind '(("C-c o a" . org-agenda)
                ("C-c o c" . org-capture)
                ("C-c o t" . org-todo-list)
                ("C-c o s" . org-search-view)
                ("C-c o l" . org-store-link)
                ("C-c t t" . drot/toggle-table-mode)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Configuration
(after-load 'org
  ;; Set local key binding
  (define-key org-mode-map (kbd "C-c M-o") #'ace-link-org)
  ;; Default directory and file location
  (setq org-directory "~/Documents/org"
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files '("~/Documents/org"))
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

;;; Time display
(global-set-key (kbd "<C-f12>") #'display-time-world)
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
  (setq display-time-world-list
        '(("Europe/Riga" "Riga")
          ("America/Los_Angeles" "Los Angeles")
          ("Canada/Eastern" "Toronto")
          ("Asia/Saigon" "Saigon")
          ("UTC" "Universal"))))

;;; 0x0 paste support
(straight-use-package '0x0)
;; Set global key bindings
(dolist (bind '(("C-c b y" . 0x0-upload)
                ("C-c f u" . 0x0-upload-file)
                ("C-c x y" . 0x0-upload-string)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Ace-window
(straight-use-package 'ace-window)
;; Set global key binding
(global-set-key (kbd "M-o") #'ace-window)
;; Configuration
(after-load 'ace-window
  ;; Use keys on the home row
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; AUCTeX
(straight-use-package 'auctex)
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
(straight-use-package 'cider)
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
(straight-use-package 'clojure-mode)
;; Configuration
(after-load 'clojure-mode
  ;; Enable CIDER mode
  (add-hook 'clojure-mode-hook #'cider-mode)
  ;; Enable SubWord mode
  (add-hook 'clojure-mode-hook #'subword-mode))

;;; Dash
(straight-use-package 'dash)
;; Configuration
(after-load 'dash
  ;; Enable syntax coloring for Dash functions
  (dash-enable-font-lock))

;;; Debbugs browser
(straight-use-package 'debbugs)
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
(straight-use-package 'dired-filter)
;; Configuration
(after-load 'dired-x
  ;; Enable mode
  (require 'dired-filter)
  ;; Set local key binding
  (define-key dired-mode-map (kbd "\\") dired-filter-mark-map))

;;; Dired Rainbow
(straight-use-package 'dired-rainbow)
;; Configuration
(after-load 'dired-filter
  ;; Enable mode
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
                                           "tex" "org" "txt" "xlsx"))
  (dired-rainbow-define encrypted "salmon" ("gpg" "pgp" "rsa"))
  (dired-rainbow-define executable (:foreground "gold" :italic t) ("exe" "msi"))
  (dired-rainbow-define html "wheat" ("htm" "html" "xhtml"))
  (dired-rainbow-define image "goldenrod" ("jpg" "png" "jpeg" "gif"))
  (dired-rainbow-define log "dark slate gray" ("log"))
  (dired-rainbow-define config (:foreground "cadet blue" :italic t) ("conf" "ini" "yml"))
  (dired-rainbow-define packaged "khaki" ("deb" "rpm"))
  (dired-rainbow-define sourcefile "orchid" ("py" "c" "cc" "h" "java"
                                             "pl" "rb" "R" "php" "el"
                                             "scm" "cpp" "fos" "lisp" "clj"
                                             "lua" "lisp" "sh"))
  (dired-rainbow-define patch (:background "red4") ("diff" "patch"))
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

;;; Extra miscellaneous colorization
(straight-use-package 'diredfl)
;; Configuration
(after-load 'dired-rainbow
  ;; Enable mode
  (diredfl-global-mode +1))

;;; Dired Subtree
(straight-use-package 'dired-subtree)
;; Configuration
(after-load 'dired-rainbow
  ;; Enable mode
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
(straight-use-package 'dired-ranger)
;; Configuration
(after-load 'dired-subtree
  ;; Enable mode
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
(straight-use-package 'dired-narrow)
;; Configuration
(after-load 'dired-ranger
  ;; Enable mode
  (require 'dired-narrow)
  ;; Set local key binding
  (define-key dired-mode-map (kbd "C-.") #'dired-narrow)
  ;; Exit on single match
  (setq dired-narrow-exit-when-one-left t))

;;; Dired Collapse
(straight-use-package 'dired-collapse)
;; Configuration
(after-load 'dired-narrow
  ;; Enable mode
  (require 'dired-collapse)
  ;; Set local key binding
  (define-key dired-mode-map (kbd ",") #'dired-collapse-mode))

;;; Dired-du
(straight-use-package 'dired-du)
;; Configuration
(after-load 'dired-collapse
  ;; Enable mode
  (require 'dired-du)
  ;; Use human readable output by default
  (setq dired-du-size-format t))

;;; Dired Async
(straight-use-package 'async)
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
    (define-key dired-mode-map (kbd (car bind)) (cdr bind))))

;;; Dired rsync
(straight-use-package 'dired-rsync)
;; Configuration
(after-load 'dired-async
  (define-key dired-mode-map (kbd "C-c C-r") #'dired-rsync))

;;; Dockerfile mode
(straight-use-package 'dockerfile-mode)

;;; Eglot
(straight-use-package 'eglot)
;; Set global key binding
(global-set-key (kbd "C-c e t") #'eglot)
;; Configuration
(after-load 'eglot
  ;; Set local key bindings
  (dolist (bind '(("C-c e c" . eglot-reconnect)
                  ("C-c e s" . eglot-shutdown)
                  ("C-c e r" . eglot-rename)
                  ("C-c e f" . eglot-format)
                  ("C-c e h" . eldoc)
                  ("C-c e a" . eglot-code-actions)
                  ("C-c e b" . eglot-events-buffer)
                  ("C-c e e" . eglot-stderr-buffer)))
    (define-key eglot-mode-map (kbd (car bind)) (cdr bind)))
  ;; Add the Lua language server
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-lsp"))))

;;; Elpher Gopher browser
(straight-use-package 'elpher)
;; Set key binding
(global-set-key (kbd "C-c w e") #'elpher)
;; Configuration
(after-load 'elpher
  ;; Colorize escape sequences by default
  (setq elpher-filter-ansi-from-text t))

;;; rcirc
(global-set-key (kbd "<f8>") #'irc)
;; Configuration
(after-load 'rcirc
  ;; User defaults
  (setq rcirc-default-user-name user-login-name
        rcirc-reconnect-delay 10)
  ;; Connect to the specified servers and channels
  (setq rcirc-server-alist
        '(("irc.rizon.net"
           :port 6697
           :encryption tls
           :channels ("#/g/technology" "#rice"))))
  ;; Authentication
  (setq rcirc-authinfo
        `(("rizon" nickserv "drot"
           ,(auth-source-pass-get 'secret "IRC/drot@irc.rizon.net"))))
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
  ;; Disable `company-mode' in rcirc buffers
  (add-hook 'rcirc-mode-hook
            (lambda () (company-mode -1)))
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
(straight-use-package 'rcirc-styles)
;; Configuration
(after-load 'rcirc
  ;; Enable mode
  (require 'rcirc-styles)
  ;; Set local key bindings
  (dolist (bind '(("C-c C-e p" . rcirc-styles-toggle-preview)
                  ("C-c C-e a" . rcirc-styles-insert-attribute)
                  ("C-c C-e c" . rcirc-styles-insert-color)))
    (define-key rcirc-mode-map (kbd (car bind)) (cdr bind)))
  ;; Use custom colors
  (setq rcirc-styles-color-vector
        ["black"
         "#a60000"
         "#005e00"
         "#813e00"
         "#0031a9"
         "#721045"
         "#00538b"
         "gray65"
         "gray35"
         "#972500"
         "#315b00"
         "#70480f"
         "#2544bb"
         "#8f0075"
         "#30517f"
         "white"]))

;; rcirc colored nicknames
(straight-use-package 'rcirc-color)
;; Configuration
(after-load 'rcirc
  ;; Enable mode
  (require 'rcirc-color)
  ;; Inherit nick colors from rcirc-styles colors
  (setq rcirc-colors (append rcirc-styles-color-vector nil)))

;; rcirc notifications
(straight-use-package 'rcirc-notify)
;; Configuration
(after-load 'rcirc
  ;; Enable mode
  (rcirc-notify-add-hooks))

;;; Expand region
(straight-use-package 'expand-region)
;; Autoload missing functions
(autoload #'er/mark-defun "er-basic-expansions"
  "Mark defun around or in front of point." t)
(autoload #'er/mark-text-paragraph "text-mode-expansions"
  "Marks one paragraph." t)
;; Define Transient for text marking operations.
(transient-define-prefix drot/mark-text-transient ()
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
(global-set-key (kbd "C-c x C-SPC") #'drot/mark-text-transient)
(global-set-key (kbd "C-=") #'er/expand-region)

;;; Flymake ShellCheck support
(straight-use-package 'flymake-shellcheck)
;; Enable mode
(when (executable-find "shellcheck")
  (add-hook 'sh-mode-hook #'flymake-shellcheck-load))

;;; Geiser
(straight-use-package 'geiser)

;;; Go mode
(straight-use-package 'go-mode)

;;; htmlize
(straight-use-package 'htmlize)

;;; Iedit
(straight-use-package 'iedit)
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
(straight-use-package 'js2-mode)
;; Enable mode
(add-hook 'js-mode-hook #'js2-minor-mode)
;; Configuration
(after-load 'js2-mode
  ;; Syntax defaults
  (setq js2-basic-offset 2
        js2-highlight-level 3)
  ;; Highlight unused variables
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

;;; JSON mode
(straight-use-package
 '(json-mode :type git :host github :repo "emacs-straight/json-mode"))

;;; Lua mode
(straight-use-package 'lua-mode)

;;; Enhanced Ruby mode
(straight-use-package 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; Add interpreter support
(straight-use-package 'inf-ruby)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;;; Rubocop
(straight-use-package 'rubocop)
;; Change default prefix
(setq rubocop-keymap-prefix (kbd "C-c C-c"))
;; Enable mode
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)

;;; EPUB format support
(straight-use-package 'nov)
;; Initialize package
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; Configuration
(after-load 'nov
  ;; Change default font
  (defun drot/nov-font-setup ()
    "Apply custom variable pitch font for `nov.el'."
    (face-remap-add-relative 'variable-pitch :family "Noto Serif" :height 1.2))
  ;; Apply the custom hook
  (add-hook 'nov-mode-hook #'drot/nov-font-setup)
  ;; Text filling
  (setq nov-text-width 80))

;;; Macrostep
(straight-use-package 'macrostep)
;; Set local key binding
(define-key emacs-lisp-mode-map (kbd "C-c M-e") #'macrostep-expand)

;;; Magit
(straight-use-package 'magit)
;; Set global key bindings
(dolist (bind '(("C-x g" . magit-status)
                ("C-x M-g" . magit-dispatch-popup)
                ("C-c g c" . magit-clone)
                ("C-c g b" . magit-blame)
                ("C-c g l" . magit-log-buffer-file)
                ("C-c g p" . magit-pull)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Configuration
(after-load 'magit
  ;; Use `selectrum' for candidate sorting
  (setq magit-completing-read-function #'selectrum-completing-read))

;;; Markdown mode
(straight-use-package 'markdown-mode)
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
(straight-use-package 'move-text)
;; Define Transient command
(transient-define-prefix drot/move-text-transient ()
  "Transient for Move-text commands."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Move Text"
   ("p" "Move Up" move-text-up)
   ("n" "Move Down" move-text-down)])
;; Set global key binding
(global-set-key (kbd "C-c x m") #'drot/move-text-transient)

;;; Multiple cursors
(straight-use-package 'multiple-cursors)
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
          mouse-drag-mode-line)))
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
;; Define Transient command
(transient-define-prefix drot/multiple-cursors-transient ()
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
(global-set-key (kbd "C-c m h") #'drot/multiple-cursors-transient)

;;; PDF Tools
(straight-use-package 'pdf-tools)
;; Enable mode
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
    (setq pdf-misc-print-program "/usr/bin/lp")))

;; PDF Tools Midnight mode colors
(after-load 'pdf-view
  ;; Tomorrow Night palette
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#1d1f21")))

;; Display PDF files to the right always
(add-to-list 'display-buffer-alist
             '("\\.pdf\\(<[^>]+>\\)?$"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right)
               (window-width . 0.5)
               (reusable-frames . nil)))

;;; PKGBUILD mode
(straight-use-package 'pkgbuild-mode)

;;; Polymode Markdown
(straight-use-package 'poly-markdown)
;; Enable mode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; Rainbow mode
(straight-use-package 'rainbow-mode)
;; Set global key binding
(global-set-key (kbd "C-c t r") #'rainbow-mode)

;;; Salt mode
(straight-use-package 'salt-mode)

;;; Skewer
(straight-use-package 'skewer-mode)
;; Enable mode
(add-hook 'js2-mode-hook #'skewer-mode)
;; Set global key binding
(global-set-key (kbd "C-c r w") #'skewer-repl)

;; Skewer CSS
(add-hook 'css-mode-hook #'skewer-css-mode)

;; Skewer HTML
(add-hook 'mhtml-mode-hook #'skewer-html-mode)

;;; SLY
(straight-use-package 'sly)
;; Set global key bindings
(global-set-key (kbd "C-c r s") #'sly)
(global-set-key (kbd "C-c r c") #'sly-connect)
;; Configuration
(after-load 'sly
  ;; Use SBCL by default
  (setq inferior-lisp-program (executable-find "sbcl"))
  ;; Disable conflicting key bindings
  (defun drot/sly-sanitize-bindings ()
    "Removes SLY's conflicting keybindings."
    (cond ((boundp 'sly-mode-map)
           (define-key sly-mode-map (kbd "C-c i") nil)
           (define-key sly-mode-map (kbd "C-c x") nil))
          ('t (message "SLY keybindings not sanitized!"))))
  ;; Apply the custom hook
  (add-hook 'sly-mode-hook #'drot/sly-sanitize-bindings)
  ;; Set local key bindings
  (define-key sly-mode-map (kbd "C-c M-s i") #'sly-import-symbol-at-point)
  (define-key sly-mode-map (kbd "C-c M-s x") #'sly-export-symbol-at-point))

;;; SLY macrostep
(straight-use-package 'sly-macrostep)

;;; SQL indentation
(straight-use-package 'sql-indent)

;;; Systemd mode
(straight-use-package
 '(systemd :type git :host github :repo "drot/systemd-mode"))

;;; VTerm
(straight-use-package 'vterm)
;; Set global key binding
(global-set-key (kbd "<f7>") #'vterm)
;; Configuration
(after-load 'vterm
  ;; Set buffer name
  (setq vterm-buffer-name-string "vterm - %s"))

;;; Wgrep
(straight-use-package 'wgrep)

;;; YAML mode
(straight-use-package 'yaml-mode)
;; Enable SubWord mode
(add-hook 'yaml-mode-hook #'subword-mode)

;;; Ace-link
(straight-use-package 'ace-link)
;; Enable mode
(ace-link-setup-default)
;; Set global key binding
(global-set-key (kbd "C-c w a") #'ace-link-addr)

;;; Avy
(straight-use-package 'avy)
;; Enable mode
(avy-setup-default)
;; Define Transient command
(transient-define-prefix drot/avy-transient ()
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
                ("C-M-'" . drot/avy-transient)))
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
(straight-use-package 'company)
;; Enable mode
(global-company-mode +1)
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
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

;;; Diff-Hl
(straight-use-package 'diff-hl)
;; Enable mode
(global-diff-hl-mode +1)
;; Update diffs immediately
(diff-hl-flydiff-mode +1)
;; Set global key binding
(global-set-key (kbd "C-c t v") #'diff-hl-margin-mode)
;; Add hooks for `dired' and `magit'
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;; Add hook for terminal
(add-hook 'diff-hl-mode-on-hook
          (lambda ()
            (unless (window-system)
              (diff-hl-margin-local-mode))))

;;; Form-feed
(straight-use-package 'form-feed)
;; Same line width as `fill-column' width
(setq form-feed-line-width fill-column)
;; Enable mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                scheme-mode-hook
                compilation-mode-hook
                outline-mode-hook
                help-mode-hook))
  (add-hook hook #'form-feed-mode))
;; Configuration
(after-load 'form-feed
  ;; Make `form-feed-line' color equal to comment color
  (set-face-attribute 'form-feed-line nil
                      :strike-through t
                      :inherit font-lock-comment-face))

;;; Hl-Todo
(straight-use-package 'hl-todo)
;; Enable mode
(global-hl-todo-mode +1)
;; Configuration
(after-load 'hl-todo
  ;; Define Transient command
  (transient-define-prefix drot/hl-todo-transient ()
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    ["Highlight TODO"
     ("n" "Next" hl-todo-next)
     ("p" "Previous" hl-todo-previous)])
  ;; Set local key bindings
  (dolist (bind '(("M-s t" . hl-todo-occur)
                  ("C-c p t" . drot/hl-todo-transient)
                  ("C-c p i" . hl-todo-insert-keyword)))
    (define-key hl-todo-mode-map (kbd (car bind)) (cdr bind))))

;;; Selectrum minibuffer completion
(straight-use-package 'selectrum)
;; Initialize mode
(selectrum-mode +1)
;; Set key binding to repeat last command
(global-set-key (kbd "C-x C-z") #'selectrum-repeat)
;; Configuration
(after-load 'selectrum
  ;; Show line count
  (setq selectrum-show-indices t)
  ;; Show total and current matches
  (setq selectrum-count-style 'current/matches))

;;; Prescient
(straight-use-package 'prescient)
;; Configuration
(after-load 'prescient
  ;; Aggressively save history
  (setq prescient-aggressive-file-save t)
  ;; Use fuzzy matching by default
  (setq prescient-filter-method 'fuzzy)
  ;; Enable persistent history
  (prescient-persist-mode +1))

;;; Selectrum Prescient
(straight-use-package 'selectrum-prescient)
;; Disable filtering
(setq selectrum-prescient-enable-filtering nil)
;; Initialize mode
(selectrum-prescient-mode +1)

;;; Orderless
(straight-use-package 'orderless)
;; Use single completion style explicitly
(setq completion-styles '(orderless))
;; Skip matching part highlight
(setq orderless-skip-highlighting (lambda () selectrum-is-active))
;; Enable `orderless' candidate highlight
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

;;; Company Prescient
(straight-use-package 'company-prescient)
;; Initialize mode
(company-prescient-mode +1)

;;; Consult
(straight-use-package 'consult)
;; Set key bindings
(dolist (bind '(;; C-c bindings (mode-specific-map)
                ("C-c h c" . consult-history)
                ("C-c h m" . consult-mode-command)
                ("C-c f b" . consult-bookmark)
                ("C-c k" . consult-kmacro)
                ;; C-x bindings (ctl-x-map)
                ("C-x M-:" . consult-complex-command) ;; orig. repeat-complet-command
                ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
                ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
                ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
                ;; Custom M-# bindings for fast register access
                ("M-#" . consult-register-load)
                ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
                ("C-M-#" . consult-register)
                ;; Other custom bindings
                ("M-y" . consult-yank-pop) ;; orig. yank-pop
                ("<help> a" . consult-apropos) ;; orig. apropos-command
                ;; M-g bindings (goto-map)
                ("M-g e" . consult-compile-error)
                ("M-g g" . consult-goto-line) ;; orig. goto-line
                ("M-g M-g" . consult-goto-line) ;; orig. goto-line
                ("M-g o" . consult-outline)
                ("M-g m" . consult-mark)
                ("M-g k" . consult-global-mark)
                ("M-g i" . consult-imenu)
                ("M-g I" . consult-project-imenu)
                ("M-g e" . consult-error)
                ;; M-s bindings (search-map)
                ("M-s f" . consult-find)
                ("M-s L" . consult-locate)
                ("M-s g" . consult-grep)
                ("M-s G" . consult-git-grep)
                ("M-s r" . consult-ripgrep)
                ("M-s l" . consult-line)
                ("M-s m" . consult-multi-occur)
                ("M-s k" . consult-keep-lines)
                ("M-s u" . consult-focus-lines)))
  (global-set-key (kbd (car bind)) (cdr bind)))
;; Isearch integration
(global-set-key (kbd "M-s e") #'consult-isearch)
;; Set local key bindings
(dolist (bind '(("M-e" . consult-isearch) ;; orig. isearch-edit-string
                ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
                ("M-s l" . consult-line))) ;; required by consult-line to detect isearch
  (define-key isearch-mode-map (kbd (car bind)) (cdr bind)))
;; Configuration
(after-load 'consult
  ;; Set narrowing key binding
  (setq consult-narrow-key (kbd "<"))
  ;; Make narrowing help available in the minibuffer
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; Don't preview buffers eagerly
  (setq consult-config `((consult-buffer :preview-key ,(kbd "C-."))))
  ;; Blink after jumping
  (setq consult-after-jump-hook '(xref-pulse-momentarily))
  ;; Integrate with `register'
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  ;; Tweak the register preview window
  (advice-add #'register-preview :override #'consult-register-window))

;;; Marginalia in the minibuffer
(straight-use-package 'marginalia)
;; Set key binidng
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
;; Enable Mode
(marginalia-mode +1)
;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations
(advice-add #'marginalia-cycle :after
            (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected))))
;; Add `tab-bar-mode' support
(add-to-list 'marginalia-prompt-categories '("tab by name" . tab))

;;; Embark
(straight-use-package 'embark)
;; Define key bindings
(global-set-key (kbd "C-S-a") #'embark-act)
;; Configuration
(after-load 'embark
  ;; Bind `marginalia-cycle' as an Embark action
  (define-key embark-general-map (kbd "A") #'marginalia-cycle)
  ;; Selectrum integration
  (defun drot/pause-selectrum ()
    "Pause Selectrum while using `embark-collect-live'."
    (when (eq embark-collect--kind :live)
      (with-selected-window (active-minibuffer-window)
        (shrink-window selectrum-num-candidates-displayed)
        (setq-local selectrum-num-candidates-displayed 0))))
  ;; Apply the custom hook
  (add-hook 'embark-collect-mode-hook #'drot/pause-selectrum)
  ;; Refresh candidate list after action
  (defun drot/refresh-selectrum ()
    "Refresh candidate list action with `selectrum'."
    (setq selectrum--previous-input-string nil))
  ;; Apply the custom hook
  (add-hook 'embark-pre-action-hook #'drot/refresh-selectrum)
  ;; Same after `embark-collect'
  (add-hook 'embark-post-action-hook #'embark-collect--update-linked))

;;; Embark Consult integration
(straight-use-package 'embark-consult)
;; Configuration
(after-load 'embark
  ;; Load library
  (require 'embark-consult)
  ;; Automatically preview entry at point in Embark Collect buffers
  (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode))

;;; Embark avy integration
(straight-use-package 'avy-embark-collect)
;; Configuration
(after-load 'embark
  ;; Load library
  (require 'avy-embark-collect)
  ;; Set local key bindings
  (dolist (bind '(("C-'" . avy-embark-collect-choose)
                  ("C-\"" . avy-embark-collect-act)))
    (define-key embark-collect-mode-map (kbd (car bind)) (cdr bind))))

;;; Minions
(straight-use-package 'minions)
;; Enable mode
(minions-mode +1)
;; Configuration
(after-load 'minions
  ;; Change mode lighter and color
  (setq minions-mode-line-lighter "M+"
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
          js2-minor-mode
          multiple-cursors-mode
          orgtbl-mode
          overwrite-mode
          poly-markdown-mode
          sqlind-minor-mode
          subword-mode
          rcirc-omit-mode
          visual-line-mode)))

;;; Paredit
(straight-use-package 'paredit)
;; Enable mode
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
  ;; Enable integration with ElDoc
  (eldoc-add-command
   #'paredit-backward-delete
   #'paredit-close-round)

  ;; Extra functions for ParEdit via `paredit-ext'
  (straight-use-package
   '(paredit-ext :type git
                 :files ("lisp/paredit-ext.el")
                 :repo "https://github.com/jwiegley/dot-emacs.git"
                 :nonrecursive t
                 :depth 1))

  ;; Load library
  (require 'paredit-ext)

  ;; Disable conflicting key binding
  (define-key paredit-mode-map (kbd "M-s") nil)
  ;; Set local key bindings
  (dolist (bind '(("M-s M-s" . paredit-splice-sexp)
                  ("M-{" . paredit-wrap-curly)
                  ("M-[" . paredit-wrap-square)
                  ("C-c C-M-s" . mark-containing-sexp)))
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
        (paredit-mode +1)))

  (add-hook 'minibuffer-setup-hook #'drot/paredit-minibuffer-setup)
  ;; Disable Electric Pair mode when Paredit is active
  (add-hook 'paredit-mode-hook
            (lambda () (setq-local electric-pair-mode nil))))

;;; Rainbow Delimiters
(straight-use-package 'rainbow-delimiters)
;; Enable mode
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                scheme-mode-hook))
  (add-hook hook #'rainbow-delimiters-mode))

;;; Undo Tree
(straight-use-package
 '(undo-tree :type git :host gitlab :repo "tsc25/undo-tree"))
;; Enable mode
(global-undo-tree-mode +1)
;; Configuration
(after-load 'undo-tree
  ;; Enable in region
  (setq undo-tree-enable-undo-in-region t))
;; Exclude `magit' status buffers
(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)

;;; Volatile Highlights
(straight-use-package 'volatile-highlights)
;; Enable mode
(volatile-highlights-mode +1)

;;; YASnippet
(straight-use-package 'yasnippet)
;; Enable mode
(yas-global-mode +1)

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
                ("C-c p s" . project-search)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Find function and variable definitions
(dolist (bind '(("C-c h f" . find-function)
                ("C-c h 4 f" . find-function-other-window)
                ("C-c h k" . find-function-on-key)
                ("C-c h v" . find-variable)
                ("C-c h 4 v" . find-variable-other-window)))
  (global-set-key (kbd (car bind)) (cdr bind)))

;;; Find library
(global-set-key (kbd "C-c h l") #'find-library)
(global-set-key (kbd "C-c h 4 l") #'find-library-other-window)
(global-set-key (kbd "C-c h 4 L") #'find-library-other-frame)

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

;;; Whitespace mode
(global-set-key (kbd "C-c x w") #'whitespace-cleanup)
(global-set-key (kbd "C-c t w") #'whitespace-mode)

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

;;; Zap up to char
(defun drot/zap-back-to-char (char)
  "Like `zap-up-to-char' but goes backwards."
  (interactive "c")
  (zap-up-to-char -1 char))
;; Set global key binding
(global-set-key (kbd "M-Z") #'drot/zap-back-to-char)

;;; Customize interface
(global-set-key (kbd "<f11>") #'customize-group)
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
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
