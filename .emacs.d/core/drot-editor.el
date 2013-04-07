;;
;; drot-editor.el - Emacs editing configuration
;;

;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" drot-saves-dir)
      bookmark-save-flag 1)

;; Enable Easy PG
(require 'epa-file)

;; Spell checking with Fly Spell
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(defun drot-enable-flyspell ()
  "Turn flyspell-mode on."
  (flyspell-mode 1))

;; Enable Fly Spell for text mode
(add-hook 'text-mode-hook 'drot-enable-flyspell)

;; Default major mode
(setq default-major-mode 'text-mode)

(provide 'drot-editor)
;; drot-editor.el ends here
