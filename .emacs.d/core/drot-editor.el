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

;; Enable CUA mode for rectangular selection
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Spell checking with Fly Spell
(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(defun activate-flyspell-mode ()
  "Turn flyspell-mode on."
  (flyspell-mode 1))

;; Whitespace mode configuration
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(defun activate-whitespace-mode ()
  "Turn whitespace-mode on."
  (whitespace-mode 1))

;; Text mode as default major mode
(setq default-major-mode 'text-mode)

;; Enable Fly Spell for text mode
(add-hook 'text-mode-hook 'activate-flyspell-mode)

;; Enable whitespace-mode for text mode
(add-hook 'text-mode-hook 'activate-whitespace-mode)

(provide 'drot-editor)
;; drot-editor.el ends here
