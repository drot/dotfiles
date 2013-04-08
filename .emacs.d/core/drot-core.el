;;
;; drot-core.el - Core Emacs configuration
;;

;; Recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Ignore case on completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Message buffer size
(setq message-log-max 100)

;; Store all backup and auto-save files in the tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save minibuffer history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring extended-command-history)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "saved.hist" drot-saves-dir))
(savehist-mode t)

;; Enable X clipboard usage
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Easier switching between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Enable Winner mode
(winner-mode 1)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Eshell directory
(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" drot-saves-dir))

;; Reuse dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Calendar
(setq mark-holidays-in-calendar t
      all-christian-calendar-holidays t
      all-islamic-calendar-holidays nil
      all-hebrew-calendar-holidays nil
      european-calendar-style t
      display-time-24hr-format t
      calendar-latitude 43.20
      calendar-longitude 17.48
      calendar-location-name "Mostar, Bosnia and Herzegovina")

(provide 'drot-core)
;; drot-core.el ends here
