;;
;; my-ui.el - Emacs UI configuration
;;

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Turn off the scrollbar
(scroll-bar-mode -1)

;; Don't show the welcome message
(setq inhibit-startup-screen t
      initial-scratch-message nil
      gnus-inhibit-startup-message t)

;; Disable cursor blink
(blink-cursor-mode 0)

;; Show tooltips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Nicer scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Answer y or n instead of yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Show column number in modeline
(column-number-mode t)

;; Show buffer size in modeline
(size-indication-mode t)

;; Color theme
(load-theme 'anti-zenburn t)

(provide 'my-ui)
;; my-ui.el ends here
