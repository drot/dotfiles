;; spelling
(erc-spelling-mode 1)

;; nick colors
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)

;; colors
(custom-set-faces
'(erc-notice-face ((t (:foreground "#61635e" :weight normal))))
'(erc-error-face ((t (:foreground "#cc0000"))))
'(erc-input-face ((t (:foreground "#4e9a06"))))
'(erc-button ((t (:background "#000" :foreground "#729fcf" :underline "#729fcf" :weight normal))))
'(erc-my-nick-face ((t (:foreground "#4e9a06" :weight bold))))
'(erc-prompt-face ((t (:background "#000" :foreground "#4e9a06" :weight normal))))
'(erc-timestamp-face ((t (:foreground "#729fcf" :weight normal)))))

;; look
(setq erc-nick-uniquifier "_"
      erc-notice-prefix "* "
      erc-prompt ">"
      erc-match-mode t
      erc-button-buttonize-nicks nil
      erc-kill-buffer-on-part t
      erc-server-auto-reconnect nil
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%H:%M "
      erc-timestamp-only-if-changed-flag nil
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477")
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; dynamic fill width
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
	  '(lambda ()
	     (save-excursion
	       (walk-windows
		(lambda (w)
		  (let ((buffer (window-buffer w)))
		    (set-buffer buffer)
		    (when (eq major-mode 'erc-mode)
		      (setq erc-fill-column (- (window-width w) 2)))))))))
