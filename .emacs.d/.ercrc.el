;; beeping
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-beep-match-types '(current-nick keyword))

;; nick colors
(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

;; look
(custom-set-variables
  '(erc-nick-uniquifier "_")
 '(erc-notice-prefix "-*- ")
 '(erc-prompt "~>>"))

;; colors
(custom-set-faces
 '(erc-notice-face ((t (:foreground "SlateBlue" :weight normal))))
 '(erc-input-face ((t (:foreground "Green"))))
 '(erc-button ((t (:background "black" :foreground "Cyan" :underline "Cyan" :weight normal))))
 '(erc-my-nick-face ((t (:foreground "Green" :weight bold))))
 '(erc-prompt-face ((t (:background "Black" :foreground "Green" :weight normal))))
 '(erc-timestamp-face ((t (:foreground "Green" :weight normal)))))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

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
