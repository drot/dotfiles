;; spelling
(erc-spelling-mode 1)

;; look
(setq erc-nick-uniquifier "_"
      erc-notice-prefix "* "
      erc-prompt ">>"
      erc-match-mode t
      erc-button-buttonize-nicks nil
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%H:%M "
      erc-timestamp-only-if-changed-flag nil
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-log-channels-directory "~/.emacs.d/logs/"
      erc-save-buffer-on-part t)

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
