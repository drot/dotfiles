;; spelling
(erc-spelling-mode 1)

;; colorize nicks
(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

;; logging
(setq erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/.emacs.d/logs/"
      erc-log-file-coding-system 'utf-8
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;; look
(setq erc-nick-uniquifier "_"
      erc-notice-prefix "* "
      erc-prompt ">>"
      erc-timestamp-format "%H:%M "
      erc-button-buttonize-nicks nil
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-only-if-changed-flag nil
      erc-hide-list '("JOIN" "PART" "QUIT"))

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
