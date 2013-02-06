;; spelling
(erc-spelling-mode 1)

;; disable buttonization
(erc-button-mode nil)

;; colorize nicks
(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

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

;; logging
(setq erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/.emacs.d/logs/"
      erc-log-file-coding-system 'utf-8
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;; queries in new buffers, notices in current buffer
(setq erc-auto-query t
      erc-auto-query 'bury
      erc-echo-notice-always-hook '(erc-echo-notice-in-active-buffer))

;; dynamic fill width for buffers
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

;; truncate buffers
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)
