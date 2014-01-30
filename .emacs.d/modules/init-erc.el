;;; init-erc.el --- Configuration for ERC

;; Load ERC
(require 'erc)

;; Load modules
(add-to-list 'erc-modules 'notifications)
(add-to-list 'erc-modules 'scrolltobottom)
(add-to-list 'erc-modules 'smiley)

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "adams.freenode.net" :port 6697
           :nick "drot")
  (erc-tls :server "pine.forestnet.org" :port 6697
           :nick "drot"))

;; Auto identify
(when (file-exists-p (expand-file-name "~/.ercpass"))
  (load "~/.ercpass")
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode (("drot" . ,freenode-password)))
          (ForestNet (("drot" . ,freenode-password))))))

;; Auto join selected channels
(setq erc-autojoin-channels-alist '(("freenode" "#archlinux" "#emacs")
                                      ("forestnet" "#reloaded" "#fo2")))

;; Enable Fly Spell mode
(erc-spelling-mode 1)

;; Static text fill
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-column 100)
(setq erc-fill-static-center 15)

;; Hide IRC spam
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "AWAY"))

;; Don't track the server buffer
(setq erc-track-exclude-server-buffer t)

;; Tracking options
(setq erc-track-showcount t)
(setq erc-track-switch-direction 'importance)
(setq erc-track-visibility 'selected-visible)

;; Show timestamp on the left side
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-only-if-changed-flag nil)

;; Interpret mIRC colors
(setq erc-interpret-mirc-color t)

;; Default nick uniquifer
(setq erc-nick-uniquifier "_")

;; Disable nick buttonization
(setq erc-button-buttonize-nicks nil)

;; Prompt format
(setq erc-prompt (lambda ()
                   (if erc-network
                       (concat "[" (symbol-name erc-network) "]")
                     (concat "[" (car erc-default-recipients) "]"))))

(defun erc-button-url-previous ()
  "Go to the previous URL button in this buffer."
  (interactive)
  (let* ((point (point))
         (found (catch 'found
                  (while (setq point (previous-single-property-change point 'erc-callback))
                    (when (eq (get-text-property point 'erc-callback) 'browse-url)
                      (throw 'found point))))))
    (if found
        (goto-char found)
      (error "No previous URL button."))))

(define-key erc-mode-map [backtab] 'erc-button-url-previous)

(provide 'init-erc)

;;; init-erc.el ends here
