;; Next browser configuration
(in-package :next-user)

;; Use Slynk instead of Swank for REPL connection
(ql:quickload :slynk)

;; Define custom Slynk command
(define-command start-slynk ()
  "Start a Slynk server that can be connected to, for instance, in Emacs via
SLY."
  (slynk:create-server :port *swank-port* :dont-close t))

;; Create custom keymap
(defvar *my-keymap* (make-keymap)
  "Keymap for `my-mode'.")

;; Set key bindings
(define-key :keymap *my-keymap*
  "C-h s" #'start-slynk)

;; Create custom mode for override purposes
(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-schemes :initform (list :emacs *my-keymap*
                                   :vi-normal *my-keymap*))))

;; Redirect to old Reddit
(defun old-reddit-handler (url)
  (let* ((uri (quri:uri url)))
    (if (search "www.reddit" (quri:uri-host uri))
        (progn
          (setf (quri:uri-host uri) "old.reddit.com")
          (let ((new-url (quri:render-uri uri)))
            (log:info "Switching to old Reddit: ~a" new-url)
            new-url))
        url)))

(defclass my-buffer (buffer)
  ((default-modes :initform
                  (cons 'my-mode (get-default 'buffer 'default-modes)))
   (load-hook :initform (next-hooks:make-hook-string->string
                         :handlers (list #'old-reddit-handler)
                         :combination #'next-hooks:combine-composed-hook))))

(setf *buffer-class* 'my-buffer)

;; Set search engines
(defclass my-remote-interface (remote-interface)
  ((search-engines
    :initform
    '(("ddg"
       "https://duckduckgo.com/?q=~a"
       "https://duckduckgo.com/")
      ("yt"
       "https://www.youtube.com/results?search_query=~a"
       "https://www.youtube.com/")
      ("wiki"
       "https://en.wikipedia.org/w/index.php?search=~a"
       "https://en.wikipedia.org/")))))

(setf *remote-interface-class* 'my-remote-interface)
