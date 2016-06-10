(in-package :stumpwm)

;; Load swank
(load (car (directory "~/.emacs.d/elpa/slime-*/swank-loader.lisp")))

(swank-loader:init)

(swank:create-server
 :port 4666
 :style swank:*communication-style*
 :dont-close t)

;; Load contrib modules
(load-module "cpu")
(load-module "mem")
(load-module "net")

;; Font
(set-font "-xos4-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1")

;; Change default color map
(setf *colors* '("#5f5f5f"              ; 0 black
                 "#dca3a3"              ; 1 red
                 "#9fc59f"              ; 2 green
                 "#f0dfaf"              ; 3 yellow
                 "#8cd0d3"              ; 4 blue
                 "#dc8cc3"              ; 5 magenta
                 "#93e0e3"              ; 6 cyan
                 "#dcdccc"))            ; 7 white
(update-color-map (current-screen))

;; Startup message
(setf *startup-message* "^4*StumpWM^n ^2*has^n ^3*initialized^n^6*.^n")

;; Window colors
(set-win-bg-color "#3f3f3f")
(set-focus-color "#5f5f5f")
(set-unfocus-color "#1e2320")
(set-float-focus-color "#5f5f5f")
(set-float-unfocus-color "#1e2320")

;; Message and input prompt colors
(set-border-color "#5f5f5f")
(set-fg-color "#dcdccc")
(set-bg-color "#1e2320")

;; Grabbed pointer style
(setq *grab-pointer-character* 40
      *grab-pointer-character-mask* 41
      *grab-pointer-foreground* (hex-to-xlib-color "#1e2320")
      *grab-pointer-background* (hex-to-xlib-color "#f0dfaf"))

;; Window style
(setf *window-border-style* :thin
      *maxsize-border-width* 1
      *mouse-focus-policy* :click)

;; Message and input prompt style
(setf *input-window-gravity* :center
      *message-window-gravity* :bottom-right
      *input-history-ignore-duplicates* 1)

;; Mode line colors
(setf *mode-line-foreground-color* "#dcdccc"
      *mode-line-background-color* "#3f3f3f"
      *mode-line-border-color* "#5f5f5f")

;; Mode line format
(setf *time-modeline-string* "^3*%d-%m %H:%M^n"
      *group-format* "%n %s %t"
      *screen-mode-line-format* '("^3*%n^n %W ^> "
                                  "^2*%c^n> ^4*%M^n> ^7*%l^n> %d")
      *mode-line-timeout* 5)

;; Show the mode line for current screen
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))

;; Show incomplete key sequences
(defun key-seq-msg (key key-seq cmd)
  "Show a message with current incomplete key sequence."
  (declare (ignore key))
  (or (eq *top-map* *resize-map*)
      (stringp cmd)
      (let ((*message-window-gravity* :bottom-left))
        (message "~A" (print-key-seq (reverse key-seq))))))

(add-hook *key-press-hook* 'key-seq-msg)

;; First group name and other group creation
(setf (group-name (car (screen-groups (current-screen)))) "term")

(gnewbg "www")
(gnewbg "emacs")
(gnewbg-float "float")

;; Run or raise
(defcommand eclient () ()
  "Run/Raise Emacsclient"
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand conkeror () ()
  "Run/Raise Conkeror"
  (run-or-raise "conkeror" '(:class "Conkeror")))

(defcommand gimp () ()
  "Run/Raise GIMP"
  (run-or-raise "gimp" '(:class "Gimp")))

;; Window placement
(clear-window-placement-rules)

(define-frame-preference "www"
    (0 t t :class "Conkeror"))

(define-frame-preference "emacs"
    (0 t t :class "Emacs"))

(define-frame-preference "misc"
    (0 t t :class "Gimp"))

;; Prefix key
(set-prefix-key (kbd "C-i"))

;; Swap defaults
(define-key *root-map* (kbd "C-c") "conkeror")
(define-key *root-map* (kbd "c") "exec st")
(define-key *root-map* (kbd "e") "eclient")

;; Fast group switching
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-6") "gselect 6")
(define-key *top-map* (kbd "s-7") "gselect 7")
(define-key *top-map* (kbd "s-8") "gselect 8")
(define-key *top-map* (kbd "s-9") "gselect 9")
(define-key *top-map* (kbd "s-0") "gselect 10")
