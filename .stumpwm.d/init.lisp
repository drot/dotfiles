(in-package :stumpwm)

;; Load swank
(load (car (directory "~/.emacs.d/elpa/slime-*/swank-loader.lisp")))

(swank-loader:init)

(swank:create-server
 :port 4666
 :style swank:*communication-style*
 :dont-close t)

;; Contrib modules
(set-contrib-dir "~/stumpwm/contrib/")
(load-module "cpu")
(load-module "mem")
(load-module "net")

;; Font
(set-font "-xos4-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1")

;; Change default color map
(setf *colors* (list "gray10"           ; 0
                     "PaleVioletRed3"   ; 1
                     "medium sea green" ; 2
                     "LightGoldenrod3"  ; 3
                     "SkyBlue"          ; 4
                     "wheat"            ; 5
                     "honeydew"         ; 6
                     "honeydew2"        ; 7
                     "honeydew3"        ; 8
                     "gray15"))         ; 9
(update-color-map (current-screen))

;; Startup message
(setf *startup-message* "^4*StumpWM^n ^6*has^n ^3*initialized^n^6*.^n")

;; Window colors
(set-win-bg-color "gray10")
(set-focus-color "honeydew2")
(set-unfocus-color "honeydew4")
(set-float-focus-color "honeydew2")
(set-float-unfocus-color "gray5")

;; Message and input prompt colors
(set-border-color "honeydew4")
(set-fg-color "honeydew")
(set-bg-color "gray10")

;; Window style
(setf *window-border-style* :thin
      *maxsize-border-width* 1
      *mouse-focus-policy* :click)

;; Message and input prompt style
(setf *input-window-gravity* :center
      *message-window-gravity* :bottom-right
      *input-history-ignore-duplicates* 1)

;; Mode line colors
(setf *mode-line-background-color* "gray10"
      *mode-line-foreground-color* "honeydew4"
      *mode-line-border-color* "gray5")

;; Mode line format
(setf *time-modeline-string* "^3*%d-%m %H:%M^n"
      *group-format* "%n %s %t"
      *screen-mode-line-format* (list "^5*%n^n %W ^> "
                                      "^3*%c^n| ^4*%M^n| ^5*%l^n| %d")
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

;; First group name and group creation
(setf (group-name (first (screen-groups (current-screen)))) "term")
(run-commands "gnewbg www" "gnewbg emacs" "gnewbg misc")

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
