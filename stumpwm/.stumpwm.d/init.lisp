;;; init.lisp --- StumpWM user configuration

;; Author: drot
;; Created 30-01-2011

;;; Commentary:

;; Zenburn themed, mostly default key bindings.

;;; Code:

(in-package :stumpwm)

;; Load swank
(load (car (directory "~/.emacs.d/elpa/slime-*/swank-loader.lisp")))

(swank-loader:init)

(defcommand swank () ()
  "Creates a Swank server in the StumpWM Lisp process."
  (swank:create-server
   :port 4666
   :style swank:*communication-style*
   :dont-close t)
  (echo-string (current-screen) "Starting Swank..."))

(define-key *root-map* (kbd "C-s") "swank")

;; Load contrib modules
(mapcar #'load-module '("cpu"
                        "mem"
                        "net"))

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

;; Font
(set-font "-*-terminus-bold-*-*-*-18-*-*-*-*-*-*-*")

;; Startup message
(setf *startup-message* "^4*StumpWM^n ^7*has^n ^3*initialized^n^6*.^n")

;; Window colors
(set-win-bg-color "#5f5f5f")
(set-focus-color "#9fc59f")
(set-unfocus-color "#1e2320")
(set-float-focus-color "#9fc59f")
(set-float-unfocus-color "#1e2320")

;; Message and input prompt colors
(set-border-color "#5f5f5f")
(set-fg-color "#dcdccc")
(set-bg-color "#1e2320")

;; Grabbed pointer style
(setf *grab-pointer-character* 40)
(setf *grab-pointer-character-mask* 41)
(setf *grab-pointer-foreground* (hex-to-xlib-color "#1e2320"))
(setf *grab-pointer-background* (hex-to-xlib-color "#f0dfaf"))

;; Message and input prompt style
(setf *input-window-gravity* :center)
(setf *message-window-gravity* :bottom-right)
(setf *input-history-ignore-duplicates* 1)

;; Mode line colors
(setf *mode-line-foreground-color* "#dcdccc")
(setf *mode-line-background-color* "#3f3f3f")
(setf *mode-line-border-color* "#5f5f5f")

;; Group and window format
(setf *group-format* "%n%s(%t)")
(setf *window-format* "%m%n%s(%50t) ")

;; Mode line format
(setf *time-modeline-string* "^6*%d-%m^n ^3*%H:%M^n")
(setf *screen-mode-line-format* '("(^7*%n^n) ^06%u^n ^30%W^n ^>"
                                  " (^2*%c^n)(^4*%M^n)(^7*%l^n)(%d)"))

;; Show the mode line for current screen
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))

;; First group name and other group creation
(setf (group-name (car (screen-groups (current-screen)))) "term")
(run-commands "gnewbg www"
              "gnewbg emacs"
              "gnewbg misc"
              "gnewbg-float float")

;; Default terminal
(defcommand terminal () ()
  "Start a terminal session."
  (run-shell-command "termite"))

;; Volume control functions
(defcommand voltoggle () ()
  "Toggle volume."
  (run-shell-command "ponymix -N toggle"))

(defcommand volplus () ()
  "Increase volume."
  (run-shell-command "ponymix -N increase 5"))

(defcommand volminus () ()
  "Decrease volume"
  (run-shell-command "ponymix -N decrease 5"))

;; Run or raise
(defcommand eclient () ()
  "Run/Raise Emacsclient."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand conkeror () ()
  "Run/Raise Conkeror."
  (run-or-raise "conkeror" '(:class "Conkeror")))

(defcommand gimp () ()
  "Run/Raise GIMP."
  (run-or-raise "gimp" '(:class "Gimp")))

;; Window placement
(clear-window-placement-rules)

(define-frame-preference "www"
    (0 t t :class "Conkeror"))

(define-frame-preference "emacs"
    (0 t t :class "Emacs"))

(define-frame-preference "misc"
    (0 t t :class "Gimp"))

(define-frame-preference "misc"
    (0 t t :class "libreoffice"))

(define-frame-preference "misc"
    (0 t t :class "Zathura"))

;; Change default prefix key
(set-prefix-key (kbd "C-i"))

;; Swap default key bindings
(define-key *root-map* (kbd "C-b") "conkeror")
(define-key *root-map* (kbd "c") "terminal")
(define-key *root-map* (kbd "C-e") "eclient")

;; Volume control key bindings
(define-key *top-map* (kbd "s-period") "voltoggle")
(define-key *top-map* (kbd "s-plus") "volplus")
(define-key *top-map* (kbd "s-minus") "volminus")

;;; init.lisp ends here
