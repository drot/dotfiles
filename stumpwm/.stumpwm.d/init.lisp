;;; init.lisp --- StumpWM user configuration

;; Author: drot
;; Created 30-01-2011

;;; Commentary:

;; Tomorrow Night themed, mostly default key bindings. `stumptray' module requires
;; `xembed' to be installed via Quicklisp. `ttf-fonts' module requires
;; `clx-truetype' to be installed via Quicklisp and running `xft:cache-fonts'
;; for initial font cache population.

;;; Code:

(in-package :stumpwm)

;; Load Slynk
(push (car (directory "~/.emacs.d/elpa/sly-2*/slynk/")) asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'slynk)

;; Command for on-demand starting of the Swank server
(defcommand slynk () ()
  "Creates a Slynk server in the StumpWM Lisp process."
  (slynk:create-server
   :port 4005
   :dont-close t)
  (echo-string (current-screen) "Starting Slynk..."))

(define-key *root-map* (kbd "C-s") "slynk")

;; Load contrib modules
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "stumptray")
(load-module "ttf-fonts")

;; Change default color map
(setf *colors* '("#1d1f21"              ; 0 black
                 "#cc6666"              ; 1 red
                 "#b5bd68"              ; 2 green
                 "#f0c674"              ; 3 yellow
                 "#81a2be"              ; 4 blue
                 "#b294bb"              ; 5 magenta
                 "#8abeb7"              ; 6 cyan
                 "#c5c8c6"))            ; 7 white
(update-color-map (current-screen))

;; Font
(set-font (make-instance 'xft:font
                         :family "Iosevka Term"
                         :subfamily "Regular"
                         :size 12))

;; Startup message
(setf *startup-message* "^4*StumpWM^n ^7*has^n ^3*initialized^n^6*.^n")

;; Window colors
(set-win-bg-color "#373b41")
(set-focus-color "#282a2e")
(set-unfocus-color "#1d1f21")
(set-float-focus-color "#282a2e")
(set-float-unfocus-color "#1d1f21")

;; Message and input prompt colors
(set-border-color "#373b41")
(set-fg-color "#c5c8c6")
(set-bg-color "#1d1f21")

;; Message and input prompt style
(setf *input-window-gravity* :center)
(setf *message-window-gravity* :bottom-right)
(setf *input-history-ignore-duplicates* 1)

;; Grabbed pointer style
(setf *grab-pointer-character* 40)
(setf *grab-pointer-character-mask* 41)
(setf *grab-pointer-foreground* (hex-to-xlib-color "#1d1f21"))
(setf *grab-pointer-background* (hex-to-xlib-color "#f0c674"))

;; Disable mouse focus
(setf *mouse-focus-policy* :sloppy)

;; Mode line colors
(setf *mode-line-foreground-color* "#c5c8c6")
(setf *mode-line-background-color* "#1d1f21")
(setf *mode-line-border-color* "#373b41")

;; Group and window format
(setf *group-format* "%n%s(%t)")
(setf *window-format* "%m%n%s(%50t) ")

;; Mode line time format
(setf *time-modeline-string* "^5*%e-%m^n ^3*%R^n")

;; Mode line format
(setf *screen-mode-line-format* '("<^7*%n^n> ^06%u^n ^30%W^n ^>"
                                  " ^2*%c^n >> ^4*%M^n >> ^3*%l^n >> %d %T"))

;; Show the mode line for the current screen
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))

;; Toggle tray space
(stumptray:stumptray)

;; First group name and other group creation
(setf (group-name (car (screen-groups (current-screen)))) "term")
(run-commands "gnewbg www"
              "gnewbg emacs"
              "gnewbg misc"
              "gnewbg-float float")

;; Default terminal
(defcommand terminal () ()
  "Start a terminal session."
  (run-shell-command "kitty"))

;; Volume control functions
(defcommand voltoggle () ()
  "Toggle volume."
  (run-shell-command "pactl set-sink-mute 0 toggle"))

(defcommand volplus () ()
  "Increase volume."
  (run-shell-command "pactl set-sink-volume 0 +5%"))

(defcommand volminus () ()
  "Decrease volume"
  (run-shell-command "pactl set-sink-volume 0 -5%"))

;; Run or raise
(defcommand eclient () ()
  "Run/Raise Emacsclient."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand firefox () ()
  "Run/Raise Firefox."
  (run-or-raise "firefox" '(:class "Firefox")))

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

;; Display prefix key in use
(defun key-seq-msg (key key-seq cmd)
  "Show a message with current incomplete key sequence."
  (declare (ignore key))
  (or (eq *top-map* *resize-map*)
      (stringp cmd)
      (let ((*message-window-gravity* :bottom-left))
        (message "~A" (print-key-seq (reverse key-seq))))))

(add-hook *key-press-hook* 'key-seq-msg)

;; Swap default key bindings
(define-key *root-map* (kbd "C-b") "firefox")
(define-key *root-map* (kbd "c") "terminal")
(define-key *root-map* (kbd "C-e") "eclient")

;; Volume control key bindings
(define-key *top-map* (kbd "XF86AudioMute") "voltoggle")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volplus")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volminus")

;;; init.lisp ends here
