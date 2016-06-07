;;; init.lisp --- Configuration for StumpWM

;;; Code:

(in-package :stumpwm)

;; Load swank
(require :swank)
(swank:create-server :port 4004
                     :dont-close t)

;; Change default color map
(setf *colors* (list "#3f3f3f"          ; 0 black
                     "#bc8383"          ; 1 red
                     "#5f7f5f"          ; 2 green
                     "#e0cf9f"          ; 3 yellow
                     "#7cb8bb"          ; 4 blue
                     "#dc8cc3"          ; 5 magenta
                     "#93e0e3"          ; 6 cyan
                     "#dcdccc"))        ; 7 white
(update-color-map (current-screen))

;; Window colors
(set-win-bg-color "#5f5f5f")
(set-focus-color "#5f5f5f")
(set-unfocus-color "#3f3f3f")
(set-float-focus-color "#5f5f5f")
(set-float-unfocus-color "#3f3f3f")

;; Message and input bar colors
(set-fg-color "#dcdccc")
(set-bg-color "#3f3f3f")
(set-border-color "#5f5f5f")

;; Message and input prompt style
(setf *input-window-gravity* :center
      *message-window-gravity* :bottom-right
      *input-history-ignore-duplicates* 1)

;; Toggle the mode line
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))
;;; init.lisp ends here
