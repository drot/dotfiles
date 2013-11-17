;;
;; init.el - Modular Emacs configuration
;;

;; Define directories
(defvar my-emacs-dir (file-name-directory load-file-name)
  "The root directory of the Emacs distribution.")

(defvar my-core-dir (expand-file-name "core" my-emacs-dir)
  "This directory houses the Emacs core configuration.")

(defvar my-modules-dir (expand-file-name "modules" my-emacs-dir)
  "This directory houses all of the Emacs modules.")

(defvar my-saves-dir (expand-file-name "saves" my-emacs-dir)
  "This directory houses all save files.")

(defvar my-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid))
  "This directory houses all backup and auto-save files.")

(defvar my-elisp-dir (expand-file-name "elisp" my-emacs-dir)
  "This directory houses all custom elisp files.")

;; Define customize file
(defvar my-custom-file (expand-file-name "custom.el" my-saves-dir)
  "Store changes from the customize interface in the selected file.")

;; Add directories to load path
(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-modules-dir)
(add-to-list 'load-path my-elisp-dir)

;; Load packages
(require 'my-packages)

;; Load UI configuration
(require 'my-ui)

;; Load general configuration
(require 'my-defaults)

;; Load editing-specific configuration
(require 'my-editing)

;; Load modules
(require 'my-modules)

;; Load custom key bindings
(require 'my-keybindings)

;; Load changes from the customize interface
(if (file-exists-p my-custom-file)
  (load my-custom-file))
;; init.el ends here
