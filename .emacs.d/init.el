;;
;; init.el
;;

;; Define directories
(defvar drot-emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs distribution.")

(defvar drot-core-dir (expand-file-name "core" drot-emacs-dir)
  "This directory houses Emacs core configuration.")

(defvar drot-modules-dir (expand-file-name "modules" drot-emacs-dir)
  "This directory houses all of the Emacs modules.")

(defvar drot-personal-dir (expand-file-name "personal" drot-emacs-dir)
  "This directory houses all personal customizations.")

(defvar drot-vendor-dir (expand-file-name "vendor" drot-emacs-dir)
  "This directory houses packages that are not yet available.")

(defvar drot-saves-dir (expand-file-name "saves" drot-emacs-dir)
  "This directory houses all automatically generated save/history-files.")

;; Add directories to load path
(add-to-list 'load-path drot-core-dir)
(add-to-list 'load-path drot-modules-dir)
(add-to-list 'load-path drot-vendor-dir)

;; Load packages
(require 'drot-packages)

;; Load UI configuration
(require 'drot-ui)

;; Load general configuration
(require 'drot-core)

;; Load editing-specific configuration
(require 'drot-editor)

;; Load modules
(require 'drot-modules)

;; Load custom key bindings
(require 'drot-keybindings)

;; Store changes from the customize interface in the selected file
(setq custom-file (expand-file-name "custom.el" drot-personal-dir))

;; Load personal configuration
(when (file-exists-p drot-personal-dir)
  (message "Loading personal configuration files in %s..." drot-personal-dir)
  (mapc 'load (directory-files drot-personal-dir 't "^[^#].*el$")))

;; init.el ends here
