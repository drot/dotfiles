;;
;; init.el
;;

;; Define directories
(defvar drot-emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs distribution.")
(defvar drot-core-dir (expand-file-name "core" drot-emacs-dir)
  "The home of Emacs core functionality.")
(defvar drot-modules-dir (expand-file-name "modules" drot-emacs-dir)
  "This directory houses all of the Emacs modules.")
(defvar drot-vendor-dir (expand-file-name "vendor" drot-emacs-dir)
  "This directory houses packages that are not yet available.")
(defvar drot-saves-dir (expand-file-name "saves" drot-emacs-dir)
  "This folder stores all the automatically generated save/history-files.")

;; Add to load path
(add-to-list 'load-path drot-core-dir)
(add-to-list 'load-path drot-modules-dir)
(add-to-list 'load-path drot-vendor-dir)

;; Core configuration
(require 'drot-packages)
(require 'drot-ui)
(require 'drot-core)
(require 'drot-editor)

;; Load modules
(require 'drot-modules)

;; init.el ends here
