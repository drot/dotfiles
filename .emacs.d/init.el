;;
;; init.el
;;

;; Define directories
(defvar emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs distribution.")
(defvar core-dir (expand-file-name "core" emacs-dir)
  "The home of Emacs core functionality.")
(defvar modules-dir (expand-file-name "modules" emacs-dir)
  "This directory houses all of the Emacs modules.")
(defvar vendor-dir (expand-file-name "vendor" emacs-dir)
  "This directory houses packages that are not yet available.")
(defvar drot-modules-file (expand-file-name "drot-modules.el" emacs-dir)
  "This files contains a list of modules that will be loaded by Emacs.")

;; Add to load path
(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)

;; Core configuration
(require 'drot-packages)
(require 'drot-ui)
(require 'drot-core)
(require 'drot-editor)

;; Load modules
(when (file-exists-p drot-modules-file)
  (load drot-modules-file))

;; init.el ends here
