(defvar drot/emacs-directory (file-name-directory load-file-name)
  "Emacs root directory.")

(defvar drot/configuration-file (expand-file-name "config.org" drot/emacs-directory)
  "Emacs configuration file written and loaded with Org-mode")

(defvar drot/custom-file (expand-file-name "custom.el" drot/emacs-directory)
  "Store changes from the customize interface in the selected file.")

(defvar drot/cache-directory (expand-file-name "cache" drot/emacs-directory)
  "This directory houses all cache files.")
(make-directory drot/cache-directory t)

;; Reduce GC frequency
(setq gc-cons-threshold (* 20 (expt 2 20)))

;; Package repository selection and activation
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/"))
      package-enable-at-startup nil)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load configuraton file with Org-mode
(org-babel-load-file drot/configuration-file)
