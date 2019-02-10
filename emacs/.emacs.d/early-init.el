;;; early-init.el --- drot Emacs early configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2019  drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/master/emacs/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Setting up early initialization options.

;;; Code:

;;; Delay garbage collection during startup
(defun drot/reset-gc-cons-threshold ()
  "Reset garbage collection threshold."
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))

;; Use the highest number possible
(setq gc-cons-threshold most-positive-fixnum)

;; Reset garbage collection threshold value to default after startup
(add-hook 'after-init-hook #'drot/reset-gc-cons-threshold)

;;; Prefer newest version of a file
(setq load-prefer-newer t)

;;; Disable the site default settings
(setq inhibit-default-init t)

;;; Package configuration
(require 'package)

;; Add the MELPA package archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Pinned packages
(setq package-pinned-packages '((json-mode . "gnu")))

;; Speed up package startup
(setq package-quickstart-file (locate-user-emacs-file "elpa/package-quickstart.el")
      package-quickstart t)

;; Helper function for installing packages
(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;; early-init.el ends here
