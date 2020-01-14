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

;; Setup of early initialization options.

;;; Code:

;;; Delay garbage collection during startup
(defun drot/reset-gc-cons-threshold ()
  "Resets the garbage collection threshold to its default value."
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))

;; Increase threshold to maximum possible value
(setq gc-cons-threshold most-positive-fixnum)
;; Reset threshold after initialization
(add-hook 'after-init-hook #'drot/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun drot/reset-file-name-handler-alist ()
  "Resets the file name handler to its default value."
  (setq file-name-handler-alist
	    (append default-file-name-handler-alist
		        file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(add-hook 'after-init-hook #'drot/reset-file-name-handler-alist)

;;; Prefer newest version of a file
(setq load-prefer-newer t)

;;; Disable the site default settings
(setq inhibit-default-init t)

;;; Disable scroll bars and fringes in minibuffer area
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;;; Package configuration
(require 'package)

;; Enable package quickstart
(setq package-quickstart t)

;; Add the MELPA package archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Pinned packages
(setq package-pinned-packages '((json-mode . "gnu")))

;; Helper function for installing packages
(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;; early-init.el ends here
