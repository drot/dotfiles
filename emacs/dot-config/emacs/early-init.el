;;; early-init.el --- drot Emacs early configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2023  drot

;; Author: drot
;; URL: https://github.com/drot/dotfiles/tree/work-arch/emacs/.config/emacs
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

;;; Disable needless GUI elements
(dolist (mode '(tool-bar-mode
                menu-bar-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Disable scroll bars and fringes in minibuffer area
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;;; Bootstrap `elpaca'
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Disable `package' initialization
(setq package-enable-at-startup nil)

;;; Don't popup the async compilation buffer
(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
