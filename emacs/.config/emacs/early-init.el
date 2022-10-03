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

;;; Disable `package' initialization
(setq package-enable-at-startup nil)

;;; Bootstrap `elpaca'
(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca (elpaca :host github :repo "progfolio/elpaca"))

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
