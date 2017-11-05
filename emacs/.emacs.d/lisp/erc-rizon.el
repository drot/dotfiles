;;; erc-rizon.el --- Rizon authentication functions for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  drot

;; Author: drot
;; Keywords: convenience

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

;; Workaround for Rizon NickServ authentication

;;; Code:

(defun erc-rizon-fetch-password (&rest params)
  "Fetch password with PARAMS for ERC authentication from an encrypted source."
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun erc-rizon-nickserv-password ()
  "Fetch NickServ password for the Rizon IRC network."
  (erc-rizon-fetch-password :user "drot" :host "irc.rizon.net"))

(provide 'erc-rizon)
;;; erc-rizon.el ends here
