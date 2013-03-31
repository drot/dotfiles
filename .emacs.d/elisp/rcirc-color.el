;;; rcirc-color.el -- color nicks
;; Copyright 2005, 2006, 2007, 2008, 2010, 2012  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Use /COLOR to list all colored nicks with their color
;; Use /COLOR NICK COLOR to color NICK using COLOR

;;; Code:

(require 'rcirc)

(defun rcirc-color-distance (color1 color2)
  "Compute the difference between two colors
using the weighted Euclidean distance formula proposed on
<http://www.compuphase.com/cmetric.htm>.
Remember that every component for the formula is in the range of 0-xFF
and `color-values' will return a range of 0-FFFF. Thus, divide everything
by 256. This also helps preventing integer overflow."
  (let* ((dr (/ (- (nth 0 (color-values color1))
		   (nth 0 (color-values color2))) 256))
	 (dg (/ (- (nth 1 (color-values color1))
		   (nth 1 (color-values color2))) 256))
	 (db (/ (- (nth 2 (color-values color1))
		   (nth 2 (color-values color2))) 256))
	 (red-mean (/ (+ (nth 0 (color-values color1))
			 (nth 0 (color-values color2)))
		      2 256)))
    (sqrt (+ (ash (* (+ 512 red-mean) dr dr) -8)
	     (* 4 dg dg)
	     (ash (* (- 767 red-mean) dr dr) -8)))))

(defvar rcirc-colors
  (let ((min-distance 200); heuristics
	(bg (face-background 'default))
	(fg (face-foreground 'rcirc-my-nick))
	candidates)
    (dolist (item color-name-rgb-alist)
      (let ((color (car item)))
	(when (and (not (color-gray-p color))
		   (> (rcirc-color-distance color bg) min-distance)
		   (> (rcirc-color-distance color fg) min-distance))
	  (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  This uses `rcirc-color-distance'
to compute distance between colors.

To check out the list, evaluate (list-colors-display rcirc-colors).")

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defvar rcirc-color-is-deterministic nil
  "Normally rcirc just assigns random colors to nicks.
These colors are based on the list in `rcirc-colors'.
If you set this variable to a non-nil value, an md5 hash is
computed based on the nickname and the first twelve bytes are
used to determine the color: #rrrrggggbbbb.")

(defadvice rcirc-facify (before rcirc-facify-colors activate)
  "Add colors to other nicks based on `rcirc-colors'."
  (when (and (eq face 'rcirc-other-nick)
             (not (string= string "")))
    (let ((cell (gethash string rcirc-color-mapping)))
      (unless cell
	(setq cell (cons 'foreground-color
			 (if rcirc-color-is-deterministic
			     (concat "#" (substring (md5 string) 0 12))
			   (elt rcirc-colors (random (length rcirc-colors))))))
        (puthash (substring-no-properties string) cell rcirc-color-mapping))
      (setq face (list cell)))))

(defun rcirc-markup-nick-colors (sender response)
  (with-syntax-table rcirc-nick-syntax-table
    (while (re-search-forward "\\w+" nil t)
      (let ((face (gethash (match-string-no-properties 0) rcirc-color-mapping)))
	(when face
	  (rcirc-add-face (match-beginning 0) (match-end 0) face))))))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-nick-colors)

(defun-rcirc-command color (args)
  "Change one of the nick colors."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-color (car args) (cadr args) process target))

(defun rcirc-do-color (nick color process target)
  "Implement /COLOR."
  (if (not nick)
      (let (names)
        (maphash (lambda (key value)
                   (add-text-properties
                    0 (length key)
                    `(face (,value) help-echo ,(cdr value))
                    key)
                   (setq names (cons key names)))
                 rcirc-color-mapping)
        (rcirc-print process (rcirc-nick process) "NOTICE" target
                     (mapconcat 'identity names " ")))
    (unless color
      (error "Use what color?"))
    (puthash nick (cons 'foreground-color color) rcirc-color-mapping)))

(defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
         (cell (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (when cell
      (puthash new-nick cell rcirc-color-mapping))))

(provide 'rcirc-color)

;;; rcirc-color.el ends here
