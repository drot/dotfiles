;;; my-packages.el --- Emacs default package selection

(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize packages
(package-initialize)

;; Refresh the package database
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-package-list
  '(auto-complete
    flatland-theme
    ido-hacks
    magit
    paredit
    rainbow-delimiters
    undo-tree)
  "A list of packages to install.")

;; Install packages
(dolist (p my-package-list)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'my-packages)

;;; my-packages.el ends here
