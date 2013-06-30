;;
;; my-packages.el - Emacs default package selection
;;

(require 'cl-lib)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defvar my-packages-list
  '(auto-complete
    diminish
    ido-hacks
    lua-mode
    magit
    monokai-theme
    paredit
    pkgbuild-mode
    rainbow-delimiters
    undo-tree)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  "Check if all packages in `my-packages-list' are installed."
  (cl-every #'package-installed-p my-packages-list))

(defun my-packages-installation ()
  "Install all packages listed in `my-packages-list'."
  (unless (my-packages-installed-p)
    ;; Check for new package versions
    (message "%s" "Emacs is now refreshing the package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; Install the missing packages
    (mapc #'package-install
     (cl-remove-if #'package-installed-p my-packages-list))))

(my-packages-installation)

(provide 'my-packages)
;; my-packages.el ends here
