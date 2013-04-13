;;
;; drot-packages.el - Emacs default package selection
;;

;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Check for installed packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar drot-package-list '(auto-complete
                            diminish
                            paredit
                            pkgbuild-mode
                            undo-tree
                            yasnippet
                            ido-hacks
                            zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p drot-package-list)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'drot-packages)
;; drot-packages.el ends here
