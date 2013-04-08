;;
;; drot-packages.el - Emacs default package selection
;;

;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Check for installed packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar drot-package-list
  '(pkgbuild-mode undo-tree smartparens yasnippet helm helm-descbinds diminish zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p drot-package-list)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'drot-packages)
;; drot-packages.el ends here
