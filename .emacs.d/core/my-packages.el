;;
;; my-packages.el - Emacs default package selection
;;

(defvar el-get-dir (expand-file-name "el-get" my-emacs-dir)
  "El-Get root directory")
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))

;; Bootstrap El-Get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Additional recipes
(add-to-list 'el-get-recipe-path (expand-file-name "recipes" my-emacs-dir))

(defvar my-package-list
  '(anti-zenburn-theme
    auto-complete
    diminish
    el-get
    ido-hacks
    magit
    org-mode
    paredit
    pkgbuild-mode
    rainbow-delimiters
    undo-tree)
  "A list of packages to ensure are installed at launch.")

(el-get 'sync my-package-list)

(provide 'my-packages)
;; my-packages.el ends here
