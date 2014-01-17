;;; init-packages.el --- Emacs default package selection

(defvar el-get-dir (expand-file-name "el-get" my-emacs-dir)
  "El-Get root directory")
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))

;; Bootstrap El-Get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Additional recipes
(add-to-list 'el-get-recipe-path (expand-file-name "recipes" my-emacs-dir))

(defvar my-package-list
  '(auto-complete
    el-get
    flatland-emacs
    ido-hacks
    magit
    org-mode
    paredit
    rainbow-delimiters
    rcirc-color
    undo-tree)
  "A list of packages to ensure are installed at launch.")

(el-get 'sync my-package-list)

(provide 'init-packages)

;;; init-packages.el ends here
