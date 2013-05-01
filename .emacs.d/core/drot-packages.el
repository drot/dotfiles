;;
;; drot-packages.el - Emacs default package selection
;;

;; Put El-Get into the Emacs directory
(setq el-get-dir (file-name-as-directory (expand-file-name "el-get" drot-emacs-dir)))
(add-to-list 'load-path (file-name-as-directory (expand-file-name "el-get" el-get-dir)))

;; Bootstrap El-Get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Additional recipes
(add-to-list 'el-get-recipe-path (expand-file-name "recipes" drot-emacs-dir))

(defvar my-packages '(auto-complete
                      diminish
                      el-get
                      emacs-clang-complete-async
                      idomenu
                      ido-hacks
                      jazz-theme
                      lua-mode
                      paredit
                      magit
                      pkgbuild-mode
                      undo-tree
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

(el-get 'sync my-packages)

(provide 'drot-packages)
;; drot-packages.el ends here
