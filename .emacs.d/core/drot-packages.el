;;
;; drot-packages.el - Emacs default package selection
;;

;; Put el-get into the Emacs directory
(setq el-get-dir (file-name-as-directory (expand-file-name "el-get" drot-emacs-dir)))
(add-to-list 'load-path (file-name-as-directory (expand-file-name "el-get" el-get-dir)))
;; Fetch only latest versions from git repositories
(setq el-get-git-shallow-clone t)
;; Bootstrap el-get
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (end-of-buffer)
      (eval-print-last-sexp)))
  (require 'el-get))

;; Additional recipes
(add-to-list 'el-get-recipe-path (expand-file-name "rcp" drot-emacs-dir))

(defvar drot-package-list '(auto-complete
                            diminish
                            emacs-clang-complete-async
                            jazz-theme
                            paredit
                            magit
                            pkgbuild-mode
                            undo-tree
                            yasnippet
                            ido-hacks)
  "A list of packages to ensure are installed at launch.")

(el-get 'sync drot-package-list)

(provide 'drot-packages)
;; drot-packages.el ends here
