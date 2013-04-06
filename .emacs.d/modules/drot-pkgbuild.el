;;
;; drot-pkgbuild.el - Configuration for the Arch Linux PKGBUILD mode
;;

;; Enable PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(provide 'drot-pkgbuild)
;; drot-pkgbuild.el ends here
