;;
;; my-pkgbuild.el - PKGBUILD mode configuration
;;

;; Enable PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(provide 'my-pkgbuild)
;; my-pkgbuild.el ends here
