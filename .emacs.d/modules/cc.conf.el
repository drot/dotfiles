;;; cc.conf.el --- Configuration for the C/C++ Mode

;; General indentation style
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

(defun my-c-mode-hook ()
  "C mode setup"
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "gcc " (buffer-file-name) " -o "))))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  "C++ mode setup"
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "g++ " (buffer-file-name) " -o "))))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Enable Auto Fill mode
(add-hook 'c-mode-common-hook 'auto-fill-mode)

(provide 'cc.conf)

;;; cc.conf.el ends here
