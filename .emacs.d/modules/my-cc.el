;;
;; my-cc.el - Configuration for the C/C++ mode
;;

;; General indentation style
(setq c-basic-offset 4)

(defun my-c-mode-hook ()
  "C mode setup"
  (c-set-style "k&r")
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "gcc " (buffer-file-name) " -o "))))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; C++ indentation style
(c-add-style "my-c++-style"
	     '("stroustrup"
	       (c-offsets-alist . ((inline-open . 0)
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  "C++ mode setup"
  (c-set-style "my-c++-style")
  (c-toggle-auto-hungry-state 1)
  (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "g++ " (buffer-file-name) " -o "))))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Enable Auto Fill mode
(add-hook 'c-mode-common-hook 'auto-fill-mode)

(provide 'my-cc)
;; my-cc.el ends here
