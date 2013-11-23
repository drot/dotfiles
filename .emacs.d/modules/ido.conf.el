;;; ido.conf.el --- Configuration for Ido

;; Enable Ido
(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 5
      ido-save-directory-list-file (expand-file-name "ido.hist" my-saves-dir)
      ido-default-file-method 'selected-window
      ido-everywhere t)
(ido-mode 1)

;; Extend Ido completion
(require 'ido-hacks)
(ido-hacks-mode 1)

(provide 'ido.conf)

;;; ido.conf.el ends here
