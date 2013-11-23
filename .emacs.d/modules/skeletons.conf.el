;;; skeletons.conf.el --- Skeleton mode skeletons

;; Prevent skeleton/abbrev recursion
(setq skeleton-further-elements '((abbrev-mode nil)))

(define-skeleton my-cpp-skel
  "C++ skeleton"
  nil
  "#include <iostream>\n"
  "\n"
  "int main ()\n"
  "{\n"
  > _
  "\n"
  > "return 0;"
  "\n}")

(provide 'skeletons.conf)

;;; skeletons.conf.el ends here
