;; my-skeletons.el - Skeleton mode skeletons

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

(provide 'my-skeletons)
;; my-skeletons.el ends here
