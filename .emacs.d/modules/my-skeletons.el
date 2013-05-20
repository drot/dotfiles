;; my-skeletons.el - Skeleton mode skeletons

(define-skeleton cpp-boiler
  "C++ boilerplate"
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
