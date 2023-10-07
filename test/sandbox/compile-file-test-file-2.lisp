(s:in-package "MACLINA.TEST")

(s:defun compile-file-test-fun.2 () nil)

(eval-when (:compile-toplevel)
  (s:unless (find-class 'compile-file-test-condition.2 nil)
    (define-condition compile-file-test-condition.2 (style-warning) nil))
  (warn (make-condition 'compile-file-test-condition.2)))
