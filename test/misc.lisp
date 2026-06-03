(in-package #:maclina.test)

(5am:def-suite misc :in maclina)
(5am:in-suite misc)

(deftest special-mid-binding
    ((lambda (x *y* z) (declare (special *y*)) (values x *y* z)) 1 2 3)
  1 2 3)
