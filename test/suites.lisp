(in-package #:maclina.test)

;;; We can do more tests with the cross version.
(5am:def-suite maclina-cross)

(5am:def-suite maclina :in maclina-cross)

(5am:def-suite ansi :in maclina)
(5am:def-suite eval-and-compile :in ansi)
(5am:def-suite data-and-control-flow :in ansi)
(5am:def-suite system-construction :in ansi)

(5am:def-suite compiler-conditions :in maclina)

(5am:def-suite fasl :in maclina-cross)
