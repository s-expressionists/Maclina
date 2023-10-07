(defpackage #:maclina.test.sham
  (:use)
  (:export #:expand-in-current-env #:notnot)
  (:export #:macroexpand-1 #:macroexpand #:eval)
  (:export #:note-defun)
  (:export #:multiple-value-bind #:setf #:incf #:decf #:push #:defun
           #:prog1 #:when #:unless #:return #:prog #:in-package))

(defpackage #:maclina.test
  (:use #:cl)
  (:local-nicknames (#:s #:maclina.test.sham)
		    (#:m #:maclina.machine))
  (:export #:run #:run!)
  (:export #:run-native! #:run-cross!)
  ;; We don't define these. They're shadowed so that if you
  ;; use one accidentally in a test, you get an obvious error.
  ;; We could shadow eval but that messes with e.g. eval-when.
  (:shadow #:eval #:compile))
