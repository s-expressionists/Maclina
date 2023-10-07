(in-package #:maclina.test)

;;;; Tests that the cross VM plays well with host code.

(5am:def-suite cooperation :in maclina-cross)
(5am:in-suite cooperation)

(5am:test return-through-unbind
  (let ((rte (maclina.compile:run-time-environment m:*client* *environment*)))
    (m:progv m:*client* rte '(x) '(:good)
      ;; Return to a native block through a VM binding.
      (flet ((outer (inner)
               (block nil
                 (funcall inner (lambda () (return))))
               (m:symbol-value m:*client* rte 'x)))
        (let ((inner (ceval '#'(lambda (f)
                                 (let ((x :bad))
                                   (declare (special x))
                                   (funcall f))))))
          (5am:is (eql :good (outer inner)))))
      ;; Return to a VM block through a native binding.
      ;; This essentially tests M:PROGV's NLX behavior.
      (flet ((inner (f)
               (m:progv m:*client* rte '(x) '(:bad) (funcall f))))
        (5am:is (eql :good
                     (ceval `(progn
                               (block nil
                                 (funcall ,#'inner
                                          #'(lambda () (return))))
                               (locally (declare (special x)) x)))))))))
