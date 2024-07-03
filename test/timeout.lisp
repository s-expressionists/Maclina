(in-package #:maclina.test)

;;;; Tests of the timeout mechanism.

(5am:def-suite timeout :in maclina-cross)
(5am:in-suite timeout)

(5am:test timeout
  (let ((spinner
          (ceval '#'(lambda (i)
                      (block nil
                        (tagbody
                         loop
                           (if (= 0 i) (return))
                           (setq i (- i 1))
                           (go loop)))))))
    (5am:finishes (maclina.vm-cross:with-timeout (1000000)
                    ;; It is unlikely that any future revisions will be
                    ;; dumb enough that a loop iteration takes 100k instructions.
                    (funcall spinner 10)))
    (5am:signals maclina.vm-cross:timeout
      (maclina.vm-cross:with-timeout (10)
        (funcall spinner 1000)))
    ;; make sure that without a timeout, we can run as long as we want.
    (5am:finishes (funcall spinner 1000))))
