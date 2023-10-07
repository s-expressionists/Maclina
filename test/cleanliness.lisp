(in-package #:maclina.test)

;;;; Tests to make sure that evaluation works even in very empty environments.

(5am:def-suite cleanliness :in maclina-cross)
(5am:in-suite cleanliness)

(defvar *standard-special-operators*
  '(block      let*                  return-from
    catch      load-time-value       setq
    eval-when  locally               symbol-macrolet
    flet       macrolet              tagbody
    function   multiple-value-call   the
    go         multiple-value-prog1  throw
    if         progn                 unwind-protect
    labels     progv
    let        quote))

(5am:test special-operator-errors
  (let* ((empty
           (make-instance 'clostrum-basic:compilation-environment
             :parent (make-instance 'clostrum-basic:run-time-environment))))
    (loop for op in *standard-special-operators*
          do (5am:signals undefined-function (maclina.compile:eval `(,op) empty)))))

(defun make-sole-operator-env (operator)
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment :parent rte)))
    (clostrum:make-special-operator m:*client* rte operator t)
    ce))

(5am:test lone-special-operators
  ;; Test that an environment with only one special operator can work ok.
  (macrolet ((test-op (opname expected form)
               `(5am:is
                 (eql ,expected
                      (maclina.compile:eval ',form
                                        (make-sole-operator-env ',opname))))))
    (test-op block 1 (block nil 1))
    (test-op catch 2 (catch 3 2))
    (test-op eval-when 3 (eval-when (:execute) 3))
    (test-op flet 4 (flet ((f () 4)) (f)))
    (test-op if 5 (if 3 5 4))
    (test-op labels 6 (labels ((g () 6)) (g)))
    (test-op let 7 (let ((h 7)) h))
    (test-op let* 8 (let* ((i 8) (j i)) j))
    (test-op load-time-value 9 (load-time-value 9 t))
    (test-op locally 10 (locally (declare) 10))
    (test-op macrolet 11 (macrolet ((m () 11)) (m)))
    (test-op multiple-value-prog1 12 (multiple-value-prog1 12 17 289))
    (test-op progn 13 (progn 28 28 1 13))
    (test-op quote 14 (quote 14))
    (test-op symbol-macrolet 15 (symbol-macrolet ((s 15)) s))
    (test-op tagbody nil (tagbody 1 beta 2))
    (test-op the 16 (the integer 16))
    (test-op unwind-protect 17 (unwind-protect 17 18 19))))
