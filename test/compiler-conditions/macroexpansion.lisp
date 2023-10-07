(in-package #:maclina.test)

(5am:def-suite macroexpansion-conditions :in compiler-conditions)
(5am:in-suite macroexpansion-conditions)

;;; Throughout this file we use the trick of having bytecode refer to a
;;; literal host function, so that we can test behavior without having to
;;; bother with defining functions in the VM environment.

(5am:test macro-error
  (flet ((die () (error "die")))
    ;; By default an error in a macroexpansion will just escape as-is, but
    ;; in the future some client may suppress the error, signal a warning
    ;; instead, and compile in a runtime error. Either is fine.
    (block nil
      (multiple-value-bind (fun warningsp failurep)
          (handler-case
              (ccompile nil
                        `(lambda () (macrolet ((m () (funcall ,#'die))) (m))))
            (error ()
              (5am:pass "Macro error escaped COMPILE")
              (return)))
        (declare (ignore fun))
        (5am:is-true warningsp "Macro error not reported as warning")
        (5am:is-true failurep "Macro error not reported as failure")))
    ;; Do it again in a compilation unit to make sure it fails immediately,
    ;; unlike what it should do with reference conditions.
    (block nil
      (multiple-value-bind (fun warningsp failurep)
          (handler-case
              (maclina.compile:with-compilation-unit (:override t)
                (ccompile nil
                          `(lambda ()
                             (macrolet ((m () (funcall ,#'die))) (m)))))
            (error ()
              (5am:pass "Macro error escaped COMPILE")
              (return)))
        (declare (ignore fun))
        (5am:is-true warningsp "Macro error not reported as warning")
        (5am:is-true failurep "Macro error not reported as failure")))))

(5am:test macro-warning
  (flet ((w () (warn "uh")))
    (multiple-value-bind (fun warningsp failurep)
        (ccompile nil `(lambda ()
                         (macrolet ((m () (funcall ,#'w))) (m))))
      (declare (ignore fun))
      (5am:is-true warningsp "Macro warning not reported")
      (5am:is-true failurep "Macro warning not reported as failure"))
    (multiple-value-bind (fun warningsp failurep)
        (maclina.compile:with-compilation-unit (:override t)
          (ccompile nil `(lambda ()
                           (macrolet ((m () (funcall ,#'w))) (m)))))
      (declare (ignore fun))
      (5am:is-true warningsp "Macro warning not reported")
      (5am:is-true failurep "Macro warning not reported as failure"))))

(define-condition tasteless (style-warning) ())

(5am:test macro-style-warning
  (flet ((w () (warn 'tasteless)))
    (multiple-value-bind (fun warningsp failurep)
        (ccompile nil `(lambda ()
                         (macrolet ((m () (funcall ,#'w))) (m))))
      (declare (ignore fun))
      (5am:is-true warningsp "Macro style warning not reported")
      (5am:is-false failurep "Macro style warning reported as failure"))
    (multiple-value-bind (fun warningsp failurep)
        (maclina.compile:with-compilation-unit (:override t)
          (ccompile nil `(lambda ()
                           (macrolet ((m () (funcall ,#'w))) (m)))))
      (declare (ignore fun))
      (5am:is-true warningsp "Macro style warning not reported")
      (5am:is-false failurep "Macro style warning reported as failure"))))

;;; TODO: compiler macros, but they're a little more involved since we should
;;; probably encapsulate them and stuff.
