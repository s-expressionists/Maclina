(in-package #:maclina.test)

(5am:def-suite unknown-reference-conditions :in compiler-conditions)
(5am:in-suite unknown-reference-conditions)

(5am:test unknown-variable-read
  (let ((var (make-symbol "UNKNOWN-VARIABLE"))
        warning)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-variable
               (lambda (w)
                 (setq warning w)
                 ;; Make sure we still fail with a MUFFLE-WARNING.
                 (muffle-warning w))))
          (ccompile nil `(lambda () ,var)))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-true failurep "COMPILE did not fail")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql var (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (5am:signals unbound-variable (funcall fun))
      ;; Now make sure it was assumed to be a special variable.
      (handler-case
          (let ((val (ceval `(let ((,var 71))
                               (declare (special ,var))
                               (funcall ,fun)))))
            (5am:is (eql 71 val)))
        (unbound-variable ()
          (5am:fail "Unknown variable was not assumed to be special"))))))

(5am:test unknown-variable-write
  (let ((var (make-symbol "UNKNOWN-VARIABLE"))
        warning)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-variable
               (lambda (w) (setq warning w) (muffle-warning w))))
          (ccompile nil `(lambda (val) (setq ,var val))))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-true failurep "COMPILE did not fail")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql var (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (handler-case
          (let ((val (ceval `(let ((,var 71))
                               (declare (special ,var))
                               (funcall ,fun 83)
                               ,var))))
            (5am:is (eql 83 val)))
        (unbound-variable ()
          (5am:fail "Unknown variable was not assumed to be special"))))))

(5am:test unknown-function
  (let ((fname (make-symbol "UNKNOWN-FUNCTION")) warning)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-function
               (lambda (w) (setq warning w) (muffle-warning w))))
          (ccompile nil `(lambda () (,fname))))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-false failurep "COMPILE failed")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql fname (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (5am:signals undefined-function (funcall fun)))))

(5am:test resolve-unknown-function
  (let ((warning nil))
    (handler-bind
        ((warning (lambda (w) (setq warning w))))
      (maclina.compile:with-compilation-unit (:override t)
        (let ((fname (make-symbol "UNKNOWN-FUNCTION")))
          (multiple-value-bind (_ warningsp failurep)
              (ccompile nil `(lambda () (,fname)))
            (declare (ignore _))
            (5am:is-false warningsp "COMPILE reported warning too early")
            (5am:is-false failurep "COMPILE reported failure too early"))
          (signal 'maclina.compile:resolve-function :name fname))))
    (5am:is-false warning
                  "Unknown function resolution failed: ~s signaled" warning)))

(5am:test resolve-unknown-macro
  (5am:signals
      maclina.compile:assumed-function-now-macro
      (maclina.compile:with-compilation-unit (:override t)
        (let ((mname (make-symbol "UNKNOWN-MACRO")))
          (multiple-value-bind (_ warningsp failurep)
              (ccompile nil `(lambda () (,mname)))
            (declare (ignore _))
            (5am:is-false warningsp "COMPILE reported warning too early")
            (5am:is-false failurep "COMPILE reported failure too early"))
          (signal 'maclina.compile:resolve-macro :name mname)))))
