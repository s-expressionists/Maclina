(in-package #:maclina.test)

(5am:def-suite unknown-reference-conditions :in compiler-conditions)
(5am:in-suite unknown-reference-conditions)

(5am:test unknown-variable-read
  (let* ((var (make-symbol "UNKNOWN-VARIABLE"))
         (form `(progn ,var))
         (maclina.compile:*source-locations* (make-hash-table))
         warning)
    (setf (gethash form maclina.compile:*source-locations*) 37)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-variable
               (lambda (w)
                 (setq warning w)
                 ;; Make sure we still fail with a MUFFLE-WARNING.
                 (muffle-warning w))))
          (ccompile nil `(lambda () ,form)))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-true failurep "COMPILE did not fail")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql var (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (5am:is (eql 37 (maclina.compile:source warning))
              "COMPILE's warning did not have a source location")
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
  (let* ((var (make-symbol "UNKNOWN-VARIABLE"))
         (form `(setq ,var val))
         (maclina.compile:*source-locations* (make-hash-table))
         warning)
    (setf (gethash form maclina.compile:*source-locations*) 92)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-variable
               (lambda (w) (setq warning w) (muffle-warning w))))
          (ccompile nil `(lambda (val) ,form)))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-true failurep "COMPILE did not fail")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql var (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (5am:is (eql 92 (maclina.compile:source warning))
              "COMPILE's warning did not have a source location")
      (handler-case
          (let ((val (ceval `(let ((,var 71))
                               (declare (special ,var))
                               (funcall ,fun 83)
                               ,var))))
            (5am:is (eql 83 val)))
        (unbound-variable ()
          (5am:fail "Unknown variable was not assumed to be special"))))))

(5am:test unknown-function
  (let* ((fname (make-symbol "UNKNOWN-FUNCTION")) warning
         (form `(,fname))
         (maclina.compile:*source-locations* (make-hash-table)))
    (setf (gethash form maclina.compile:*source-locations*) 81)
    (multiple-value-bind (fun warningsp failurep)
        (handler-bind
            ((maclina.compile:unknown-function
               (lambda (w) (setq warning w) (muffle-warning w))))
          (ccompile nil `(lambda () ,form)))
      (5am:is-true warningsp "COMPILE did not report a warning")
      (5am:is-false failurep "COMPILE failed")
      (5am:is-true warning "COMPILE did not signal a warning")
      (5am:is (eql fname (maclina.compile:name warning))
              "COMPILE's warning did not have the correct name")
      (5am:is (eql 81 (maclina.compile:source warning))
              "COMPILE's warning did not have a source location")
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
  (let* ((mname (make-symbol "UNKNOWN-MACRO"))
         (form `(,mname))
         (maclina.compile:*source-locations* (make-hash-table))
         warning)
    (setf (gethash form maclina.compile:*source-locations*) 20)
    (handler-bind
        ((warning (lambda (w) (setq warning w) (muffle-warning w))))
      (maclina.compile:with-compilation-unit (:override t)
        (multiple-value-bind (_ warningsp failurep)
            (ccompile nil `(lambda () ,form))
          (declare (ignore _))
          (5am:is-false warningsp "COMPILE reported warning too early")
          (5am:is-false failurep "COMPILE reported failure too early"))
        (signal 'maclina.compile:resolve-macro :name mname)))
    (5am:is-true (typep warning 'maclina.compile:assumed-function-now-macro)
                 "WITH-COMPILATION-UNIT did not signal an ~s warning"
                 'maclina.compile:assumed-function-now-macro)
    (5am:is (eql 20 (maclina.compile:source warning))
            "WITH-COMPILATION-UNIT's warning did not have a source location")))
