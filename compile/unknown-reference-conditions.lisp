(in-package #:maclina.compile)

(define-condition unknown-reference (condition)
  ((%name :initarg :name :reader name))
  (:documentation "Parent type of conditions signaled by the compiler to indicate an unknown name that may be defined later."))

;;; Signal an UNKNOWN-REFERENCE condition in such a way that if we're in a
;;; compilation unit, it will be suppressed until the unit ends.
;;; Like WARN, returns NIL.
(defun warn-unknown (datum &rest arguments)
  (restart-case (apply #'warn datum arguments) (continue ())))

(define-condition unknown-variable (unknown-reference warning)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown variable ~s: treating as special"
                     (name condition))))
  (:documentation "Condition signaled when the compiler encounters an unknown variable."))

(define-condition unknown-function (unknown-reference style-warning)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown operator ~s: treating as global function"
                     (name condition))))
  (:documentation "Condition signaled when the compiler encounters an unknown operator."))

(define-condition unknown-reference-resolution (condition)
  ((%name :initarg :name :reader name))
  (:documentation "Parent type for conditions that can be SIGNALed at compile time to indicate to an ongoing compilation unit that a previously unknown name is now known."))

;;; Iff RESOLUTION is a resolution of REFERENCE, returns true.
;;; May have other side effects (see ASSUMED-FUNCTION-NOW-MACRO below).
(defgeneric resolve-reference (resolution reference)
  (:method (resolution reference)
    (declare (ignore resolution reference))
    nil))

;;; Can be SIGNALed by e.g. DEFUN's expansion at compile time.
(define-condition resolve-function (unknown-reference-resolution) ()
  (:documentation "Condition that can be SIGNALed to indicate to a compilation unit that a new function has been defined, and that previously unknown references to an operator by that name can now be resolved."))
(defmethod resolve-reference ((r1 resolve-function) (r2 unknown-function))
  (equal (name r1) (name r2)))

(define-condition assumed-function-now-macro (warning)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "Uses of newly noted macro ~s were previously assumed to be function calls"
                     (name condition))))
  (:documentation "This warning is signaled when a macro has been newly defined, but the compilation unit has previously encountered uses of the name that it assumed to refer to a function."))

;;; Can be SIGNALed by e.g. DEFMACRO's expansion at compile time.
(define-condition resolve-macro (unknown-reference-resolution) ()
  (:documentation "Condition that can be SIGNALed to indicate to a compilation unit that a new macro has been defined, and that previously unknown references to an operator by that name can now be resolved."))
(defmethod resolve-reference ((r1 resolve-macro) (r2 unknown-function))
  (when (equal (name r1) (name r2))
    (warn 'assumed-function-now-macro :name (name r1))
    t))
