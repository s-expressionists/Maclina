(in-package #:maclina.machine)

;;; Retrieve information from an environment based on a name.
;;; These are used both by the evaluator/compiler (compile.lisp)
;;; and the loader (loadltv.lisp).

;;; Given a function name, return a cell that the VM
;;; FDEFINITION instruction can get an actual function from.
(defgeneric link-function (client environment function-name)
  ;; default: use the name itself as a cell.
  ;; VM FDEFINITION will just use CL:FDEFINITION.
  (:method (client env fname)
    (declare (ignore client env))
    fname))

;;; Given a variable name, return a cell that the VM
;;; SPECIAL-BIND etc. instructions can use to access the
;;; variable's bindings and values.
(defgeneric link-variable (client environment variable-name)
  ;; default: use the name itself as a cell.
  (:method (client env vname)
    (declare (ignore client env))
    vname))

;;; Return an environment that the VM PROGV and FDESIGNATOR
;;; instructions can use to perform lookups.
(defgeneric link-environment (client environment)
  ;; default: just return the given environment.
  ;; It seems unlikely any client will need to customize this,
  ;; but it's included for completeness.
  (:method (client env)
    (declare (ignore client))
    env))
