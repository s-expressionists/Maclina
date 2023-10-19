(in-package #:maclina.machine)

;;; Some basic functions for getting at parts of the VM state.

(defgeneric symbol-value (client environment symbol))
(defgeneric (setf symbol-value) (new client environment symbol))
(defgeneric boundp (client environment symbol))
(defgeneric makunbound (client environment symbol))
(defgeneric call-with-progv (client environment symbols values thunk))

(defmacro progv (client environment symbols values &body body)
  `(call-with-progv ,client ,environment ,symbols ,values
		    (lambda () ,@body)))

(defgeneric fboundp (client environment function-name))
(defgeneric fdefinition (client environment function-name))
(defgeneric (setf fdefinition) (new client environment function-name))
(defgeneric fmakunbound (client environment function-name))

;;; And getting at the Common Lisp evaluator constants.
;;; Most of these are pretty baked into the definition of the VM, but maybe a client
;;; has its own ideas. They're also kind of sketchy at high numbers - you could technically
;;; have 65535 parameters, but then there wouldn't be any indices left for local variables.
(defgeneric lambda-parameters-limit (client)
  (:method (client)
    (declare (ignore client))
    #.(1- (expt 2 16))))
(defgeneric call-arguments-limit (client)
  (:method (client)
    (declare (ignore client))
    #.(1- (expt 2 16))))
(defgeneric lambda-list-keywords (client)
  (:method (client)
    (declare (ignore client))
    '(&whole &optional &rest &body &key &allow-other-keys &aux &environment)))
(defgeneric multiple-values-limit (client))
