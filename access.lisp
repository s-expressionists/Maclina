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
