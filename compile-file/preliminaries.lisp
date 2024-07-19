(in-package #:maclina.compile-file)

;;; Compilation environment the file compiler evaluates and compiles code in.
(defvar *environment*)

;;; Magic number used to identify our FASLs.
(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

;;; Major and minor version numbers. These are up to two bytes each.
;;; The versioning encompasses both the FASL format itself as well as the
;;; bytecode in modules. Changes to bytecode should get a version bump too.
(defparameter *major-version* 0)
(defparameter *minor-version* 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debugging
;;;

(defvar *debug-compiler* nil)

(defmacro dbgprint (message &rest args)
  `(when *debug-compiler*
     (let ((*print-level* 2) (*print-length* 1) (*print-circle* t))
       (format *error-output* ,(concatenate 'string "~&; " message "~%")
               ,@args))))
