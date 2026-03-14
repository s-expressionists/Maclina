(in-package #:maclina.compile-file)

;;; Compilation environment the file compiler evaluates and compiles code in.
(defvar *environment*)

;;; Magic number used to identify our FASLs.
(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

;;; Major and minor version numbers. These are up to two bytes each.
;;; The versioning encompasses both the FASL format itself as well as the
;;; bytecode in modules. Changes to bytecode should get a version bump too.
(defparameter *major-version* 0)
(defparameter *minor-version* 16)

;;; Like *compile-file-pathname*, but can be controlled by the user. This
;;; pathname is by default used in source info, so setting this pathname controls
;;; what path is used for source info reported and saved by the compiler.
;;; (*compile-file-pathname* also cannot be used directly because the effect of
;;;  binding or setting it is undefined.)
(defparameter *source-pathname* nil)

;;; A representation of the FASL we're compiling into; see cmpltv
(defvar *coalescence*)
;;; Another for any CFASL
(defvar *cfasl-coalescence*)

;;; The client to use for compile time evaluation and CFASLs.
;;; By default it's just the client provided to compile-file, which is in turn
;;; just maclina.machine:*client*.
(defvar *evaluation-client*)

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
