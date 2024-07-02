(defpackage #:maclina.machine
  (:use #:cl)
  (:shadow #:return #:throw #:symbol-value #:progv #:fdefinition #:nil #:eq
           #:set #:push #:pop)
  (:shadow #:disassemble)
  (:shadow #:boundp #:makunbound #:fboundp #:fmakunbound)
  (:shadow #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  ;; Additional opname exports are done below.
  (:export #:*client*)
  (:export #:bytecode-module #:make-bytecode-module
           #:bytecode-module-bytecode #:bytecode-module-literals)
  (:export #:bytecode-function #:make-bytecode-function
           #:bytecode-function-module #:bytecode-function-entry-pc
           #:bytecode-function-environment-size
           #:bytecode-function-locals-frame-size
           #:bytecode-function-name #:bytecode-function-lambda-list)
  (:export #:bytecode-closure #:make-bytecode-closure
           #:bytecode-closure-template #:bytecode-closure-env)
  (:export #:compute-instance-function)
  (:export #:link-function #:link-variable #:link-environment)
  (:export #:boundp #:makunbound #:symbol-value #:call-with-progv #:progv
	   #:fdefinition #:fmakunbound #:fboundp)
  (:export #:lambda-parameters-limit #:call-arguments-limit
           #:lambda-list-keywords #:multiple-values-limit)
  (:export #:disassemble #:display-instruction))

;;;; Definition of the virtual machine, used by both the compiler and the VM.

(in-package #:maclina.machine)

(defconstant +mask-arg+     #b011000)
(defconstant +constant-arg+ #b001000)
(defconstant +keys-arg+     #b011000)
(defconstant +label-arg+    #b010000)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-arg (val)
    (logior +constant-arg+ val))
  
  (defun label-arg (val)
    (logior +label-arg+ val))
  
  (defun keys-arg (val)
    (logior +keys-arg+ val)))

(defun constant-arg-p (val)
  (= (logand +mask-arg+ val) +constant-arg+))

(defun label-arg-p (val)
  (= (logand +mask-arg+ val) +label-arg+))

(defun keys-arg-p (val)
  (= (logand +mask-arg+ val) +keys-arg+))

;;; *full-codes* contains descriptions of the instructions in the following format:
;;; (name opcode (args...) (long-args...))
;;; the name is a symbol.
;;; the args and long args are encoded as a number of bytes from 1 to 3, LOGIOR'd
;;; with the constant, label, and keys code that is appropriate, if any.

(macrolet ((defops (&rest ops)
             (let (rev-fullcodes
                   rev-codes
                   rev-defconstants)
               (dolist (op ops)
                 (destructuring-bind (name code &optional argument-codes long-argument-codes)
                     op
                   (let ((arguments (mapcar #'eval argument-codes))
                         (long-arguments (mapcar #'eval long-argument-codes)))
                     (cl:push (list name code arguments long-arguments)
                              rev-fullcodes))
                   (cl:push name rev-codes)
                   (cl:push `(defconstant ,name ,code) rev-defconstants)))
               `(progn
                  (defparameter *full-codes* ',(reverse rev-fullcodes))
                  (defparameter *codes* ',(reverse rev-codes))
                  (export '(,@rev-codes))
                  ,@rev-defconstants))))
  (defops
    (ref 0 (1) (2))
    (const 1 ((constant-arg 1)) ((constant-arg 2)))
    (closure 2 (1) (2))
    (call 3 (1) (2))
    (call-receive-one 4 (1) (2))
    (call-receive-fixed 5 (1 1) (2 2))
    (bind 6 (1 1) (2 2))
    (set 7 (1) (2))
    (make-cell 8)
    (cell-ref 9)
    (cell-set 10)
    (make-closure 11 ((constant-arg 1)) ((constant-arg 2)))
    (make-uninitialized-closure 12 ((constant-arg 1)) ((constant-arg 2)))
    (initialize-closure 13 (1) (2))
    (return 14)
    (bind-required-args 15 (1) (2))
    (bind-optional-args 16 (1 1) (2 2))
    (listify-rest-args 17 (1) (2))
    (vaslistify-rest-args 18 (1))
    (parse-key-args 19 (1 1 (keys-arg 1) 1) (2 2 (keys-arg 2) 2))
    (jump-8 20 ((label-arg 1)))
    (jump-16 21 ((label-arg 2)))
    (jump-24 22 ((label-arg 3)))
    (jump-if-8 23 ((label-arg 1)))
    (jump-if-16 24 ((label-arg 2)))
    (jump-if-24 25 ((label-arg 3)))
    (jump-if-supplied-8 26 (1 (label-arg 1)) (2 (label-arg 1)))
    (jump-if-supplied-16 27 (1 (label-arg 2)) (2 (label-arg 2)))
    (check-arg-count-<= 28 (1) (2))
    (check-arg-count->= 29 (1) (2))
    (check-arg-count-= 30 (1) (2))
    (push-values 31)
    (append-values 32)
    (pop-values 33)
    (mv-call 34)
    (mv-call-receive-one 35)
    (mv-call-receive-fixed 36 (1) (2))
    (save-sp 37 (1))
    (restore-sp 38 (1))
    (entry 39 (1))
    (exit-8 40 ((label-arg 1)))
    (exit-16 41 ((label-arg 2)))
    (exit-24 42 ((label-arg 3)))
    (entry-close 43)
    (catch-8 44 ((label-arg 1)))
    (catch-16 45 ((label-arg 2)))
    (throw 46)
    (catch-close 47)
    (special-bind 48 ((constant-arg 1)) ((constant-arg 2)))
    (symbol-value 49 ((constant-arg 1)) ((constant-arg 2)))
    (symbol-value-set 50 ((constant-arg 1)) ((constant-arg 2)))
    (unbind 51)
    (progv 52 ((constant-arg 1)) ((constant-arg 2)))
    (fdefinition 53 ((constant-arg 1)) ((constant-arg 2)))
    (nil 54)
    (eq 55)
    (push 56)
    (pop 57)
    (dup 58)
    (fdesignator 59)
    (called-fdefinition 60 ((constant-arg 1)) ((constant-arg 2)))
    (protect 61 ((constant-arg 1)) ((constant-arg 2)))
    (cleanup 62)
    (encell 63 (1) (2))
    (long 255)))
