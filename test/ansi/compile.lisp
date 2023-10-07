;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 20:54:20 2002
;;;; Contains: Tests for COMPILE, COMPILED-FUNCTION-P, COMPILED-FUNCTION

(in-package #:maclina.test)

(5am:def-suite compile :in eval-and-compile)
(5am:in-suite compile)

#+(or)
(deftest compile.1
  (progn
    (fmakunbound 'compile.1-fn)
    (values
     (eval '(defun compile.1-fn (x) x))
     (compiled-function-p 'compile.1-fn)
     (let ((x (compile 'compile.1-fn)))
       (or (eqt x 'compile.1-fn)
           (notnot (compiled-function-p x))))
     (compiled-function-p 'compile.1-fn)
     (not (compiled-function-p #'compile.1-fn))
     (fmakunbound 'compile.1-fn)))
  compile.1-fn
  nil
  t
  nil
  nil
  compile.1-fn)

;;; COMPILE returns three values (function, warnings-p, failure-p)
#+(or)
(deftest compile.2
  (let* ((results (multiple-value-list
                   (compile nil '(lambda (x y) (cons y x)))))
         (fn (car results)))
    (values (length results)
            (funcall fn 'a 'b)
            (second results)
            (third results)))
  3
  (b . a)
  nil
  nil)

;;; Compile does not coalesce literal constants
(5am:test compile.3
  (5am:is-false
   (let ((x (list 'a 'b))
         (y (list 'a 'b)))
     (funcall (ccompile nil `(lambda () (eq ',x ',y)))))))

(5am:test compile.4
  (5am:is-false
   (let ((x (copy-seq "abc"))
         (y (copy-seq "abc")))
     (funcall (ccompile nil `(lambda () (eq ,x ,y)))))))

(5am:test compile.5
  (5am:is-true
   (let ((x (copy-seq "abc")))
     (funcall (ccompile nil `(lambda () (eq ,x ,x)))))))

(5am:test compile.6
  (5am:is-true
   (let ((x (copy-seq "abc")))
     (funcall (ccompile nil `(lambda () (eq ',x ',x)))))))

(5am:test compile.7
  (let ((x (copy-seq "abc")))
    (5am:is (eq x (funcall (ccompile nil `(lambda () ,x)))))))

(5am:test compile.8
  (let ((x (list 'a 'b)))
    (5am:is (eq x (funcall (ccompile nil `(lambda () ',x)))))))

#+(or)
(deftest compile.9
  (let ((i 0) a b)
    (values
     (funcall (compile (progn (setf a (incf i)) nil)
                       (progn (setf b (incf i)) '(lambda () 'z))))
     i a b))
  z 2 1 2)

;;; Error tests

#+(or)
(deftest compile.error.1
  (signals-error (compile) program-error)
  t)

#+(or)
(deftest compile.error.2
  (signals-error (compile nil '(lambda () nil) 'garbage)
                 program-error)
  t)
