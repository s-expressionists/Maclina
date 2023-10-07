;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May  6 06:48:48 2003
;;;; Contains: Tests of THE

(in-package #:maclina.test)

(5am:def-suite the :in eval-and-compile)
(5am:in-suite the)

(deftest the.1
  (the (values) (values)))

(deftest the.2
  (the (values) 'a)
  a)

#+(or)
(deftest the.3
  (check-predicate #'(lambda (e)
                       (let ((x (multiple-value-list (eval `(the (values) (quote ,e))))))
                         (and x (not (cdr x)) (eql (car x) e)))))
  nil)

#+(or)
(deftest the.4
  (check-predicate #'(lambda (e)
                       (let ((x (multiple-value-list (eval `(the ,(type-of e) (quote ,e))))))
                         (and x (not (cdr x)) (eql (car x) e)))))
  nil)

#+(or)
(deftest the.5
  (check-predicate #'(lambda (e)
                       (let ((x (multiple-value-list (eval `(the (values ,(type-of e)) (quote ,e))))))
                         (and x (not (cdr x)) (eql (car x) e)))))
  nil)

#+(or)
(deftest the.6
  (check-predicate #'(lambda (e)
                       (let ((x (multiple-value-list (eval `(the (values ,(type-of e) t) (quote ,e))))))
                         (and x (not (cdr x)) (eql (car x) e)))))
  nil)

#+(or)
(deftest the.7
  (check-predicate
   #'(lambda (e)
       (let ((x (multiple-value-list (eval `(the (values ,(type-of e))
                                              (values (quote ,e) :ignored))))))
         (and (eql (length x) 2)
              (eql (car x) e)
              (eql (cadr x) :ignored)))))
  nil)

#+(or)
(deftest the.8
  (check-predicate #'(lambda (e) (or (not (constantp e))
                                     (eql (eval `(the ,(type-of e) ,e)) e))))
  nil)

#+(or)
(deftest the.9
  (check-predicate #'(lambda (e) (or (not (constantp e))
                                     (eql (eval `(the ,(class-of e) ,e)) e))))
  nil)

#+(or)
(deftest the.10
  (check-predicate #'(lambda (e) (eql (eval `(the ,(class-of e) ',e)) e)))
  nil)

#+(or)
(deftest the.11
  (check-predicate
   #'(lambda (e)
       (let* ((type (type-of e))
              (x (multiple-value-list (eval `(the ,type (the ,type (quote ,e)))))))
         (and x (not (cdr x)) (eql (car x) e)))))
  nil)

#+(or)
(deftest the.12
  (let ((lexpr
         `(lambda ()
            (and
             ,@(loop for e in *mini-universe*
                     for type = (type-of e)
                     collect `(eqlt (quote ,e) (the ,type (quote ,e))))))))
    (funcall (compile nil lexpr)))
  t)

(deftest the.13
  (let ((x 0))
    (values
     (the (or symbol integer) (s:incf x))
     x))
  1 1)

(deftest the.14
  (the (values &rest t) (values 'a 'b))
  a b)

(deftest the.15
  (the (values &rest symbol) (values 'a 'b))
  a b)

(deftest the.16
  (the (values &rest null) (values)))

(deftest the.17
  (the (values symbol integer &rest null) (values 'a 1))
  a 1)

(deftest the.18
  (the (values symbol integer &rest t) (values 'a 1 'foo '(x y)))
  a 1 foo (x y))

#+(or)
(deftest the.19
  (let () (list (the (values) (eval '(values)))))
  (nil))

;;; This is from SBCL bug 261
#+(or)
(deftest the.20
  (let () (list (the (values &optional fixnum) (eval '(values)))))
  (nil))

#+(or)
(deftest the.21
  (let () (list (the (values &rest t) (eval '(values)))))
  (nil))

#+(or)
(deftest the.22
  (the (values symbol integer &rest t) (eval '(values 'a 1 'foo '(x y))))
  a 1 foo (x y))

#+(or)
(deftest the.23
  (multiple-value-list
   (the (values symbol integer &optional fixnum) (eval '(values 'a 1))))
  (a 1))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest the.24
  (macrolet
      ((%m (z) z))
    (the (integer 0 10) (s:expand-in-current-env (%m 4))))
  4)

(deftest the.25
  (macrolet
      ((%m (z) z))
    (the (values t t) (s:expand-in-current-env (%m (values 1 2)))))
  1 2)
