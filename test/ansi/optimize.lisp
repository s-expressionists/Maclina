;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 09:31:34 2005
;;;; Contains: Tests of the OPTIMIZE declaration

(in-package #:maclina.test)

(5am:def-suite optimize :in eval-and-compile)
(5am:in-suite optimize)

(deftest optimize.1
  (locally (declare (optimize)) nil)
  nil)

(deftest optimize.2
  (locally (declare (optimize speed)) nil)
  nil)

(deftest optimize.3
  (locally (declare (optimize space)) nil)
  nil)

(deftest optimize.4
  (locally (declare (optimize safety)) nil)
  nil)

(deftest optimize.5
  (locally (declare (optimize debug)) nil)
  nil)

(deftest optimize.6
  (locally (declare (optimize compilation-speed)) nil)
  nil)

(5am:test optimize.7
  (5am:is-false
   (loop for d in '(speed space safety debug compilation-speed)
         nconc (loop for n from 0 to 3
                     for form = `(locally (declare (optimize (,d ,n))) t)
                     for val = (ceval form)
                     unless (eql val t)
                       collect (list d n val)))))

(5am:test optimize.8
  (5am:is-false
   (loop for d in '(speed space safety debug compilation-speed)
         nconc (loop for n from 0 to 3
                     for form = `(lambda ()
                                   (declare (optimize (,d ,n)))
                                   t)
                     for val = (funcall (ccompile nil form))
                     unless (eql val t)
                       collect (list d n val)))))




