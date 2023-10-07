;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 14:41:16 2002
;;;; Contains: Tests of UNWIND-PROTECT

(in-package #:maclina.test)

(5am:def-suite unwind-protect :in data-and-control-flow)
(5am:in-suite unwind-protect)

(deftest unwind-protect.1
  (let ((x nil))
    (unwind-protect
        (s:push 1 x)
      (s:incf (car x))))
  (2))

(deftest unwind-protect.2
  (let ((x nil))
    (block foo
      (unwind-protect
          (progn (s:push 1 x) (return-from foo x))
        (s:incf (car x)))))
  (2))

(deftest unwind-protect.3
  (let ((x nil))
    (tagbody
      (unwind-protect
          (progn (s:push 1 x) (go done))
        (s:incf (car x)))
      done)
    x)
  (2))

(deftest unwind-protect.4
  (let ((x nil))
    (catch 'done
      (unwind-protect
          (progn (s:push 1 x) (throw 'done x))
        (s:incf (car x)))))
  (2))

#+(or) ; error, ignore-errors
(deftest unwind-protect.5
  (let ((x nil))
    (ignore-errors
      (unwind-protect
          (progn (push 1 x) (error "Boo!"))
        (s:incf (car x))))
    x)
  (2))

(deftest unwind-protect.6
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
        (unwind-protect (%f)
          (s:push 'a x))))
    x)
  (a))

(deftest unwind-protect.7
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
        (unwind-protect
            (unwind-protect (%f)
              (s:push 'b x))
          (s:push 'a x))))
    x)
  (a b))

(deftest unwind-protect.8
  (let ((x nil))
    (block done
      (unwind-protect
          (flet ((%f () (return-from done nil)))
            (unwind-protect
                (unwind-protect (%f)
                  (s:push 'b x))
              (s:push 'a x)))
        (s:push 'c x)))
    x)
  (c a b))

#+(or) ; handler-case
(deftest unwind-protect.9
  (let ((x nil))
    (handler-case
      (flet ((%f () (error 'type-error :datum 'foo :expected-type nil)))
        (unwind-protect (handler-case (%f))
          (push 'a x)))
      (type-error () x)))
  (a))

;;; No implicit tagbody
(deftest unwind-protect.10
  (block done
    (tagbody
     (unwind-protect
         'foo
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Executes all forms of the implicit progn
(deftest unwind-protect.11
  (let ((x nil) (y nil))
    (values
     (block nil
       (unwind-protect (return 'a)
         (s:setf y 'c)
         (s:setf x 'b)))
     x y))
  a b c)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest unwind-protect.12
  (macrolet
   ((%m (z) z))
   (unwind-protect (s:expand-in-current-env (%m :good)) :bad))
  :good)

(deftest unwind-protect.13
  (macrolet
   ((%m (z) z))
   (unwind-protect :good (s:expand-in-current-env (%m :bad))))
  :good)


