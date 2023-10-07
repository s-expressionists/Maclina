;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Oct  9 19:41:24 2002
;;;; Contains: Tests of MACROLET

(in-package #:maclina.test)

(5am:def-suite macrolet :in data-and-control-flow)
(5am:in-suite macrolet)

;;;; WARNING: Do not use backquote in any code that will be passed
;;;; to Maclina. It is read implementation-specifically, and these
;;;; tests are designed to run in limited first-class environments.

(deftest macrolet.1
  (let ((z (list 3 4)))
    (macrolet ((%m (x) (list 'car x)))
      (let ((y (list 1 2)))
        (values (%m y) (%m z)))))
  1 3)

(deftest macrolet.2
  (let ((z (list 3 4)))
    (macrolet ((%m (x) (list 'car x)))
      (let ((y (list 1 2)))
        (values
         (s:setf (%m y) 6)
         (s:setf (%m z) 'a)
         y z))))
  6 a (6 2) (a 4))

;;; Inner definitions shadow outer ones
(deftest macrolet.3
  (macrolet ((%m (w) (list 'cadr w)))
    (let ((z (list 3 4)))
      (macrolet ((%m (x) (list 'car x)))
        (let ((y (list 1 2)))
          (values
           (%m y) (%m z)
           (s:setf (%m y) 6)
           (s:setf (%m z) 'a)
           y z)))))
  1 3 6 a (6 2) (a 4))

;;; &whole parameter
(deftest macrolet.4
  (let ((x nil))
    (macrolet ((%m (&whole w arg)
                 (list 'progn (list 'setq 'x (list 'quote w)) arg)
                 #+(or)
                 `(progn (setq x (quote ,w))
                         ,arg)))
      (values (%m 1) x)))
  1 (%m 1))

;;; &whole parameter (nested, destructuring; see section 3.4.4)
(deftest macrolet.5
  (let ((x nil))
    (macrolet ((%m ((&whole w arg))
                 (list 'progn (list 'setq 'x (list 'quote w)) arg)
                 #+(or)
                 `(progn (setq x (quote ,w))
                         ,arg)))
      (values (%m (1)) x)))
  1 (1))

;;; key parameter
(deftest macrolet.6
  (let ((x nil))
    (macrolet ((%m (&key (a 'xxx) b)
                 (list 'setq 'x (list 'quote a))
                 #+(or)
                 `(setq x (quote ,a))))
      (values (%m :a foo) x
              (%m :b bar) x)))
  foo foo xxx xxx)

;;; nested key parameters
(deftest macrolet.7
  (let ((x nil))
    (macrolet ((%m ((&key a b))
                 (list 'setq 'x (list 'quote a))
                 #+(or)
                 `(setq x (quote ,a))))
      (values (%m (:a foo)) x
              (%m (:b bar)) x)))
  foo foo nil nil)

;;; nested key parameters
(deftest macrolet.8
  (let ((x nil))
    (macrolet ((%m ((&key (a 10) b))
                 (list 'setq 'x (list 'quote a))
                 #+(or)
                 `(setq x (quote ,a))))
      (values (%m (:a foo)) x
              (%m (:b bar)) x)))
  foo foo 10 10)

;;; keyword parameter with supplied-p parameter
(deftest macrolet.9
  (let ((x nil))
    (macrolet ((%m (&key (a 'xxx a-p) b)
                 (list 'setq 'x (list 'quote (list a (not (not a-p)))))
                 #+(or)
                 `(setq x (quote ,(list a (not (not a-p)))))))
      (values (%m :a foo) x
              (%m :b bar) x)))
  (foo t) (foo t) (xxx nil) (xxx nil))


;;; rest parameter
(deftest macrolet.10
  (let ((x nil))
    (macrolet ((%m (b &rest a)
                 (list 'setq 'x (list 'quote a))
                 #+(or)
                 `(setq x (quote ,a))))
      (values (%m a1 a2) x)))
  (a2) (a2))

;;; rest parameter w. destructuring
(deftest macrolet.11
  (let ((x nil))
    (macrolet ((%m ((b &rest a))
                 (list 'setq 'x (list 'quote a))
                 #+(or)
                 `(setq x (quote ,a))))
      (values (%m (a1 a2)) x)))
  (a2) (a2))

;;; rest parameter w. whole
(deftest macrolet.12
  (let ((x nil))
    (macrolet ((%m (&whole w b &rest a)
                 (list 'setq 'x (list 'quote (list a w)))
                 #+(or)
                 `(setq x (quote ,(list a w)))))
      (values (%m a1 a2) x)))
  ((a2) (%m a1 a2))
  ((a2) (%m a1 a2)))

;;; Interaction with symbol-macrolet

(deftest macrolet.13
  (symbol-macrolet ((a b))
    (macrolet ((foo (x &environment env)
                 (let ((y (s:macroexpand x env)))
                   (if (eq y 'a) 1 2))))
      (foo a)))
  2)

(deftest macrolet.14
  (symbol-macrolet ((a b))
    (macrolet ((foo (x &environment env)
                 (let ((y (s:macroexpand-1 x env)))
                   (if (eq y 'a) 1 2))))
      (foo a)))
  2)

(deftest macrolet.15
  (macrolet ((nil () ''a))
    (nil))
  a)

#+(or)
(deftest macrolet.16
  (loop for s in *cl-non-function-macro-special-operator-symbols*
        for form = `(ignore-errors (macrolet ((,s () ''a)) (,s)))
        unless (eq (eval form) 'a)
        collect s)
  nil)

(deftest macrolet.17
  (macrolet ((%m (&key (a t)) (list 'quote a)))
    (%m :a nil))
  nil)

(deftest macrolet.18
  (macrolet ((%m (&key (a t a-p))
               (list 'quote (list a (s:notnot a-p)))
               #+(or) `(quote (,a ,(s:notnot a-p)))))
    (%m :a nil))
  (nil t))

(deftest macrolet.19
  (macrolet ((%m (x &optional y)
               (list 'quote (list x y))
               #+(or) `(quote (,x ,y))))
    (values (%m 1) (%m 2 3)))
  (1 nil)
  (2 3))

(deftest macrolet.20
  (macrolet ((%m (x &optional (y 'a))
               (list 'quote (list x y))
               #+(or) `(quote (,x ,y))))
    (values (%m 1) (%m 2 3)))
  (1 a)
  (2 3))

;;; Note -- the supplied-p parameter in a macrolet &optional
;;; is required to be T (not just true) if the parameter is present.
;;; See section 3.4.4.1.2
(deftest macrolet.21
  (macrolet ((%m (x &optional (y 'a y-p))
               (list 'quote (list x y y-p))
               #+(or) `(quote (,x ,y ,y-p))))
    (values (%m 1) (%m 2 3)))
  (1 a nil)
  (2 3 t))

(deftest macrolet.22
  (macrolet ((%m (x &optional ((y z) '(2 3)))
               (list 'quote (list x y z))
               #+(or) `(quote (,x ,y ,z))))
    (values
     (%m a)
     (%m a (b c))))
  (a 2 3)
  (a b c))

(deftest macrolet.22a
  (macrolet ((%m (x &optional ((y z) '(2 3) y-z-p))
               (list 'quote (list x y z y-z-p))
               #+(or)`(quote (,x ,y ,z ,y-z-p))))
    (values
     (%m a)
     (%m a (b c))))
  (a 2 3 nil)
  (a b c t))

(deftest macrolet.23
  (macrolet ((%m (&rest y) (list 'quote y) #+(or) `(quote ,y)))
    (%m 1 2 3))
  (1 2 3))

;;; According to 3.4.4.1.2, the entity following &rest is
;;; 'a destructuring pattern that matches the rest of the list.'

(5am:test macrolet.24
  (5am:skip "Ecclesia considers this invalid syntax")
  #+(or)
  (is-values-eval (macrolet ((%m (&rest (x y z))
                               (list 'quote (list x y z))
                               #+(or) `(quote (,x ,y ,z))))
                    (%m 1 2 3))
                  (1 2 3)))

(5am:test macrolet.25
  (5am:skip "Ecclesia considers this invalid syntax")
  #+(or)
  (is-values-eval (macrolet ((%m (&body (x y z))
                               (list 'quote (list x y z))
                               #+(or) `(quote (,x ,y ,z))))
                    (%m 1 2 3))
                  (1 2 3)))

;;; More key parameters

(deftest macrolet.26
  (macrolet ((%m (&key ((:a b))) (list 'quote b)))
    (values (%m)
            (%m :a x)))
  nil
  x)

(deftest macrolet.27
  (macrolet ((%m (&key ((:a (b c)))) (list 'quote (list c b))))
    (%m :a (1 2)))
  (2 1))

(deftest macrolet.28
  (macrolet ((%m (&key ((:a (b c)) '(3 4)))
               (list 'quote (list c b))))
    (values (%m :a (1 2))
            (%m :a (1 2) :a (10 11))
            (%m)))
  (2 1)
  (2 1)
  (4 3))

(deftest macrolet.29
  (macrolet ((%m (&key a (b a)) (list 'quote (list a b))))
    (values (%m)
            (%m :a 1)
            (%m :b 2)
            (%m :a 3 :b 4)
            (%m :b 5 :a 6)
            (%m :a 7 :a 8)
            (%m :a 9 :b nil)
            (%m :a 10 :b nil :b 11)))
  (nil nil)
  (1 1)
  (nil 2)
  (3 4)
  (6 5)
  (7 7)
  (9 nil)
  (10 nil))

(deftest macrolet.30
  (macrolet ((%m ((&key a) &key (b a))
               (list 'quote (list a b))
               #+(or) `(quote (,a ,b))))
    (values (%m ())
            (%m (:a 1))
            (%m () :b 2)
            (%m (:a 3) :b 4)
            (%m (:a 7 :a 8))
            (%m (:a 9) :b nil)
            (%m (:a 10) :b nil :b 11)))
  (nil nil)
  (1 1)
  (nil 2)
  (3 4)
  (7 7)
  (9 nil)
  (10 nil))

(deftest macrolet.31
  (macrolet ((%m (&key ((:a (b c)) '(3 4) a-p))
               (list 'quote (list (s:notnot a-p) c b))
               #+(or) `(quote (,(s:notnot a-p) ,c ,b))))
    (values (%m :a (1 2))
            (%m :a (1 2) :a (10 11))
            (%m)))
  (t 2 1)
  (t 2 1)
  (nil 4 3))

;;; Allow-other-keys tests

(deftest macrolet.32
  (macrolet ((%m (&key a b c) (list 'quote (list a b c))))
    (values
     (%m :allow-other-keys nil)
     (%m :a 1 :allow-other-keys nil)
     (%m :allow-other-keys t)
     (%m :allow-other-keys t :allow-other-keys nil :foo t)
     (%m :allow-other-keys t :c 1 :b 2 :a 3)
     (%m :allow-other-keys nil :c 1 :b 2 :a 3)))
  (nil nil nil)
  (1 nil nil)
  (nil nil nil)
  (nil nil nil)
  (3 2 1)
  (3 2 1))

(deftest macrolet.33
  (macrolet ((%m (&key allow-other-keys)
               (list 'quote allow-other-keys)))
    (values
     (%m)
     (%m :allow-other-keys nil)
     (%m :allow-other-keys t :foo t)))
  nil
  nil
  t)

(deftest macrolet.34
  (macrolet ((%m (&key &allow-other-keys) :good))
    (values
     (%m)
     (%m :foo t)
     (%m :allow-other-keys nil :foo t)))
  :good
  :good
  :good)

(deftest macrolet.35
  (macrolet ((%m (&key a b &allow-other-keys)
               (list 'quote (list a b))))
    (values
     (%m :a 1)
     (%m :foo t :b 2)
     (%m :allow-other-keys nil :a 1 :foo t :b 2)))
  (1 nil)
  (nil 2)
  (1 2))

;;; &whole is followed by a destructuring pattern (see 3.4.4.1.2)
(5am:test macrolet.36
  (5am:skip "Ecclesia considers this invalid syntax")
  #+(or)
  (is-values-eval (macrolet ((%m (&whole (m a b) c d)
                               (list 'quote (list m a b c d))
                               #+(or) `(quote (,m ,a ,b ,c ,d))))
                    (%m 1 2))
                  (%m 1 2 1 2)))

;;; Macro names are shadowed by local functions

(deftest macrolet.37
  (macrolet ((%f () :bad))
    (flet ((%f () :good))
      (%f)))
  :good)


;;; The &environment parameter is bound first

(deftest macrolet.38
  (macrolet ((foo () 1))
    (macrolet ((%f (&optional (x (s:macroexpand '(foo) env)) &environment env)
                 x))
      (%f)))
  1)

;;; Test for bug that showed up in sbcl

(5am:test macrolet.39
  (5am:skip "Ecclesia considers this invalid syntax (probably incorrectly)")
  #+(or)
  (is-values-eval (macrolet ((%m (()) :good)) (%m ()))
                  :good))

;;; Test that macrolets accept declarations

(deftest macrolet.40
  (macrolet ((%x () t))
    (declare (optimize)))
  nil)

(deftest macrolet.41
  (macrolet ((%x () t))
    (declare (optimize))
    (declare (notinline identity)))
  nil)

(deftest macrolet.42
  (macrolet ((%x () t))
    (declare (optimize))
    (%x))
  t)

(deftest macrolet.43
  (let ((*x-in-macrolet.43* nil))
    (declare (special *x-in-macrolet.43*))
    (let ((*f* #'(lambda () *x-in-macrolet.43*)))
      (declare (special *f*))
      (s:eval '(macrolet ((%m (*x-in-macrolet.43*)
                           (declare (special *f*))
                           (funcall *f*)))
                (%m t)))))
  nil)

(deftest macrolet.44
  (let ((*x-in-macrolet.44* nil))
    (declare (special *x-in-macrolet.44*))
    (let ((*f* #'(lambda () *x-in-macrolet.44*)))
      (declare (special *f*))
      (s:eval '(macrolet ((%m (*x-in-macrolet.44*)
                            (declare (special *f* *x-in-macrolet.44*))
                            (funcall *f*)))
                 (%m t)))))
  t)

(deftest macrolet.45
  (let ((*x-in-macrolet.45* nil))
    (declare (special *x-in-macrolet.45*))
    (let ((*f* #'(lambda () *x-in-macrolet.45*)))
      (declare (special *f*))
      (s:eval '(macrolet ((%m ((*x-in-macrolet.45*))
                            (declare (special *f* *x-in-macrolet.45*))
                            (funcall *f*)))
                 (%m (t))))))
  t)

;;; Macros are expanded in the appropriate environment

(deftest macrolet.46
  (macrolet ((%m (z) z))
    (macrolet () (s:expand-in-current-env (%m :good))))
  :good)

;;; Free declarations in macrolet

(deftest macrolet.47
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (macrolet () (declare (special x)) x)))
  :good)

(deftest macrolet.48
  (let ((x :good))
    (let ((y :bad))
      (macrolet () (declare (ignore y)) x)))
  :good)

(deftest macrolet.49
  (let ((x :good))
    (let ((y :bad))
      (macrolet () (declare (ignorable y)) x)))
  :good)


;;; TODO: more special declarations for other macrolet arguments


;;; macrolet shadows global macro, function and compiler-macro
;;; definitions

#+(or)
(defmacro macrolet.50 () :bad)

#+(or)
(deftest macrolet.50
   (macrolet ((macrolet.50 () :good))
     (macrolet.50))
   :good)

#+(or)
(defun macrolet.51 () :bad)

#+(or)
(deftest macrolet.51
   (macrolet ((macrolet.51 () :good))
      (macrolet.51))
   :good)

#+(or)
(define-compiler-macro macrolet.52 (&whole form)
  :bad)

#+(or)
(deftest macrolet.52
   (macrolet ((macrolet.52 () :good))
      (macrolet.52))
   :good)
