;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr  9 08:25:25 2005
;;;; Contains: Tests of COMPILE-FILE

(in-package #:maclina.test)

(5am:def-suite compile-file :in system-construction)
(5am:in-suite compile-file)

(defun compile-file-test (file funname &rest args
			  &key
			    (environment *environment*)
                            expect-warnings
                            expect-style-warnings output-file
                            (print nil print-p)
                            (verbose nil verbose-p)
                            (*compile-print* nil)
                            (*compile-verbose* nil)
                            external-format)
  (declare (ignorable external-format))
  (let* ((target-pathname (or output-file
                              (maclina.compile-file:compile-file-pathname file)))
         (actual-warnings-p nil)
         (actual-style-warnings-p nil)
	 (run-time-environment
	   (maclina.compile:run-time-environment m:*client* environment)))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (m:fmakunbound m:*client* run-time-environment funname)
    (let* ((str (make-array '(0)
			    :element-type 'character :adjustable t :fill-pointer 0))
           (vals (multiple-value-list
                  (handler-bind
                      ((style-warning #'(lambda (c)
					  (declare (ignore c))
					  (setf actual-style-warnings-p t)
					  nil))
                       ((or error warning)
			 #'(lambda (c)
                             (unless (typep c 'style-warning)
                               (setf actual-warnings-p t))
                             nil)))
                    (with-output-to-string
			(*standard-output* str)
                      (apply #'maclina.compile-file:compile-file file
			     :environment environment :allow-other-keys t args))))))
      (5am:is (= 3 (length vals)))
      (destructuring-bind (output-truename warnings-p failure-p) vals
        (print (namestring (truename target-pathname)))
        (print (namestring output-truename))
	(5am:is-true (or print verbose
			 (and (not print-p) *compile-print*)
			 (and (not verbose-p) *compile-verbose*)
			 (string= str "")))
	(5am:is-true (or (and verbose-p (not verbose))
			 (and (not verbose-p) (not *compile-verbose*))
			 (position #\; str))
		     "Verbose output was specified but none is apparent")
	(5am:is-true (if actual-warnings-p failure-p t))
        (5am:is-true (if actual-style-warnings-p warnings-p t))
        (5am:is-true (if expect-warnings failure-p t))
	(5am:is-true (if expect-style-warnings warnings-p t))
        (5am:is-true (or (null output-truename) (pathnamep output-truename)))
	(5am:is (equalp (namestring (truename target-pathname))
			(namestring output-truename)))
	(5am:is-false (m:fboundp m:*client* run-time-environment funname))
        (maclina.load:load-bytecode output-truename
				:environment run-time-environment)
	(5am:is-true (m:fboundp m:*client* run-time-environment funname))
        (5am:is-false
	 (funcall (m:fdefinition m:*client* run-time-environment funname)))))))

(5am:test compile-file.1
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1))
#+(or) ; warn
(5am:test compile-file.2
  (compile-file-test "compile-file-test-file-2.lisp"
                     'compile-file-test-fun.2
                     :expect-style-warnings t)
  t nil)
#+(or) ; warn
(deftest compile-file.2a
  (compile-file-test "compile-file-test-file-2a.lisp"
                     'compile-file-test-fun.2a
                     :expect-warnings t)
  t nil)

(5am:test compile-file.3
  (let ((*package* (find-package "MACLINA.TEST")))
    (compile-file-test "compile-file-test-file-3.lisp"
                       'compile-file-test-fun.3)))

(5am:test compile-file.4
  (let ((*package* (find-package "CL-USER")))
    (compile-file-test "compile-file-test-file-3.lisp"
                       'cl-user::compile-file-test-fun.3)))

(5am:test compile-file.5
  (compile-file-test #p"compile-file-test-file.lisp"
                     'compile-file-test-fun.1))

(5am:test compile-file.6
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :output-file "foo.fasl"))

(5am:test compile-file.6a
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :output-file "foo.ufsl"))

(5am:test compile-file.7
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :external-format :default))

(5am:test compile-file.8
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :output-file #p"foo.fasl"))

(5am:test compile-file.9
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :print t))

(5am:test compile-file.10
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :verbose t))

(5am:test compile-file.11
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :print nil))

(5am:test compile-file.12
    (compile-file-test "compile-file-test-file.lisp"
                       'compile-file-test-fun.1
                     :verbose nil))

;;; A file stream is a pathname designator
(5am:test compile-file.13
  (with-open-file (s "compile-file-test-file.lisp" :direction :input)
    (compile-file-test s 'compile-file-test-fun.1)))

(5am:test compile-file.14
  (let ((s (open "foo.fasl" :direction :output :if-exists :supersede
                 :if-does-not-exist :create)))
    (close s)
    (compile-file-test "compile-file-test-file.lisp"
                       'compile-file-test-fun.1
                       :output-file s)))

#+(or) ; gotta use eclector readtable
(5am:test compile-file.15
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\! (get-macro-character #\'))
    (compile-file-test "compile-file-test-file-4.lisp"
                       'compile-file-test-fun.4)) t foo)

;;; Tests for *compile-file-truename*, *compile-file-pathname*

#+(or) ; involves dumping pathnames, no good on SBCL
(5am:test compile-file.16
  (let* ((file #p"compile-file-test-file-5.lisp")
         (target-pathname (maclina.compile-file:compile-file-pathname file))
         (*compile-print* nil)
         (*compile-verbose* nil))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (maclina.compile-file:compile-file file)
    (load target-pathname)
    (5am:is (equalp (truename file) (funcall 'compile-file-test-fun.5)))
    (5am:is (equalp (pathname (merge-pathnames file))
		    (funcall 'compile-file-test-fun.5a)))))

;;; Add tests of logical pathnames
#+(or) ; gotta set up logical pathname host and such
(deftest compile-file.17
  (let ((file (logical-pathname "CLTEST:COMPILE-FILE-TEST-LP.LISP")))
    (with-open-file
     (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format s "(in-package :cl-test)~%(defun compile-file-test-lp.fun () nil)~%"))
    (compile-file-test file 'compile-file-test-lp.fun))
  t nil)
#+(or)
(deftest compile-file.18
  (let ((file (logical-pathname "CLTEST:COMPILE-FILE-TEST-LP.OUT")))
    (with-open-file
     (s file :direction :output :if-exists :supersede :if-does-not-exist :create))
    (compile-file-test "compile-file-test-file.lisp"
                       'compile-file-test-fun.1
                       :output-file file))
  t nil)

(5am:test compile-file.19
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :*compile-verbose* t))

(5am:test compile-file.20
  (compile-file-test "compile-file-test-file.lisp"
                     'compile-file-test-fun.1
                     :*compile-print* t))
#+(or) ; testing host behavior
(deftest compile-file-pathname.1
  *compile-file-pathname*
  nil)
#+(or)
(deftest compile-file-truename.1
  *compile-file-truename*
  nil)

;;; Error cases

(5am:test compile-file.error.1
  (5am:signals file-error (compile-file "nonexistent-file-to-compile.lisp")))

(5am:test compile-file.error.2
  (5am:signals program-error (compile-file)))
