;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Feb 24 20:22:23 2004
;;;; Contains: Tests of RETURN-FROM

(in-package #:maclina.test)

(5am:def-suite return-from :in data-and-control-flow)
(5am:in-suite return-from)

;;; RETURN-FROM is tested extensively in other files

(deftest return-from.1
  (block xyz (return-from xyz) :bad)
  nil)

(deftest return-from.2
  (block nil (return-from nil :good) :bad)
  :good)

;;; Macros are expanded in the appropriate environment

(deftest return-from.3
  (macrolet
      ((%m (z) z))
    (block foo (return-from foo (s:expand-in-current-env (%m :good)))))
  :good)
