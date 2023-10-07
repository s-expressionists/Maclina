(defpackage #:maclina.argparse
  (:use #:cl)
  (:export #:argument-error
           #:wrong-number-of-arguments #:odd-keywords
           #:unrecognized-keyword-argument))

(in-package #:maclina.argparse)

;;; abstract parent type for errors signaled by lambda list processing
(define-condition argument-error (program-error)
  ((%called-function :initform nil :initarg :called-function
                     :reader called-function)))

;;; nabbed from clasp
(define-condition wrong-number-of-arguments (argument-error)
  ((%given-nargs :initarg :given-nargs :reader given-nargs)
   (%min-nargs :initarg :min-nargs :reader min-nargs :initform nil)
   (%max-nargs :initarg :max-nargs :reader max-nargs :initform nil))
  (:report (lambda (condition stream)
             (let* ((min (min-nargs condition))
                    (max (max-nargs condition))
                    ;; FIXME: get an actual name if possible
                    (dname nil))
               (format stream "~@[Calling ~a - ~]Got ~d arguments, but expected ~@?"
                       dname (given-nargs condition)
                       (cond ((null max)  "at least ~d")
                             ((null min)  "at most ~*~d")
                             ;; I think "exactly 0" is better than "at most 0", thus duplication
                             ((= min max) "exactly ~d")
                             ((zerop min) "at most ~*~d")
                             (t           "between ~d and ~d"))
                       min max)))))

(define-condition odd-keywords (argument-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Odd number of keyword arguments~:[~; for ~s~]."
                     (called-function condition)
                     ;; FIXME: again, get an actual name somehow.
                     nil))))

(define-condition unrecognized-keyword-argument (argument-error)
  ((%unrecognized-keywords :initarg :unrecognized-keywords
                           :reader unrecognized-keywords))
  (:report (lambda (condition stream)
             (format stream "Unrecognized keyword arguments ~S~:[~; for ~S~]."
                     (unrecognized-keywords condition)
                     (called-function condition) ; FIXME: name
                     nil))))
