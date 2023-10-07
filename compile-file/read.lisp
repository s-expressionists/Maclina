(in-package #:maclina.compile-file)

;;; Hooking up Eclector.
;;; Using Eclector means we don't need to worry about implementation-dependent
;;; expansions for backquote and such, and lets us hook up #. to evaluate in
;;; the correct environment.

(defclass reader-client () ())

;;; We have our own variable here rather than using m:*client* again because
;;; we need this stuff to work regardless of m:*client*. But compile-file also
;;; accepts a :reader-client argument.

(defvar *reader-client* (make-instance 'reader-client))

(defmethod eclector.reader:state-value ((client reader-client) aspect)
  (let (;; This is required so that we don't try to mess with *readtable*
	;; with the native client. We can't bind cl:*readtable* to one of
	;; eclector's readtables.
	(aspect (if (eql aspect '*readtable*) 'eclector.reader:*readtable* aspect)))
    (m:symbol-value m:*client*
		    (cmp:run-time-environment m:*client* *environment*) aspect)))

(defmethod eclector.reader:call-with-state-value
    ((client reader-client) thunk aspect value)
  (let ((aspect (if (eql aspect '*readtable*) 'eclector.reader:*readtable* aspect)))
    (m:progv m:*client* (cmp:run-time-environment m:*client* *environment*)
      (list aspect) (list value)
      (funcall thunk))))

(defmethod eclector.reader:evaluate-expression ((client reader-client)
                                                expression)
  (cmp:eval expression *environment*))

;;; FIXME: make-structure-instance probably also needs specialization.
;;; The host reader macro will look up the structure name in the host
;;; global environment to get a class to instantiate.
