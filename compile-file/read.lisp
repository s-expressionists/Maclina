(in-package #:maclina.compile-file)

;;; Hooking up Eclector.
;;; Using Eclector means we don't need to worry about implementation-dependent
;;; expansions for backquote and such, and lets us hook up #. to evaluate in
;;; the correct environment.

(defclass reader-client (eclector.parse-result:parse-result-client) ())

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

(defgeneric package-name (client environment package))
(defmethod package-name (client environment package)
  (declare (ignore client environment))
  (cl:package-name package))

(defgeneric find-package (client package-name))
(defmethod find-package ((client reader-client) package-name)
  (cl:find-package package-name))

(defmethod eclector.reader:interpret-symbol ((client reader-client) input-stream
                                             package-indicator symbol-name internp)
  (if (null package-indicator)
      (make-symbol symbol-name)
      (let ((package (case package-indicator
                       (:current (eclector.reader:state-value client '*package*))
                       (:keyword (find-package client "KEYWORD"))
                       (t (find-package client package-indicator)))))
        (cond ((null package) (error "No package named ~a" package-indicator))
              (internp (intern symbol-name package))
              (t (multiple-value-bind (symbol accessiblep)
                     (find-symbol symbol-name package)
                   (if accessiblep
                       symbol
                       (error "No symbol ~a:~a" package-indicator symbol-name))))))))

(defclass source-location ()
  ((%pathname :initarg :pathname :reader source-location-pathname)
   (%position :initarg :position :reader source-location-position)))
(defmethod print-object ((obj source-location) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a position ~a" (source-location-pathname obj)
              (source-location-position obj))))

;;; we need a primary method here. But we just want to pass the compiler
;;; raw forms, so we don't actually make any results.
(defmethod eclector.parse-result:make-expression-result
    ((client reader-client) result children source)
  (declare (ignore children))
  (when (boundp 'cmp:*source-locations*)
    (setf (gethash result cmp:*source-locations*)
          (make-instance 'source-location
            :pathname *compile-file-pathname* :position source)))
  result)
