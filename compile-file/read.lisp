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

(defun %find-package (client environment name)
  ;; Find a package, accounting for package-local nicknames,
  ;; and letting name be any string designator.
  (or
    (let* ((cur (m:symbol-value client environment '*package*))
           (local-nicknames
             (trivial-package-local-nicknames:package-local-nicknames cur)))
      (cdr (assoc name local-nicknames :test #'string=)))
    (m:find-package client environment (string name))))

(define-condition package-missing (package-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No package named ~s"
                     (package-error-package condition)))))

(defun find-package-or-err (client environment name)
  (or (%find-package client environment name)
    (error 'package-missing :package name)))

(defmethod eclector.reader:call-with-state-value
    ((client reader-client) thunk aspect value)
  (let* ((environment (cmp:run-time-environment m:*client* *environment*))
         (aspect (if (eql aspect '*readtable*) 'eclector.reader:*readtable* aspect))
         (value (if (eql aspect '*package*)
                    ;; Per Eclector documentation, it calls this
                    ;; with a value that's actually a string designator
                    ;; rather than a package.
                    (find-package-or-err m:*client* environment value)
                    value)))
    (m:progv m:*client* environment
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

(defmethod eclector.reader:interpret-symbol ((client reader-client) input-stream
                                             package-indicator symbol-name internp)
  (declare (ignore input-stream))
  (if (null package-indicator)
      (make-symbol symbol-name)
      (let ((package (case package-indicator
                       (:current
                        (let ((cur (eclector.reader:state-value
                                    client '*package*)))
                          ;; We disallow this through Eclector above, but
                          ;; there are other ways we can't control in which
                          ;; *package* could be set to something illegal.
                          (assert (packagep cur) ()
                                  "~s is not bound to a package" '*package*)
                          cur))
                       (:keyword (find-package-or-err
                                  m:*client* *environment* "KEYWORD"))
                       (t (find-package-or-err m:*client* *environment*
                                               package-indicator)))))
        (if internp
            (intern symbol-name package)
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (ecase status
                ((:external) symbol)
                ((:internal :inherited)
                 (error "~a is not external in ~a"
                        symbol-name package-indicator))
                ((nil)
                 (error "No symbol ~a:~a" package-indicator symbol-name))))))))

(defclass source-location ()
  ((%pathname :initarg :pathname :reader source-location-pathname)
   (%position :initarg :position :reader source-location-position)))
(defmethod print-object ((obj source-location) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a position ~a" (source-location-pathname obj)
              (source-location-position obj))))

(defgeneric source-location-data (client source))
(defmethod source-location-data (client (source source-location))
  (declare (ignore client))
  (values (source-location-pathname source)
          0 0 ; lineno, column
          (car (source-location-position source))))

;;; we need a primary method here. But we just want to pass the compiler
;;; raw forms, so we don't actually make any results.
(defmethod eclector.parse-result:make-expression-result
    ((client reader-client) result children source)
  (declare (ignore children))
  (when (and (boundp 'cmp:*source-locations*) *source-pathname*)
    (setf (gethash result cmp:*source-locations*)
          (make-instance 'source-location
            :pathname *source-pathname* :position source)))
  result)
