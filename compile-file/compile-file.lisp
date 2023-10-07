(in-package #:maclina.compile-file)

;;; Main external entry points: COMPILE-STREAM and COMPILE-FILE.

;; Print information about a form for *compile-print*.
(defun describe-form (form)
  (fresh-line)
  (write-string ";   ")
  (write form :length 2 :level 2 :lines 1 :pretty nil)
  (terpri)
  (values))

;; input is a character stream. output is a ub8 stream.
(defun compile-stream (input output
                       &key environment (reader-client *reader-client*)
		       ((:client m:*client*) m:*client*)
                       &allow-other-keys)
  (cmp:with-compilation-results
    (cmp:with-compilation-unit ()
      (with-constants ()
	(m:progv m:*client* (cmp:run-time-environment m:*client* environment)
	  '(eclector.reader:*readtable* *package*)
	  (list eclector.reader:*readtable* *package*)
          ;; Read and compile the forms.
          (loop with env = (cmp:make-null-lexical-environment environment)
		with eof = (gensym "EOF")
		with *compile-time-too* = nil
		with *environment* = environment
		with eclector.base:*client* = reader-client
		for form = (eclector.reader:read input nil eof)
		until (eq form eof)
		when *compile-print*
                  do (describe-form form)
		do (compile-toplevel form env)))
        ;; Write out the FASO bytecode.
        (write-bytecode (reverse *instructions*) output))
      output)))

;;; FIXME: I really doubt this is enough to be conforming.
(defun compile-file-pathname (input-file &key (output-file nil ofp)
			      &allow-other-keys)
  (if ofp
      output-file
      (make-pathname :defaults input-file :type "faslbc")))

(defun compile-file (input-file
                     &rest keys
                     &key (output-file nil ofp)
		       (external-format :default)
                       ((:verbose *compile-verbose*) *compile-verbose*)
                       ((:print *compile-print*) *compile-print*)
                       environment (reader-client *reader-client*)
		       ((:client m:*client*) m:*client*)
                     &allow-other-keys)
  (declare (ignore reader-client)) ; passed to compile-stream
  (let ((output-file (apply #'compile-file-pathname input-file keys)))
    (with-open-file (in input-file :external-format external-format)
      (with-open-file (out output-file
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
	(when *compile-verbose*
	  (format t "~&; Compiling file: ~a~%" (namestring input-file)))
        (multiple-value-bind (out warningsp failurep)
	    (m:progv m:*client* (cmp:run-time-environment m:*client* environment)
	      '(*compile-file-pathname* *compile-file-truename*)
	      (list (truename input-file) (pathname (merge-pathnames input-file)))
              (apply #'compile-stream in out keys))
	  (declare (ignore out))
          (values (truename output-file) warningsp failurep))))))
