(in-package #:maclina.compile-file)

;;; Main external entry points: COMPILE-STREAM and COMPILE-FILE.
;;; For more complicated usage there are COMPILE-FILES,
;;; COMPILE-FILE-TO-UNIT, and COMPILE-STREAM-TO-UNIT.

;;; COMPILE-FILES compiles multiple source files into one FASL,
;;; as if the files had been compiled individually and then linked by LINK-FASLS,
;;; but without needing any intermediate FASL files.

;;; COMPILE-FILE-TO-UNIT and COMPILE-STREAM-TO-UNIT are lower level.
;;; Instead of outputting any file, they output a "FASL unit"
;;; (not to be confused with a compilation unit)
;;; which is an opaque object representing the compiled code and objects.
;;; FASL units can be passed to WRITE-UNITS or WRITE-UNITS-TO-FILE.
;;; Both of these functions accept either a FASL unit or a list thereof,
;;; and write a single total FASL to a byte stream or to a file respectively.

;; Print information about a form for *compile-print*.
(defun describe-form (form)
  (fresh-line)
  (write-string ";   ")
  (write form :length 2 :level 2 :lines 1 :pretty nil)
  (terpri)
  (values))

(defun compile-stream-to-unit (input
                               &key environment
                                 (reader-client *reader-client*)
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
        (finish-fasl-unit)))))

(defun compile-file-to-unit (input-file
                             &rest keys
                             &key (external-format :default)
                               ((:verbose *compile-verbose*) *compile-verbose*)
                               ((:print *compile-print*) *compile-print*)
                               environment (reader-client *reader-client*)
		               ((:client m:*client*) m:*client*)
                             &allow-other-keys)
  (declare (ignore reader-client))
  (with-open-file (in input-file :external-format external-format)
    (when *compile-verbose*
      (format t "~&; Compiling file: ~a~%" (namestring input-file)))
    (m:progv m:*client* (cmp:run-time-environment m:*client* environment)
      '(*compile-file-pathname* *compile-file-truename*)
      (list (truename input-file) (pathname (merge-pathnames input-file)))
      (apply #'compile-stream-to-unit in keys))))

;; input is a character stream. output is a ub8 stream.
(defun compile-stream (input output &rest keys
                       &key environment (reader-client *reader-client*)
		       ((:client m:*client*) m:*client*)
                       &allow-other-keys)
  (declare (ignore environment reader-client))
  (multiple-value-bind (unit warning failure)
      (apply #'compile-stream-to-unit input keys)
    (write-bytecode unit output)
    (values output warning failure)))

;;; FIXME: I really doubt this is enough to be conforming.
(defun compile-file-pathname (input-file &key (output-file nil ofp)
			      &allow-other-keys)
  (if ofp
      output-file
      (make-pathname :defaults input-file :type "faslbc")))

(defun compile-file (input-file
                     &rest keys
                     &key output-file
		       (external-format :default)
                       ((:verbose *compile-verbose*) *compile-verbose*)
                       ((:print *compile-print*) *compile-print*)
                       environment (reader-client *reader-client*)
		       ((:client m:*client*) m:*client*)
                     &allow-other-keys)
  ;; passed to compile-file-to-unit
  (declare (ignore external-format environment reader-client))
  (declare (ignore output-file)) ; passed to compile-file-pathname
  (let ((output-file (apply #'compile-file-pathname input-file keys)))
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (multiple-value-bind (unit warningsp failurep)
          (apply #'compile-file-to-unit input-file keys)
        (write-bytecode unit out)
        (values (truename output-file) warningsp failurep)))))

(defun write-units (units stream)
  (etypecase units
    (fasl-unit (write-bytecode units stream))
    (list
     (write-bytecode-header
      (reduce #'+ units :key #'instruction-count)
      stream)
     (dolist (unit units)
       (write-bytecode-instructions unit stream))))
  (values))

(defun compile-files (input-files output-file
                      &rest keys
                      &key (external-format :default)
                        ((:verbose *compile-verbose*) *compile-verbose*)
                        ((:print *compile-print*) *compile-print*)
                        environment (reader-client *reader-client*)
		        ((:client m:*client*) m:*client*)
                      &allow-other-keys)
  (declare (ignore external-format environment reader-client))
  (let ((all-warnings-p nil) (all-failure-p nil))
    ;; Open the output file first even though we don't need it yet,
    ;; so that any filesystem errors occur early.
    (with-open-file (out output-file
                         :direction :output :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (write-units
       (cmp:with-compilation-unit ()
         (loop for input-file in input-files
               collect (multiple-value-bind (unit warningsp failurep)
                           (apply #'compile-file-to-unit input-file keys)
                         (setf all-warnings-p (or all-warnings-p warningsp)
                               all-failure-p (or all-failure-p failurep))
                         unit)))
       out))
    (values (truename output-file)
            all-warnings-p all-failure-p)))

(defun write-units-to-file (units filename)
  (with-open-file (out filename
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (write-units units out))
  (values))
