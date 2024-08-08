(in-package #:maclina.test)

(5am:def-suite externalize :in fasl)
(5am:in-suite externalize)

;;; Make a fresh environment containing only SETQ, QUOTE, *SOURCE*, *COMPILED*,
;;; and stuff for the reader. The file will be compiled there.
(defun make-clean-compilation-environment (object)
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (clostrum:make-special-operator m:*client* rte 'setq t)
    (clostrum:make-special-operator m:*client* rte 'quote t)
    (clostrum:make-parameter m:*client* rte '*source* object)
    ;; FIXME: make-variable's value should be optional. Oopsie.
    (clostrum:make-variable m:*client* rte '*compiled* nil)
    (loop for var in '(*read-suppress* *read-eval* *features* *read-base*
		       *read-default-float-format*)
	  do (clostrum:make-parameter m:*client* rte var (symbol-value var)))
    ce))

;;; Make a fresh environment containing just *COMPILED*.
(defun make-clean-load-environment ()
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (clostrum:make-variable m:*client* rte '*compiled* nil)
    ce))

(defun compile-test-file (input-file &rest keys &key &allow-other-keys)
  ;; Check that warning behavior matches return values.
  (let ((signaled-warnings-p nil) (signaled-failure-p nil))
    (multiple-value-call
        (lambda (&optional output warningsp (failurep nil 3vp)
                 &rest extra)
          (5am:is-true 3vp
                       "<3 values returned from ~s" 'compile-file)
          (5am:is (null extra)
                  "Extra values returned from ~s" 'compile-file)
          (5am:is (eql signaled-failure-p (not (not failurep)))
                  "~s signaled failure but didn't return failurep"
                  'compile-file)
          (5am:is (eql signaled-warnings-p (not (not warningsp)))
                  "~s signaled warnings but didn't return warningsp"
                  'compile-file)
          (values output failurep warningsp))
      (handler-bind ((warning (lambda (c)
                                (declare (ignore c))
                                (setq signaled-warnings-p t)))
                     ((or error (and warning (not style-warning)))
                       (lambda (c)
                         (declare (ignore c))
                         (setq signaled-failure-p t))))
        (apply #'maclina.compile-file:compile-file input-file keys)))))

(defun compile-externalization-test-file (object)
  (with-open-file (s "externalization-test-file.lisp"
                     :direction :output :if-exists :supersede)
    (write-line "(setq *compiled* '#.*source*)" s))
  (multiple-value-bind (output failurep warningsp)
      (compile-test-file "externalization-test-file.lisp"
			 :verbose nil
                         :environment (make-clean-compilation-environment object))
    (5am:is-false failurep "Compilation failed")
    (5am:is-false warningsp "Compilation signaled warnings")
    output))

(defun load-externalized-object (filename)
  (let* ((load-env (make-clean-load-environment))
         (load-rte
           (maclina.compile:run-time-environment m:*client* load-env)))
    (maclina.load:load-bytecode filename :environment load-rte)
    (maclina.compile:eval '*compiled* load-env)))

(defun test-externalize (object)
  (let* ((output (compile-externalization-test-file object))
	 (compiled (load-externalized-object output)))
    (5am:is (similarp object compiled)
	    "~s loaded as un-similar object ~s" object compiled)))

;;; List of objects to test externalization of.
(defparameter *externalization-universe*
  '(12 #.(* 3 most-positive-fixnum)
    #-sbcl #p"SYS:SRC;CORE;LISPLIST.CC"
    #-sbcl #p"/usr/share/Adobe/doc/example/android_vm/root/sbin/ls.jar"
    -17 0 #.(- (* most-negative-fixnum 7) 8)
    4/3 -3/2 7.6s0 3.2f-4 4.1d8 5.6l23 #c(3.2 4.7) #c(-8 3/2)
    #\Newline #\7 dribble #:make-load-form #:nonexist
    #.*package* (machine-instance . 223) #1=(#1# (#1# . #1#) . #1#)
    #.(make-hash-table) #.(make-hash-table :test #'equal)
    #.(let ((ht (make-hash-table)))
        (setf (gethash 4 ht) "booze" (gethash 'hello ht) 'world)
        ht)
    #3=(#2=(#3# #2#) #1# . #2#) "Hello, world!" #*11010110101
    #0a17 #4=#0a#4# #5=#2a((#5# #4#) (#3# #5#)) "畫蛇添足"
    #.(make-array '(3 2) :element-type 'standard-char
       :initial-contents '((#\f #\7) (#\Space #\\) (#\. #\%)))
    #.(make-array '(3 3 3) :adjustable t :initial-element '#:horn)
    ;; This constant crashes SBCL - see SBCL bug 2038233
    #-sbcl #.(make-array 2 :displaced-to '#5#)
    #.(make-array 7 :element-type '(unsigned-byte 3) :fill-pointer 2
       :initial-element 2)
    #(2 #:blue #'standard-object "who?" #2a((0 1) (1 0)))))

(5am:test externalize-objects
  (mapc #'test-externalize *externalization-universe*)
  ;; Plus.
  (test-externalize *externalization-universe*)
  (test-externalize (let ((o (copy-list *externalization-universe*)))
                      (nconc o o)))
  ;; FIXME: dunno about other implementations
  #+sbcl(5am:skip "Can't externalize SBCL pathnames as the hosts are bespoke and unexternalizable"))

(5am:test environment-inconsistency
  ;; Package inconsistency
  ;; Unfortunately this test is not valid: DELETE-PACKAGE gives later
  ;; package operations (e.g. INTERN) undefined effects, rather than
  ;; making them errors. For shame.
  #+(or)
  (let* ((pname (gensym "PACKAGE"))
	 (package (make-package pname))
	 (symbol (intern (symbol-name pname) package))
	 (fasl (compile-externalization-test-file symbol)))
    (delete-package package)
    (handler-case (load-externalized-object fasl)
      (package-error (e)
	(5am:pass "Loader signaled a package error")
	(5am:is (string= (symbol-name pname) (package-error-package e))
		"Loader package error had incorrect package name ~s"
		(package-error-package e)))
      (error (e)
	(5am:fail "Loader signaled ~s instead of a package error" e))
      (:no-error (object)
	(5am:fail "Loader did not signal an error and instead returned ~s" object)))))
