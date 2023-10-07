(in-package #:maclina.test)

;;;; KLUDGE TIME
;;;; In an ideal world, all Lisp implementations would support
;;;; first-class environments, and their MACROEXPAND(-1)
;;;; implementations would go through a common interface like Trucler
;;;; so that they could be used with custom environments like ours.
;;;; In our world, none of them do this. This means that any macro
;;;; that calls MACROEXPAND(-1) will very likely throw a fit if
;;;; our compiler expands it. Sometimes it's worse - in SBCL,
;;;; seemingly innocuous macros like COND and UNLESS try to grab an
;;;; internal policy object from their environment, so we can't even
;;;; use those.
;;;; In this file we define some sham operators that work with the
;;;; VM compiler's environments properly. These are used in the tests
;;;; rather than the standard operators.
;;;; VERY IMPORTANTLY, these sham operators must ONLY used in code that
;;;; is fed to the VM compiler - not code for the host. This is in
;;;; preparation for that ideal world, in which the environment
;;;; these tests are run in is minimally defined to include only the
;;;; standard special operators, these few macros, and a short list of
;;;; functions to be catalogued.

;;; Force true values to T.
(defun s:notnot (v) (not (not v)))

(setf (fdefinition 's:macroexpand-1) #'maclina.compile:macroexpand-1
      (fdefinition 's:macroexpand)   #'maclina.compile:macroexpand)

;;; used in e.g. MACROLET.43
(setf (fdefinition 's:eval) #'ceval)

;;; Macro used in tests of environments in system macros
;;; This was inspired by a bug in ACL 8.0 beta where CONSTANTP
;;; was being called in some system macros without the proper
;;; environment argument
(defmacro s:expand-in-current-env (macro-form &environment env)
  (s:macroexpand macro-form env))

;;; used indirectly in ecclesia:parse-macro results
(defmacro s:when (test &body forms)
  `(if ,test (progn ,@forms) nil))
(defmacro s:unless (test &body forms)
  `(if ,test nil (progn ,@forms)))

(defmacro s:prog1 (result &body body)
  (let ((temp (gensym)))
    ;; progn to invalidate declarations
    `(let ((,temp ,result)) (progn ,@body) ,temp)))

(defmacro s:return (&optional value) `(return-from nil ,value))

(defmacro s:prog ((&rest bindings) &body body)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    `(block nil
       (let (,@bindings)
         ,@decls
         (tagbody ,@body)))))

;;; SETF is also quite common.
(defun default-symbol-setf-expansion (symbol)
  (let ((new (gensym "NEW")))
    (values () () `(,new) `(setq ,symbol ,new) symbol)))

(defun default-call-setf-expansion (fname arguments)
  (let ((new (gensym "NEW"))
        (temps (loop repeat (length arguments) collect (gensym))))
    (values temps arguments `(,new)
            `(funcall #'(setf ,fname) ,new ,@temps)
            `(,fname ,@temps))))

(defun %get-setf-expansion (place &optional env)
  (etypecase place
    (symbol (multiple-value-bind (expansion expandedp)
                (s:macroexpand-1 place env)
              (if expandedp
                  (%get-setf-expansion expansion env)
                  (default-symbol-setf-expansion place))))
    (cons (let* ((head (car place))
                 (rest (rest place))
                 (info (trucler:describe-function m:*client* env head)))
            (typecase info
              (trucler:local-function-description ; shadowed
               (default-call-setf-expansion head rest))
              (trucler:local-macro-description
               ;; note that we leave global macros to the host,
               ;; because there might be a global setf expander
               ;; that overrides the macroexpansion.
               (%get-setf-expansion (s:macroexpand-1 place env) env))
              (t ; (for now) we have no setf expanders, so
               (default-call-setf-expansion head rest)))))))

(defmacro s:multiple-value-bind (vars valform &body body)
  (if (= (length vars) 1)
      `(let ((,(first vars) ,valform)) ,@body)
      (let ((r (gensym "REST")))
        `(multiple-value-call (lambda (&optional ,@(mapcar #'list vars)
                                       &rest ,r)
                                (declare (ignore ,r))
                                ,@body)
           ,valform))))

(defmacro s:setf (&rest pairs &environment env)
  (flet ((expand-setf-1 (place value)
           (multiple-value-bind (temps forms news write read)
               (%get-setf-expansion place env)
             (declare (ignore read))
             `(let* (,@(mapcar #'list temps forms))
                (multiple-value-bind (,@news) ,value
                  ,write)))))
    `(progn
       ,@(loop for (place value) on pairs by #'cddr
               collect (expand-setf-1 place value)))))

;;; Used extensively in tests as side effects.
(defmacro s:incf (place &optional (delta 1) &environment env)
  (multiple-value-bind (temps forms news write read)
      (%get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps forms))
       (multiple-value-bind (,@news) (+ ,read ,delta)
         ,write))))
(defmacro s:decf (place &optional (delta 1) &environment env)
  (multiple-value-bind (temps forms news write read)
      (%get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps forms))
       (multiple-value-bind (,@news) (- ,read ,delta)
         ,write))))

;;; Used in UNWIND-PROTECT tests as a side effect.
(defmacro s:push (object place &environment env)
  (multiple-value-bind (temps forms news write read)
      (%get-setf-expansion place env)
    (let ((osym (gensym "OBJECT")))
      `(let* ((,osym ,object)
              ,@(mapcar #'list temps forms)
              (,(first news) (cons ,osym ,read)))
         ,write))))

;;; in compile-file tests
(defun fun-name-block-name (fun-name)
  (etypecase fun-name
    (symbol fun-name)
    ((cons (eql setf) (cons symbol null)) (second fun-name))))

;;; does nothing. the cross definition can be more interesting.
(defun s:note-defun (fun-name) (declare (ignore fun-name)))

(defmacro s:defun (name lambda-list &body body)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation t)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (s:note-defun ',name))
       (s:setf (fdefinition ',name)
	       #'(lambda ,lambda-list
		   (declare ,@decls)
		   ,doc
		   (block ,(fun-name-block-name name) ,@body)))
       ',name)))

(defmacro s:in-package (package-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-package ,package-designator))))
