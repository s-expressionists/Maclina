(in-package #:maclina.test.cross)

(defun define-specials (client environment)
  ;; from figure 3-2
  (loop for op in '(block      let*                  return-from
                    catch      load-time-value       setq
                    eval-when  locally               symbol-macrolet
                    flet       macrolet              tagbody
                    function   multiple-value-call   the
                    go         multiple-value-prog1  throw
                    if         progn                 unwind-protect
                    labels     progv
                    let        quote)
        do (clostrum:make-special-operator client environment op t)))

;;; define functions that can be just copied from the host
(defun define-aliases (client environment)
  (loop for op in '(list list* length vector make-array make-string
                    consp cons car cdr null not
                    ;; coerce only ok because no test does
                    ;; (coerce foo 'function)
                    1+ 1- + = - floor values functionp coerce
                    values-list eq eql equal equalp
                    error find-package)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f)))

;;; may or may not exist in the lisp, so
(defun define-setters (client environment)
  (flet (((setf %car) (new cons) (setf (car cons) new))
         ((setf %cdr) (new cons) (setf (cdr cons) new)))
    (setf (clostrum:fdefinition client environment '(setf car)) #'(setf %car)
          (clostrum:fdefinition client environment '(setf cdr)) #'(setf %cdr))))

(defun define-sham-aliases (client environment)
  ;; FIXME: Give note-defun an interesting definition
  (loop for op in '(s:notnot s:note-defun)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f))
  (loop for op in '(s:macroexpand-1 s:macroexpand s:eval)
        for cl in '(  macroexpand-1   macroexpand   eval)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f
                 (clostrum:fdefinition client environment cl) f)))

(defun define-env-access (client environment)
  (flet ((%fdefinition (name)
           (clostrum:fdefinition client environment name))
	 ((setf %fdefinition) (fun name)
	   (setf (clostrum:fdefinition client environment name) fun))
         (%symbol-function (name)
           (check-type name symbol)
           (clostrum:fdefinition client environment name)))
    (setf (clostrum:fdefinition client environment 'fdefinition)
          #'%fdefinition
	  (clostrum:fdefinition client environment '(setf fdefinition))
	  #'(setf %fdefinition)
          (clostrum:fdefinition client environment 'symbol-function)
          #'%symbol-function))
  (multiple-value-bind (symbol-value setf-symbol-value
                        boundp makunbound)
      (maclina.vm-cross:make-variable-access-closures client environment)
    (setf (clostrum:fdefinition client environment 'symbol-value)
          symbol-value
          (clostrum:fdefinition client environment '(setf symbol-value))
          setf-symbol-value
          (clostrum:fdefinition client environment 'boundp)
          boundp
          (clostrum:fdefinition client environment 'makunbound)
          makunbound)))

;;; functions that can be copied except we ban the env-specific parts
(defun define-stricter-aliases (client environment)
  (loop for op in '(mapc mapcar mapcan mapl maplist mapcon
                    every some notany notevery funcall apply)
        for f = (fdefinition op)
        for g = (let ((f f))
                  (lambda (fun &rest args)
                    (check-type fun function)
                    (apply f fun args)))
        do (setf (clostrum:fdefinition client environment op) g)))

;;; constants copied from the host
(defun define-constants (client environment)
  (loop for c in '(t nil most-positive-fixnum most-negative-fixnum)
        for v = (symbol-value c)
        do (clostrum:make-constant client environment c v)))

(defun define-macros (client environment)
  (loop for mac in '(s:expand-in-current-env)
        for f = (macro-function mac)
        do (setf (clostrum:macro-function client environment mac) f))
  (loop for mac in '(s:multiple-value-bind s:in-package
                     s:setf s:incf s:decf s:push s:defun
                     s:when s:unless s:prog1 s:prog s:return)
        for cl in    '(multiple-value-bind   in-package
                       setf   incf   decf   push   defun
                       when   unless   prog1   prog   return)
        for f = (macro-function mac)
        do (setf (clostrum:macro-function client environment mac) f
                 (clostrum:macro-function client environment cl) f)))

(defun define-variables (client environment)
  ;; Needed for COMPILE-FILE.
  (loop for var in '(*read-suppress* *read-eval* *features* *read-base*
		     *read-default-float-format*)
	for v = (symbol-value var)
	do (clostrum:make-parameter client environment var v)))

(defun %fill-environment (client environment)
  (define-specials client environment)
  (define-aliases client environment)
  (define-setters client environment)
  (define-sham-aliases client environment)
  (define-env-access client environment)
  (define-stricter-aliases client environment)
  (define-constants client environment)
  (define-macros client environment)
  (define-variables client environment))

;;; On top of all that, we need to define a client so that we
;;; can define some methods to automatically bind keywords.
(defclass client (maclina.vm-cross:client) ())

(defmethod clostrum-sys:variable-cell :around ((client client)
                                               environment symbol)
  (if (keywordp symbol)
      (let ((cell (clostrum-sys:ensure-variable-cell client environment symbol)))
        (setf (clostrum-sys:variable-cell-value client cell) symbol)
        cell)
      (call-next-method)))

(defmethod clostrum-sys:variable-status :around ((client client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))
