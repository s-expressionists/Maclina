(in-package #:cvm.test.cross)

#| ; accounting for what's actually used
compile: (eq let quote)
dynamic-extent: (let quote list length vector flet mapcar labels consp cons car cdr most-positive-fixnum 1- values setq prog1 make-array coerce equal make-string every eql s:setf function)
eval: (let)
eval-when: (eval-when values setq let s:expand-in-current-env)
ignorable: ()
ignore: ()
lambda: (s:notnot 1+ s:incf) ; not the lambda macro, funnily enough
locally: (locally t nil macrolet) ; nil's probably in an earlier file, w/e
optimize: ()
special: ()
symbol-macrolet: (symbol-macrolet)
the: (the)
type: (s:decf)
block: (block return-from return tagbody)
flet: (:x) ; bla bla keywords will need a clostrum intercept
if: (if =)
labels:
let: (+)
multiple-value-call: (multiple-value-call floor)
multiple-value-prog1: (values-list)
progn: (progn go)
return-from: ()
tagbody: ()
|#

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
                    error)
        for f = (fdefinition op)
        do (setf (clostrum:fdefinition client environment op) f)))

;;; may or may not exist in the lisp, so
(defun define-setters (client environment)
  (flet (((setf %car) (new cons) (setf (car cons) new))
         ((setf %cdr) (new cons) (setf (cdr cons) new)))
    (setf (clostrum:fdefinition client environment '(setf car)) #'(setf %car)
          (clostrum:fdefinition client environment '(setf cdr)) #'(setf %cdr))))

(defun define-sham-aliases (client environment)
  (loop for op in '(s:notnot)
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
         (%symbol-function (name)
           (check-type name symbol)
           (clostrum:fdefinition client environment name)))
    (setf (clostrum:fdefinition client environment 'fdefinition)
          #'%fdefinition
          (clostrum:fdefinition client environment 'symbol-function)
          #'%symbol-function))
  (multiple-value-bind (symbol-value setf-symbol-value
                        boundp makunbound)
      (cvm.cross.vm:make-variable-access-closures client environment)
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
  (loop for mac in '(s:multiple-value-bind
                     s:setf s:incf s:decf
                     s:when s:unless s:prog1 s:prog s:return)
        for cl in    '(multiple-value-bind
                       setf   incf   decf
                       when   unless   prog1   prog   return)
        for f = (macro-function mac)
        do (setf (clostrum:macro-function client environment mac) f
                 (clostrum:macro-function client environment cl) f)))

(defun %fill-environment (client environment)
  (define-specials client environment)
  (define-aliases client environment)
  (define-setters client environment)
  (define-sham-aliases client environment)
  (define-env-access client environment)
  (define-stricter-aliases client environment)
  (define-constants client environment)
  (define-macros client environment))

;;; On top of all that, we need to define a client so that we
;;; can define some methods to automatically bind keywords.
(defclass client (cvm.cross:client) ())

(defmethod clostrum-sys:variable-cell :around ((client client)
                                               environment symbol)
  (let ((cell (call-next-method)))
    (when (keywordp symbol)
      (setf (clostrum-sys:variable-cell-value client cell) symbol))
    cell))

(defmethod clostrum-sys:variable-status :around ((client client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))
