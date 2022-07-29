(defpackage #:compile-to-vm
  (:use #:cl)
  (:shadow #:compile #:macroexpand-1 #:macroexpand #:constantp))

(in-package #:compile-to-vm)

;;; FIXME: New package
(macrolet ((defcodes (&rest names)
             `(progn
                ,@(loop for i from 0
                        for name in names
                        collect `(defconstant ,name ,i))
                (defun decode (code)
                  (nth code '(,@names))))))
  (defcodes +ref+ +const+ +closure+
    +call+ +call-receive-one+ +call-receive-fixed+
    +bind+ +set+
    +make-cell+ +cell-ref+ +cell-set+
    +make-closure+
    +return+
    +jump-if+
    +block-open+ +return-from+ +block-close+
    +tagbody-open+ +go+ +tagbody-close+
    +special-bind+ +symbol-value+ +symbol-value-set+ +unbind+
    +fdefinition+
    +nil+
    +eq+))

;;;

(defun macroexpand-1 (form env) (declare (ignore env)) (values form nil))
(defun macroexpand (form env) (declare (ignore env)) (values form nil))
(defun specialp (symbol env) (declare (ignore symbol env)) nil)
(defun constantp (symbol env) (declare (ignore symbol env)) nil)
(defun constant-form-value (symbol env) (declare (ignore symbol env)))

;;;

(defvar *constants*)

(defun constant-index (constant)
  (or (position constant *constants*)
      (vector-push-extend constant *constants*)))

(defvar *assembly*)

(defmacro assemble (&rest values)
  `(progn
     ,@(loop for val in values
             collect `(vector-push-extend ,val *assembly*))))

(defclass environment ()
  ((%parent :initarg :parent :reader parent :type (or null environment))))

(defclass local-environment (environment)
  (;; An alist (symbol . index)
   (%lexicals :initarg :lexicals :reader lexicals :type list)
   (%parent :initarg :parent :reader parent :type (or null environment))))

;;; This is a "choke" put in place so that the compiler knows when a variable
;;; is bound in an outer function and so must be closed over.
(defclass closure-environment (environment) ())

;;; Get information about a lexical variable.
;;; Returns two values. The first is :CLOSURE or :LOCAL or NIL.
;;; The second is an index into the associated data corresponding to the symbol, or NIL.
;;; If the first value is NIL, the variable is unknown.
(defgeneric lexical-index (symbol env)
  (:argument-precedence-order env symbol))

(defmethod lexical-index ((symbol symbol) (env local-environment))
  (let ((pair (assoc symbol (lexicals env))))
    (if pair
        (values :local (cdr pair))
        (lexical-index symbol (parent env)))))

(defvar *closure*)

(defun closure-index (symbol)
  (or (position symbol *closure*) (vector-push-extend symbol *closure*)))

(defmethod lexical-index ((symbol symbol) (env closure-environment))
  ;; Make sure the variable is known. If it is, put it in the closure.
  (if (lexical-index symbol (parent env))
      (values :closure (closure-index symbol))
      (values nil nil)))

(defmethod lexical-index ((symbol symbol) (env null)) (values nil nil))

(defvar *next-local-index*)
(defvar *local-index-count*) ; highest value of *n-l-i* obtained while compiling

(defun update-lic ()
  (setf *local-index-count* (max *local-index-count* *next-local-index*)))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))

(defstruct (function-prototype (:constructor make-function-prototype
                                 (bytecode constants nlocals closed)))
  bytecode constants nlocals closed)

(defun compile (lambda-expression env)
  (check-type lambda-expression lambda-expression)
  (let ((lambda-list (cadr lambda-expression))
        (body (cddr lambda-expression))
        (*assembly* (make-array 0 :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t))
        (*constants* (make-array 0 :fill-pointer 0 :adjustable t))
        (*closure* (make-array 0 :fill-pointer 0 :adjustable t))
        (*local-index-count* 0))
    (assert (every #'symbolp lambda-list))
    ;; Initialize variables. Replace each value on the stack with a mutable cell
    ;; containing that value.
    (loop for i from 0 for arg in lambda-list
          do (assemble +ref+ i +make-cell+ +set+ i))
    (let ((env (make-instance 'local-environment
                 :lexicals (loop for i from 0 for arg in lambda-list
                                 collect (cons arg i))
                 :parent (make-instance 'closure-environment :parent env)))
          (*next-local-index* (length lambda-list)))
      (update-lic)
      (compile-progn body env t))
    (assemble +return+)
    (make-function-prototype (copy-seq *assembly*) (copy-seq *constants*)
                             *local-index-count* *closure*)))

;;;

(defun compile-form (form env context)
  (let ((form (macroexpand form env)))
    (etypecase form
      (symbol (compile-symbol form env context))
      (cons (compile-cons (car form) (cdr form) env context))
      (t (compile-constant form env context)))))

(defun compile-constant (form env context)
  (declare (ignore env))
  (unless (eql context 0)
    (assemble +const+ (constant-index form))))

(defun compile-symbol (form env context)
  (unless (eql context 0)
    (cond ((specialp form env)
           (assemble +symbol-value+ (constant-index form)))
          ((constantp form env)
           (assemble +const+ (constant-index (constant-form-value form env))))
          (t ; lexical
           (multiple-value-bind (kind index) (lexical-index form env)
             (ecase kind
               ((:local) (assemble +ref+ index +cell-ref+))
               ((:closure) (assemble +closure+ index +cell-ref+))
               (t
                (warn "Unknown variable ~a: treating as special" form)
                (assemble +symbol-value+ (constant-index form)))))))))

(defun compile-cons (head rest env context)
  (case head
    ((progn) (compile-progn rest env context))
    ((let) (compile-let (first rest) (rest rest) env context))
    ((function) (compile-function (first rest) env context))
    (otherwise ; function call
     (dolist (arg rest) (compile-form arg env 1))
     (assemble +fdefinition+ (constant-index head) +call+ (length rest)))))

(defun compile-progn (forms env context)
  (loop for form in (butlast forms)
        collect (compile-form form env 0))
  (compile-form (first (last forms)) env context))

(defun compile-let (bindings body env context)
  (let* ((vars
           ;; Compile the values as we go.
           ;; FIXME: NLX will complicate this.
           (loop for binding in bindings
                 if (symbolp binding)
                   collect binding
                   and do (assemble +nil+)
                 if (and (consp binding) (null (cdr binding)))
                   collect (car binding)
                   and do (assemble +nil+)
                 if (and (consp binding) (consp (cdr binding)) (null (cddr binding)))
                   collect (car binding)
                   and do (compile-form (cadr binding) env 1)
                 do (assemble +make-cell+)))
         (old-next-local-index *next-local-index*)
         (env (make-instance 'local-environment
                :lexicals (loop for var in vars
                                for index from old-next-local-index
                                collect (cons var index))
                :parent env))
         (*next-local-index* (+ *next-local-index* (length vars))))
    (update-lic)
    (assemble +bind+ (length vars) (1- *next-local-index*))
    (compile-progn body env context)))

(defun compile-function (fnameoid env context)
  (unless (eql context 0)
    (if (typep fnameoid 'lambda-expression)
        (let* ((proto (compile fnameoid env))
               (pin (constant-index
                     (list (function-prototype-bytecode proto) 10 ; kludge
                           (function-prototype-constants proto))))
               (closed (function-prototype-closed proto)))
          (loop for var across closed
                do (compile-closure-var var env))
          (assemble +make-closure+ (length closed) pin))
        ;; TODO: Lexical functions
        (assemble +fdefinition+ (constant-index fnameoid)))))

;;; Compile code to get the cell for a given variable and push it to the stack.
(defun compile-closure-var (var env)
  (multiple-value-bind (kind index) (lexical-index var env)
    (ecase kind
      ((:local) (assemble +ref+ index))
      ((:closure) (assemble +closure+ index)))))
