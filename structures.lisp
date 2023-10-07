(in-package #:maclina.machine)

(defvar *client*)

(defstruct bytecode-module
  bytecode
  literals)

(defclass bytecode-function (closer-mop:funcallable-standard-object)
  ((%module :initarg :module :accessor bytecode-function-module)
   (%locals-frame-size :initarg :locals-frame-size :accessor bytecode-function-locals-frame-size)
   (%environment-size :initarg :environment-size :accessor bytecode-function-environment-size)
   (%entry-pc :initarg :entry-pc :accessor bytecode-function-entry-pc)
   (%size :initarg :size :accessor bytecode-function-size)
   ;; Debug stuff.
   (%name :initform cl:nil :accessor bytecode-function-name)
   (%documentation :accessor bytecode-function-documentation)
   (%lambda-list :accessor bytecode-function-lambda-list))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((o bytecode-function) stream)
  ;; TODO? For unnamed functions put in something lambda list based.
  (print-unreadable-object (o stream :type t)
    (write (bytecode-function-name o) :stream stream))
  o)

;;; We need to specify doc-type to be more specific than the standard methods.
(defmethod documentation ((fun bytecode-function) (doc-type (eql 'cl:function)))
  (bytecode-function-documentation fun))
(defmethod documentation ((fun bytecode-function) (doc-type (eql 'cl:t)))
  (bytecode-function-documentation fun))
(defmethod (setf documentation) (new (fun bytecode-function)
                                 (doc-type (eql 'cl:function)))
  (setf (bytecode-function-documentation fun) new))
(defmethod (setf documentation) (new (fun bytecode-function)
                                 (doc-type (eql 'cl:t)))
  (setf (bytecode-function-documentation fun) new))

(defgeneric compute-instance-function (client function))

(defun make-bytecode-function (client module locals-frame-size environment-size entry-pc size)
  (let ((fun (make-instance 'bytecode-function
               :module module
               :locals-frame-size locals-frame-size
               :environment-size environment-size
               :entry-pc entry-pc
               :size size)))
    (closer-mop:set-funcallable-instance-function
     fun (compute-instance-function client fun))
    fun))

(defclass bytecode-closure (closer-mop:funcallable-standard-object)
  ((%template :initarg :template :accessor bytecode-closure-template)
   (%env :initarg :env :accessor bytecode-closure-env))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod bytecode-function-name ((fun bytecode-closure))
  (bytecode-function-name (bytecode-closure-template fun)))
(defmethod bytecode-function-lambda-list ((fun bytecode-closure))
  (bytecode-function-lambda-list (bytecode-closure-template fun)))

(defmethod print-object ((o bytecode-closure) stream)
  (print-unreadable-object (o stream :type t)
    (write (bytecode-function-name o) :stream stream))
  o)

(defmethod documentation ((fun bytecode-closure) (doc-type (eql 'cl:function)))
  (documentation (bytecode-closure-template fun) doc-type))
(defmethod documentation ((fun bytecode-closure) (doc-type (eql 'cl:t)))
  (documentation (bytecode-closure-template fun) doc-type))
(defmethod (setf documentation) (new (fun bytecode-closure)
                                 (doc-type (eql 'cl:function)))
  (setf (documentation (bytecode-closure-template fun) doc-type) new))
(defmethod (setf documentation) (new (fun bytecode-closure)
                                 (doc-type (eql 'cl:t)))
  (setf (documentation (bytecode-closure-template fun) doc-type) new))

(defun make-bytecode-closure (client template env)
  (let ((clos
          (make-instance 'bytecode-closure :template template :env env)))
    (closer-mop:set-funcallable-instance-function
     clos (compute-instance-function client clos))
    clos))
