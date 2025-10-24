(in-package #:maclina.machine)

(defvar *client*)

(defclass module ()
  ((%bytecode :initarg :bytecode :reader bytecode)
   (%literals :initarg :literals :accessor literals)
   (%pc-map :initarg :pc-map :accessor pc-map)))

(defgeneric make-module (client bytecode))
(defmethod make-module (client bytecode)
  (make-instance 'module :bytecode bytecode))

(defclass function (closer-mop:funcallable-standard-object)
  ((%module :initarg :module :accessor module)
   (%locals-frame-size :initarg :locals-frame-size :reader locals-frame-size)
   (%environment-size :initarg :environment-size :reader environment-size)
   (%entry-pc :initarg :entry-pc :reader entry-pc)
   (%size :initarg :size :reader size)
   ;; Debug stuff.
   (%name :initform cl:nil :accessor name)
   ;; not exported - use cl:documentation
   (%documentation :initform () :accessor bytecode-function-documentation)
   (%lambda-list :accessor lambda-list))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((o function) stream)
  ;; TODO? For unnamed functions put in something lambda list based.
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

;;; We need to specify doc-type to be more specific than the standard methods.
(defmethod documentation ((fun function) (doc-type (eql 'cl:function)))
  (bytecode-function-documentation fun))
(defmethod documentation ((fun function) (doc-type (eql 'cl:t)))
  (bytecode-function-documentation fun))
(defmethod (setf documentation) (new (fun function) (doc-type (eql 'cl:function)))
  (setf (bytecode-function-documentation fun) new))
(defmethod (setf documentation) (new (fun function) (doc-type (eql 'cl:t)))
  (setf (bytecode-function-documentation fun) new))

(defgeneric compute-instance-function (client function))

(defgeneric make-function (client module
                           locals-frame-size environment-size entry-pc size))
(defmethod make-function (client module
                          locals-frame-size environment-size entry-pc size)
  (let ((fun (make-instance 'function
               :module module
               :locals-frame-size locals-frame-size
               :environment-size environment-size
               :entry-pc entry-pc
               :size size)))
    (closer-mop:set-funcallable-instance-function
     fun (compute-instance-function client fun))
    fun))

(defclass closure (closer-mop:funcallable-standard-object)
  ((%template :initarg :template :reader template)
   (%environment :initarg :environment :reader environment))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod name ((fun closure)) (name (template fun)))
(defmethod lambda-list ((fun closure)) (lambda-list (template fun)))

(defmethod print-object ((o closure) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defmethod documentation ((fun closure) (doc-type (eql 'cl:function)))
  (documentation (template fun) doc-type))
(defmethod documentation ((fun closure) (doc-type (eql 'cl:t)))
  (documentation (template fun) doc-type))
(defmethod (setf documentation) (new (fun closure) (doc-type (eql 'cl:function)))
  (setf (documentation (template fun) doc-type) new))
(defmethod (setf documentation) (new (fun closure) (doc-type (eql 'cl:t)))
  (setf (documentation (template fun) doc-type) new))

(defgeneric make-closure (client template &optional environment))
(defmethod make-closure (client template
                         &optional (env (make-array (environment-size template))))
  (let ((clos
          (make-instance 'closure :template template :environment env)))
    (closer-mop:set-funcallable-instance-function
     clos (compute-instance-function client clos))
    clos))
