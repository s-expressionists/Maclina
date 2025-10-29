(in-package #:maclina.compile-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial processing level: Reflects only the necessary recursion
;;; structure, not necessarily the eventual underlying representation.
;;; We collect a sequence of specialized "instructions" that, when executed,
;;; will create and initialize the LTV table.

(defclass instruction () ())
;;; An instruction that allocates or otherwise creates an object.
;;; The object may be fully initialized or may require further initialization.
(defclass creator (instruction)
  ((%index :initform nil :initarg :index :accessor index
           :type (or null (integer 0)))))
;;; A creator for which a prototype value (which the eventual LTV will be
;;; similar to) is available.
(defclass vcreator (creator)
  ((%prototype :initarg :prototype :reader prototype)))

(defmethod print-object ((object creator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d" (index object))))

(defmethod print-object ((object vcreator) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object '%prototype)
        (prin1 (prototype object) stream)
        (write-string "[no prototype]" stream))
    (format stream " ~d" (index object))))

;;; An instruction that performs some action for effect. This can include
;;; initialization as well as arbitrary side effects (as from make-load-form).
(defclass effect (instruction) ())

;;;

;;; TODO: Abbreviate with list/dotted list, but make sure
;;; coalescence is still really possible.
(defclass cons-creator (vcreator) ())

(defclass initialize-cons (effect)
  ((%cons :initarg :cons :reader rplac-cons :type cons-creator)
   (%car :initarg :car :reader rplac-car :type creator)
   (%cdr :initarg :cdr :reader rplac-cdr :type creator)))

;;; dimensions and element-type are encoded with the array since
;;; they shouldn't really need to be coalesced.
(defclass array-creator (vcreator)
  ((%dimensions :initarg :dimensions :reader dimensions)
   (%packing-info :initarg :packing-info :reader packing-info)
   (%element-type-info :initarg :element-type-info :reader element-type-info)))

;;; Initialize contents of a general array. This is a separate instruction
;;; because such arrays may contain themselves.
(defclass initialize-array (effect)
  ((%array :initarg :array :reader initialized-array :type array-creator)
   ;; A list of creators as long as the array's total size.
   (%values :initarg :values :reader array-values :type list)))

;;; Special cases of array-creator, since they're very very common
;;; for e.g. symbol names.
(defclass base-string-creator (vcreator) ())
(defclass utf8-string-creator (vcreator)
  ((%nbytes :initarg :nbytes :reader nbytes :type (unsigned-byte 16))))

(defclass hash-table-creator (vcreator)
  (;; used in disltv
   (%test :initarg :test :reader hash-table-creator-test :type symbol)
   (%count :initarg :count :reader hash-table-creator-count
           :type (integer 0))))

;;; Initialize contents of a hash table. Separate instruction because
;;; circular references are possible.
(defclass initialize-hash-table (effect)
  ((%table :initarg :table :reader initialized-table :type hash-table-creator)
   ;; We have to store the count ourselves, since the hash table size may
   ;; not be identical to the number of elements.
   (%count :initarg :count :reader initialized-table-count
           :type (unsigned-byte 32))
   ;; An alist of all the keys and values in the table.
   ;; The keys and values are creators.
   (%alist :initarg :alist :reader alist :type list)))

(defclass symbol-creator (vcreator)
  (;; Is there actually a point to trying to coalesce symbol names?
   (%name :initarg :name :reader symbol-creator-name :type creator)))

(defclass interned-symbol-creator (symbol-creator)
  ((%package :initarg :package :reader symbol-creator-package :type creator)))

(defclass package-creator (vcreator)
  (;; Is there actually a point to trying to coalesce package names?
   ;; Also, some symbols (CL, KEYWORD) could probably be dumped without
   ;; a general package reference.
   (%name :initarg :name :reader package-creator-name :type creator)))

(defclass number-creator (vcreator) ())
(defclass sb64-creator (number-creator) ())
(defclass bignum-creator (number-creator) ())
(defclass ratio-creator (number-creator)
  ((%numerator :initarg :numerator :reader ratio-creator-numerator
               :type creator)
   (%denominator :initarg :denominator :reader ratio-creator-denominator
                 :type creator)))
(defclass complex-creator (number-creator)
  ((%realpart :initarg :realpart :reader complex-creator-realpart
              :type creator)
   (%imagpart :initarg :imagpart :reader complex-creator-imagpart
              :type creator)))
(defclass short-float-creator (number-creator) ())
(defclass single-float-creator (number-creator) ())
(defclass double-float-creator (number-creator) ())
(defclass long-float-creator (number-creator) ())

(defclass character-creator (vcreator) ())

;;; FIXME: Trying to coalesce all this stuff might be pointless.
;;; But maybe not - lots of stuff probably shares a type, I guess.
(defclass pathname-creator (vcreator)
  ((%host :initarg :host :reader pathname-creator-host :type creator)
   (%device :initarg :device :reader pathname-creator-device :type creator)
   (%directory :initarg :directory :reader pathname-creator-directory
               :type creator)
   (%name :initarg :name :reader pathname-creator-name :type creator)
   (%type :initarg :type :reader pathname-creator-type :type creator)
   (%version :initarg :version :reader pathname-creator-version :type creator)))

(defclass fdefinition-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; Look up the "cell" for a function binding - something that the VM's
;;; FDEFINITION instruction can get an actual function out of.
;;; The nature of this cell is implementation-dependent.
;;; In a one-environment implementation, the "cell" can just be the function name,
;;; and the FDEFINITION instruction just does CL:FDEFINITION.
(defclass fcell-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; Set what's in an fcell.
(defclass fcell-set (effect)
  ((%fcell :initarg :fcell :reader fcell :type creator)
   (%value :initarg :value :reader value :type creator)))

;;; Look up the "cell" for special variable binding. This is used by the
;;; SPECIAL-BIND, SYMBOL-VALUE, and SYMBOL-VALUE-SET VM instructions
;;; as a lookup key for the binding, as well as for establishing new
;;; local bindings.
;;; The nature of this cell is implementation-dependent.
;;; In a one-environment implementation, the "cell" can just be the symbol itself,
;;; and the SYMBOL-VALUE instruction just does CL:SYMBOL-VALUE, etc.
(defclass vcell-lookup (creator)
  ((%name :initarg :name :reader name :type creator)))

;;; Look up the global environment the FASL was loaded in.
;;; In a one-environment implementation this can just return NIL,
;;; as the VM won't need any references to other environments.
(defclass environment-lookup (creator)
  ())

(defclass general-creator (vcreator)
  (;; Reference to a function designator to call to allocate the object,
   ;; e.g. a function made of the first return value from make-load-form.
   ;; The function returns the new value as its primary.
   ;; Other values are ignored.
   ;; FIXME: Maybe should be a definite function, but this would require
   ;; an FDEFINITION instruction.
   (%function :initarg :function :reader general-function
              :type creator)
   ;; List of arguments (creators) to be passed to the function.
   (%arguments :initarg :arguments :reader general-arguments :type list)))

(defclass general-initializer (effect)
  (;; Reference to a function designator to call to initialize the object,
   ;; e.g. a function made of the second return value from make-load-form.
   ;; The function's return values are ignored.
   (%function :initarg :function :reader general-function
              :type creator)
   ;; List of arguments (creators) to be passed to the function.
   (%arguments :initarg :arguments :reader general-arguments :type list)))

;;; Created from certain make-load-form results.
(defclass class-creator (vcreator)
  ((%name :initarg :name :reader class-creator-name)))

(defclass singleton-creator (vcreator) ())

(defclass load-time-value-creator (creator)
  (;; Reference to a function to call to evaluate the load form.
   ;; It's called with no arguments and returns the value.
   (%function :initarg :function :reader load-time-value-creator-function
              :type creator)
   ;; Boolean indicating whether the LTV is read-only. Unused for now.
   (%read-only-p :initarg :read-only-p :type boolean
                 :reader load-time-value-creator-read-only-p)
   ;; The original form, for debugging/display
   (%form :initarg :form :reader load-time-value-creator-form)
   ;; The info object, for similarity checking
   (%info :initarg :info :reader load-time-value-creator-info)))

(defclass init-object-array (instruction)
  ((%count :initarg :count :reader init-object-array-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FASL units
;;;
;;; These are the overall product of this file, and its main interface,
;;; WITH-CONSTANTS and FINISH-FASL-UNIT.
;;; A fasl-unit is conceptually just a sequence of instructions
;;; beginning with an INIT-OBJECT-ARRAY. An actual FASL is the a header followed
;;; by the encoding of one or more FASL-UNITs.

(defclass fasl-unit ()
  ((%init-object-array :reader init-object-array :initarg :init-object-array
                       :type init-object-array)
   (%instructions :reader instructions :initarg :instructions)))

(defgeneric object-count (fasl-unit))
(defmethod object-count ((fasl-unit fasl-unit))
  (init-object-array-count (init-object-array fasl-unit)))

(defgeneric instruction-count (fasl-unit))
(defmethod instruction-count ((fasl-unit fasl-unit))
  (1+ (length (instructions fasl-unit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Attributes are bonus, possibly implementation-defined stuff also in the file.
;;; Based closely on Java attributes, the loader has to ignore any it doesn't
;;; understand, so it's verboten for attributes to do anything semantically
;;; important in general. And, attributes include inline information about their
;;; size, so they can be skipped if not understood.
;;; Unlike Java attributes, our attributes are instructions in the normal
;;; sequence. This is so that, for example, functions can be annotated with
;;; source or other debug information before they are called.

(defclass attribute (effect)
  (;; Creator for the name of the attribute, a string.
   ;; FIXME: Do this more cleanly.
   (%name :reader name :type creator)))

(defclass docstring-attr (attribute)
  ((%name :initform (ensure-constant "docstring"))
   (%object :initarg :object :reader object :type creator)
   (%docstring :initarg :docstring :reader docstring :type creator)))

(defclass name-attr (attribute)
  ((%name :initform (ensure-constant "name"))
   (%object :initarg :object :reader object :type creator)
   (%objname :initarg :objname :reader objname :type creator)))

(defclass lambda-list-attr (attribute)
  ((%name :initform (ensure-constant "lambda-list"))
   (%function :initarg :function :reader ll-function :type creator)
   (%lambda-list :initarg :lambda-list :reader lambda-list :type creator)))

(defclass function-native-attr (attribute)
  ((%name :initform (ensure-constant (function-native-attr-name m:*client*)))
   (%function :initarg :function :reader ll-function :type creator)
   ;; ID number of the native module
   (%module-id :initarg :id :reader module-id :type (unsigned-byte 16))
   ;; A sequence of indices provided by the client,
   ;; with client-specific meaning,
   ;; but which all must be (unsigned-byte 16)
   (%indices :initarg :indices :reader indices)))

(defclass spi-attr (attribute)
  ((%name :initform (ensure-constant "source-pos-info"))
   (%function :initarg :function :reader spi-attr-function :type creator)
   (%pathname :initarg :pathname :reader spi-attr-pathname :type creator)
   (%lineno :initarg :lineno :reader lineno :type (unsigned-byte 64))
   (%column :initarg :column :reader column :type (unsigned-byte 64))
   (%filepos :initarg :filepos :reader filepos :type (unsigned-byte 64))))

;;;

(defclass module-debug-attr (attribute)
  ((%name :initform (ensure-constant "module-debug-info"))
   (%module :initarg :module :reader module)
   ;; A sequence of DEBUG-INFOs (and DEBUG-INFO-FUNCTIONs)
   (%infos :initarg :infos :reader infos :type sequence)))

(defclass debug-info ()
  ((%start :initarg :start :reader start :type (unsigned-byte 32))
   (%end :initarg :end :reader end :type (unsigned-byte 32))))

(defclass debug-info-function ()
  (;; Doesn't inherit from debug-info since the function
   ;; has a start and end already.
   (%function :initarg :function :reader di-function :type creator)))

(defclass debug-info-declarations (debug-info)
  ((%declarations :initarg :declarations :reader declarations)))

(defclass debug-info-the (debug-info)
  ((%type :initarg :type :reader the-type)
   (%receiving :initarg :receiving :reader receiving)))

(defclass debug-info-if (debug-info)
  ((%receiving :initarg :receiving :reader receiving)))

(defclass debug-info-tagbody (debug-info)
  ((%tags :initarg :tags :reader tags)))

(defclass debug-info-block (debug-info)
  ((%name :initarg :name :reader name)
   (%receiving :initarg :receiving :reader receiving)))

(defclass debug-info-var ()
  ((%name :initarg :name :reader name :type creator)
   (%index :initarg :frame-index :reader frame-index :type (unsigned-byte 16))
   (%cellp :initarg :cellp :reader cellp :type boolean)
   (%dynamic-extent-p :initarg :dxp :reader dynamic-extent-p :type boolean)
   (%ignore :initarg :ignore :reader di-ignore
            :type (member nil cl:ignore cl:ignorable))
   (%inline :initarg :inline :reader di-inline
            :type (member nil cl:inline cl:notinline))
   ;; other declarations (type, user defined)
   (%declarations :initarg :declarations :reader declarations :type list)))
(defclass debug-info-vars (debug-info)
  ((%vars :initarg :vars :reader vars :type list)))

(defclass module-native-attr (attribute)
  ((%name :initform (ensure-constant (module-native-attr-name m:*client*)))
   (%module-id :initarg :id :reader module-id :type (unsigned-byte 16))
   ;; The bytecode module creator
   (%module :initarg :module :reader module :type creator)
   ;; The native code as bytes
   (%code :initarg :code :reader code
          :type (simple-array (unsigned-byte 8) (*)))
   ;; Vector of literals (i.e. creators) used by the module.
   (%literals :initarg :literals :reader literals :type simple-vector)))

;;;

;;; If this is true, symbols are avoided when possible, and attributes
;;; are not dumped. Experimental for use with chalybeate.
(defvar *primitive* nil)

;;;

(defgeneric make-load-form (client object &optional environment))
(defmethod make-load-form (client object &optional env)
  (declare (ignore client))
  (cl:make-load-form object env))

;;;

;;; Return true iff the value is similar to the existing creator.
(defgeneric similarp (creator value)
  (:method (creator value) (declare (ignore creator value)) nil))

(defmethod similarp ((creator vcreator) value)
  (eql (prototype creator) value))

(defmethod similarp ((creator load-time-value-creator) ltvi)
  (eql (load-time-value-creator-info creator) ltvi))

;;; EQL hash table from objects to creators.
(defvar *coalesce*)

;;; Another EQL hash table for out-of-band objects that are also "coalesced".
;;; So far this means cfunctions, modules, fcells, and vcells.
;;; This a separate variable because perverse code could use an out-of-band
;;; object in band (e.g. compiling a literal module) and we don't want to
;;; confuse those things.
(defvar *oob-coalesce*)

;;; For function cells. EQUAL since function names can be lists.
(defvar *fcell-coalesce*)
;;; And variable cells.
(defvar *vcell-coalesce*)
;;; Since there's only ever at most one environment cell, it's just
;;; stored directly in this variable rather than a table.
(defvar *environment-coalesce*)

;; Look up a value in the existing instructions.
;; On success returns the creator, otherwise NIL.
;; Could be extended with coalescence relations or made more efficient,
;; for example by multiple tables discriminated by type.
(defun %find-constant (value)
  (values (gethash value *coalesce*))
  #+(or)
  (find-if (lambda (c) (and (typep c 'creator) (similarp c value)))
           sequence))

(defun find-oob (value)
  (values (gethash value *oob-coalesce*)))

(defun find-fcell (name) (values (gethash name *fcell-coalesce*)))
(defun find-vcell (name) (values (gethash name *vcell-coalesce*)))

(defun find-environment () *environment-coalesce*)

;;; List of instructions to be executed by the loader.
;;; In reverse.
(defvar *instructions*)

;;; Stack of objects we are in the middle of computing creation forms for.
;;; This is used to detect circular dependencies.
;;; We only do this for MAKE-LOAD-FORM because we assume our own
;;; computations never recurse inappropriately. If they do, it's a bug,
;;; rather than the user's problem.
(defvar *creating*)

(defmacro with-constants ((&key) &body body)
  `(let ((*instructions* nil) (*creating* nil)
         (*coalesce* (make-hash-table))
         (*oob-coalesce* (make-hash-table))
         (*fcell-coalesce* (make-hash-table :test #'equal))
         (*vcell-coalesce* (make-hash-table))
         (*environment-coalesce* nil))
     ,@body))

;;; Should be called from within WITH-CONSTANTS, obviously.
(defun finish-fasl-unit ()
  (let ((nobjs (count-if (lambda (i) (typep i 'creator)) *instructions*)))
    (make-instance 'fasl-unit
      :init-object-array (make-instance 'init-object-array :count nobjs)
      :instructions (reverse *instructions*))))

(defun find-constant (value)
  (%find-constant value #+(or) *instructions*))

(defun find-constant-index (value)
  (let ((creator (%find-constant value)))
    (if creator
        (index creator)
        nil)))

(defun add-instruction (instruction)
  (push instruction *instructions*)
  instruction)

(defun add-creator (value instruction)
  (setf (gethash value *coalesce*) instruction)
  (add-instruction instruction))

(defun add-oob (key instruction)
  (setf (gethash key *oob-coalesce*) instruction)
  (add-instruction instruction))

(defun add-fcell (key instruction)
  (setf (gethash key *fcell-coalesce*) instruction)
  (add-instruction instruction))

(defun add-vcell (key instruction)
  (setf (gethash key *vcell-coalesce*) instruction)
  (add-instruction instruction))

(defun add-environment (instruction)
  (setf *environment-coalesce* instruction)
  (add-instruction instruction))

(defgeneric add-constant (value))

(defun ensure-constant (value)
  (or (find-constant value) (add-constant value)))

;;; Given a form, get a constant handle to a function that at load time will
;;; have the effect of evaluating the form in a null lexical environment.
(defun add-form (form &optional (env *environment*))
  ;; FORMS-ONLY so that (declare ...) forms for example correctly cause errors.
  (add-function (bytecode-cf-compile-lexpr `(lambda () ,form) env t)))

(defmethod add-constant ((value cons))
  (let ((cons (add-creator
               value (make-instance 'cons-creator :prototype value))))
    (add-instruction (make-instance 'initialize-cons
                       :cons cons
                       :car (ensure-constant (car value))
                       :cdr (ensure-constant (cdr value))))
    cons))

;;; Arrays are encoded with a code describing how elements are packed.
;;; This packing can be done independently of the element type, so
;;; that choice of representation is not dependent on how the host
;;; Lisp happens to represent arrays.
;;; We also use codes for common array element types. This shaves some
;;; bytes from FASLs, but is also important to avoid infinite recursion:
;;; If we just dumped the element type, say BASE-CHAR, we'd have to dump
;;; the symbol, which means having to dump the symbol name, and guess
;;; what kind of object the symbol name is?
;;; CLHS defines UPGRADED-ARRAY-ELEMENT-TYPE to return BASE-CHAR or
;;; equivalent for base char arrays, so the code works fine there.
;;; Ditto CHARACTER, and BIT but that's not as important.
;;; TODO: For version 1, put more thought into these IDs.
(defconstant +other-uaet+   #b00000010)

(defvar +array-packing-codes+
  '((:nil                    #b00000000)
    (:t                      #b00000001)
    ;; other-uaet            #b00000010
    (:base-char              #b00100000)
    (:character              #b00100001)
    (:binary16               #b01000000)
    (:binary32               #b01000001)
    (:binary64               #b01000010)
    (:binary80               #b01000011)
    (:binary128              #b01000111)
    (:complex-binary16       #b01100000)
    (:complex-binary32       #b01100001)
    (:complex-binary64       #b01100010)
    (:complex-binary80       #b01100011)
    (:complex-binary128      #b01100100)
    (:unsigned-byte1         #b10000000)
    (:unsigned-byte2         #b10000001)
    (:unsigned-byte4         #b10000010)
    (:unsigned-byte8         #b10000011)
    (:unsigned-byte16        #b10000100)
    (:unsigned-byte32        #b10000101)
    (:unsigned-byte64        #b10000110)
    (:unsigned-byte128       #b10000111)
    (:signed-byte8           #b10100011)
    (:signed-byte16          #b10100100)
    (:signed-byte32          #b10100101)
    (:signed-byte64          #b10100110)
    (:signed-byte128         #b10100111)))

;;; Mapping from array element types to equivalent packing specs above.
;;; If the element type of an array is not type-equivalent to one of these,
;;; it should be given the other-uaet code instead. This is independent of
;;; how the array is packed.
(defvar +array-uaet-infos+
  '((nil                    :nil)
    (base-char              :base-char)
    (character              :character)
    (single-float           :binary32)
    (double-float           :binary64)
    ((complex single-float) :complex-binary32)
    ((complex double-float) :complex-binary64)
    (bit                    :unsigned-byte1)
    ((unsigned-byte 2)      :unsigned-byte2)
    ((unsigned-byte 4)      :unsigned-byte4)
    ((unsigned-byte 8)      :unsigned-byte8)
    ((unsigned-byte 16)     :unsigned-byte16)
    ((unsigned-byte 32)     :unsigned-byte32)
    ((unsigned-byte 64)     :unsigned-byte64)
    ((signed-byte 8)        :signed-byte8)
    ((signed-byte 16)       :signed-byte16)
    ((signed-byte 32)       :signed-byte32)
    ((signed-byte 64)       :signed-byte64)
    (t                      :t)))

(defun array-packing-info (array)
  ;; TODO? As mentioned above, we could pack arrays more efficiently
  ;; than suggested by their element type. Iterating over every array
  ;; checking might be a little too slow though?
  ;; Also wouldn't work for NIL arrays, but who's dumping NIL arrays?
  (let ((aet (array-element-type array)))
    (dolist (info +array-uaet-infos+)
      (when (subtypep aet (first info))
        (return-from array-packing-info
          (assoc (second info) +array-packing-codes+))))
    ;; unreachable, but just for sanity's sake
    (assoc t +array-packing-codes+)))

(defun compute-element-type-info (array)
  (let ((aet (array-element-type array)))
    (dolist (info +array-uaet-infos+)
      ;; Check for actual type equality.
      ;; We do type= instead of just equal because some implementations,
      ;; like CLASP and ECL, return nonstandard specifiers from a-e-t.
      (when (and (subtypep aet (first info))
                 (subtypep (first info) aet))
        (return-from compute-element-type-info
          (second (assoc (second info) +array-packing-codes+)))))
    ;; The element type is something we don't specially code for.
    ;; Dump it as a constant and use +other-uaet+.
    (list (ensure-constant aet) +other-uaet+)))

(defmethod add-constant ((value array))
  (multiple-value-bind (dims total-size)
      ;; We dump all arrays as simple, which means we need to ignore anything
      ;; past the fill pointer.
      (if (array-has-fill-pointer-p value)
          (let ((len (length value))) (values (list len) len))
          (values (array-dimensions value) (array-total-size value)))
    (let* ((element-type-info (compute-element-type-info value))
           (info (array-packing-info value))
           (info-type (first info))
           (arr (add-creator
                 value
                 (make-instance 'array-creator
                   :prototype value :dimensions dims
                   :packing-info info :element-type-info element-type-info))))
      (when (eq info-type :t) ; general - dump setf-arefs for elements.
        ;; (we have to separate initialization here in case the array
        ;;  contains itself. packed arrays can't contain themselves)
        (add-instruction
         (make-instance 'initialize-array
           :array arr
           :values (loop for i below total-size
                         for e = (row-major-aref value i)
                         collect (ensure-constant e)))))
      arr)))

(defun utf8-length (string)
  (loop for c across string
        for cpoint = (char-code c)
        sum (cond ((< cpoint #x80) 1)
                  ((< cpoint #x800) 2)
                  ((< cpoint #x10000) 3)
                  ((< cpoint #x110000) 4)
                  #-sbcl ; whines about deleted code
                  (t (error "Codepoint #x~x for ~:c too big" cpoint c)))))

(defmethod add-constant ((value string))
  (case (array-element-type value)
    (base-char (let ((L (length value)))
                 (if (< L #.(ash 1 16))
                     ;; FIXME: Check that characters are all ASCII?
                     (add-creator
                      value
                      (make-instance 'base-string-creator
                        :prototype value))
                     (call-next-method))))
    (character (let ((L (utf8-length value)))
                 (if (< L #.(ash 1 16))
                     (add-creator
                      value
                      (make-instance 'utf8-string-creator
                        :nbytes L
                        :prototype value))
                     (call-next-method))))
    (otherwise (call-next-method))))

(defmethod add-constant ((value hash-table))
  (let* ((count (hash-table-count value))
         (ht (add-creator
              value
              (make-instance 'hash-table-creator :prototype value
                             :test (hash-table-test value)
                             :count count)))
         (alist nil))
    (unless (zerop count) ; empty hash table, so nothing to initialize
      (maphash (lambda (k v)
                 (let ((ck (ensure-constant k)) (cv (ensure-constant v)))
                   (push (cons ck cv) alist)))
               value)
      (add-instruction
       (make-instance 'initialize-hash-table
         :table ht :count count :alist alist)))
    ht))

(defmethod add-constant ((value symbol))
  (add-creator
   value
   (let ((package (symbol-package value)))
     (if package
         (make-instance 'interned-symbol-creator
           :prototype value
           :name (ensure-constant (symbol-name value))
           :package (ensure-constant package))
         (make-instance 'symbol-creator
           :prototype value
           :name (ensure-constant (symbol-name value)))))))

(defmethod add-constant ((value (eql nil)))
  (add-creator value (make-instance 'singleton-creator :prototype value)))
(defmethod add-constant ((value (eql t)))
  (add-creator value (make-instance 'singleton-creator :prototype value)))

(defmethod add-constant ((value package))
  (add-creator value
               (make-instance 'package-creator
                 :prototype value
                 :name (ensure-constant
                        (package-name m:*client* *environment* value)))))

(defmethod add-constant ((value integer))
  (add-creator
   value
   (etypecase value
     ;; TODO? Could have different opcodes for smaller integers.
     ((signed-byte 64) (make-instance 'sb64-creator :prototype value))
     (integer (make-instance 'bignum-creator :prototype value)))))

(defmethod add-constant ((value float))
  (add-creator
   value
   ;; NOTE: The order here is important, because we want to handle
   ;; merged float types. See explanation in encode.lisp.
   (etypecase value
     (single-float (make-instance 'single-float-creator :prototype value))
     (double-float (make-instance 'double-float-creator :prototype value))
     (short-float (make-instance 'short-float-creator :prototype value))
     (long-float (make-instance 'long-float-creator :prototype value)))))

(defmethod add-constant ((value ratio))
  ;; In most cases it's probably pointless to try to coalesce the numerator
  ;; and denominator. It would probably be smarter to have a small ratio
  ;; where the number is embedded versus a large ratio where they're indirect.
  (add-creator
   value
   (make-instance 'ratio-creator :prototype value
                  :numerator (ensure-constant (numerator value))
                  :denominator (ensure-constant (denominator value)))))

(defmethod add-constant ((value complex))
  ;; Similar considerations to ratios here.
  (add-creator
   value
   (make-instance 'complex-creator :prototype value
                  :realpart (ensure-constant (realpart value))
                  :imagpart (ensure-constant (imagpart value)))))

(defmethod add-constant ((value character))
  (add-creator value (make-instance 'character-creator :prototype value)))

(defmethod add-constant ((value pathname))
  (add-creator
   value
   (make-instance 'pathname-creator
     :prototype value
     :host (ensure-constant (host-namestring value))
     :device (ensure-constant (pathname-device value))
     :directory (ensure-constant (pathname-directory value))
     :name (ensure-constant (pathname-name value))
     :type (ensure-constant (pathname-type value))
     :version (ensure-constant (pathname-version value)))))

(define-condition circular-dependency (error)
  ((%path :initarg :path :reader path))
  (:report (lambda (condition stream)
             (format stream "~s circular dependency detected:~%~t~{~s~^ ~}"
                     'make-load-form (path condition)))))

(defconstant +max-call-args+ (ash 1 16))

(defun function-form-p (form)
  (and (consp form) (eq (car form) 'cl:function)
       (consp (cdr form)) (null (cddr form))))

(defun lambda-expression-p (form)
  (and (consp form) (eq (car form) 'cl:lambda)))

;;; Return true iff the proper list FORM represents a call to a global
;;; function with all constant or #' arguments (and not too many).
(defun call-with-dumpable-arguments-p (form &optional (env *environment*))
  (declare (ignorable env))
  (and (symbolp (car form))
       (fboundp (car form))
       (not (macro-function (car form)))
       (not (special-operator-p (car form)))
       (< (length (rest form)) +max-call-args+)
       (every (lambda (f) (or (cmp:constantp f env)
                              (function-form-p f)
                              (lambda-expression-p f)))
              (rest form))))

(defun f-dumpable-form-creator (env)
  (lambda (form)
    (cond ((lambda-expression-p form)
           (add-function (bytecode-cf-compile-lexpr form env)))
          ((not (function-form-p form)) ; must be a constant
           (ensure-constant (cmp:eval form env)))
          ((and (consp (second form)) (eq (caadr form) 'cl:lambda))
           ;; #'(lambda ...)
           (add-function (bytecode-cf-compile-lexpr (second form) env)))
          (t
           ;; #'function-name
           (add-instruction
            (make-instance 'fdefinition-lookup
              :name (ensure-constant (second form))))))))

;; from cleavir
(defun proper-list-p (object)
  (typecase object
    (null t)
    (cons (let ((slow object)
                (fast (cdr object)))
            (declare (type cons slow))
            (tagbody
             again
               (unless (consp fast)
                 (return-from proper-list-p
                   (if (null fast) t nil)))
               (when (eq fast slow)
                 (return-from proper-list-p nil))
               (setq fast (cdr fast))
               (unless (consp fast)
                 (return-from proper-list-p
                   (if (null fast) t nil)))
               (setq fast (cdr fast))
               (setq slow (cdr slow))
               (go again))))
    (t nil)))

;;; Make a possibly-special creator based on an MLF creation form.
(defun creation-form-creator (value form &optional (env *environment*))
  (let ((*creating* (cons value *creating*)))
    (flet ((default ()
             (make-instance 'general-creator
               :prototype value
               :function (add-form form env) :arguments ())))
      (cond ((not (proper-list-p form)) (default))
            ;; (find-class 'something)
            ((and (eq (car form) 'cl:find-class)
                  (= (length form) 2)
                  (cmp:constantp (second form) env))
             (make-instance 'class-creator
               :prototype value
               :name (ensure-constant (cmp:eval (second form) env))))
            ;; (foo 'bar 'baz)
            ((call-with-dumpable-arguments-p form)
             (make-instance 'general-creator
               :prototype value
               :function (add-instruction
                          (make-instance 'fdefinition-lookup
                            :name (ensure-constant (car form))))
               :arguments (mapcar (f-dumpable-form-creator env) (rest form))))
            (t (default))))))

;;; Make a possibly-special initializer.
(defun add-initializer-form (form &optional (env (cmp:make-null-lexical-environment *environment*)))
  (flet ((default ()
           (add-instruction
            (make-instance 'general-initializer
              :function (add-form form env) :arguments ()))))
    (cond ((cmp:constantp form env) nil) ; do nothing (good for e.g. defun's return)
          ((not (proper-list-p form)) (default))
          ((call-with-dumpable-arguments-p form env)
           (let ((cre (f-dumpable-form-creator env)))
             (if (eq (car form) 'cl:funcall)
                 ;; cut off the funcall - general-initializer does the call itself.
                 ;; this commonly arises from e.g. (funcall #'(setf fdefinition ...)
                 (add-instruction
                  (make-instance 'general-initializer
                    :function (funcall cre (second form))
                    :arguments (mapcar cre (cddr form))))
                 (add-instruction
                  (make-instance 'general-initializer
                    :function (add-instruction
                               (make-instance 'fdefinition-lookup
                                 :name (ensure-constant (car form))))
                    :arguments (mapcar cre (rest form)))))))
           (t (default)))))

(defmethod add-constant ((value t))
  (when (member value *creating*)
    (error 'circular-dependency :path *creating*))
  (multiple-value-bind (create initialize) (make-load-form m:*client* value)
    (prog1
        (add-creator value (creation-form-creator value create))
      (add-initializer-form initialize))))

;;; Loop over the instructions, assigning indices to the creators.
;;; This only affects their position in the similarity vector,
;;; not the order the instructions must be executed in.
;;; The instructions must be in forward order, i.e. reversed from how they're
;;; pushed in above. (FIXME: The reversal is too awkward.)
;;; This could probably be done in one pass somehow?
(defun assign-indices (instructions)
  (let ((next-index 0))
    (map nil (lambda (inst)
               (when (and (typep inst 'creator) (not (index inst)))
                 (setf (index inst) next-index next-index (1+ next-index))))
         instructions))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File compiler
;;;   

(defun bytecode-cf-compile-lexpr (lambda-expression environment
                                  &optional forms-only)
  (if forms-only
      (cmp:compile-into (cmp:make-cmodule) lambda-expression environment
                        :declarations nil)
      (cmp:compile-into (cmp:make-cmodule) lambda-expression environment)))

(defun compile-file-form (form env)
  (add-initializer-form form env))

(defclass bytefunction-creator (creator)
  ((%cfunction :initarg :cfunction :reader cfunction)
   (%module :initarg :module :reader module)
   (%nlocals :initarg :nlocals :reader nlocals :type (unsigned-byte 16))
   (%nclosed :initarg :nclosed :reader nclosed :type (unsigned-byte 16))
   (%entry-point :initarg :entry-point :reader entry-point
                 :type (unsigned-byte 32))
   (%size :initarg :size :reader size :type (unsigned-byte 32))))

;;; Given a CFUNCTION, generate a creator for the eventual runtime function.
(defun add-function (value)
  (let ((inst
          (add-oob
           value
           (make-instance 'bytefunction-creator
             :cfunction value
             :module (ensure-module (cmp:cfunction-cmodule value))
             :nlocals (cmp:cfunction-nlocals value)
             :nclosed (length (cmp:cfunction-closed value))
             :entry-point (cmp:cfunction-final-entry-point value)
             :size (cmp:cfunction-final-size value)))))
    ;; Something to consider: Any of these, but most likely the lambda list,
    ;; could contain unexternalizable data. In this case we should find a way
    ;; to gracefully and silently not dump the attribute.
    (when (cmp:cfunction-name value)
      (add-instruction (make-instance 'name-attr
                         :object inst
                         :objname (ensure-constant
                                   (cmp:cfunction-name value)))))
    (unless *primitive*
      (when (cmp:cfunction-doc value)
        (add-instruction (make-instance 'docstring-attr
                           :object inst
                           :docstring (ensure-constant
                                       (cmp:cfunction-doc value)))))
      (when (cmp:cfunction-lambda-list-p value)
        (add-instruction (make-instance 'lambda-list-attr
                           :function inst
                           :lambda-list (ensure-constant
                                         (cmp:cfunction-lambda-list value)))))
      (when (cmp:cfunction-source value)
        (multiple-value-bind (path lineno column pos)
            (source-location-data m:*client* (cmp:cfunction-source value))
          (add-instruction
           (make-instance 'spi-attr
             :function inst
             :pathname (ensure-constant path)
             :lineno lineno :column column :filepos pos)))))
    inst))

(defclass bytemodule-creator (vcreator)
  ((%cmodule :initarg :cmodule :reader bytemodule-cmodule)
   (%lispcode :initform nil :initarg :lispcode :reader bytemodule-lispcode)))

(defmethod print-object ((object bytemodule-creator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d" (index object)))
  object)

(defclass setf-literals (effect)
  ((%module :initarg :module :reader setf-literals-module :type creator)
   ;; The literals are not practically coalesceable and are always a T vector,
   ;; so they're just encoded inline.
   (%literals :initarg :literals :reader setf-literals-literals
              :type simple-vector)))

(defgeneric ensure-module-literal (literal-info))

(defmethod ensure-module-literal ((info cmp:constant-info))
  (ensure-constant (cmp:constant-info-value info)))

(defun ensure-function (cfunction)
  (or (find-oob cfunction) (add-function cfunction)))

(defmethod ensure-module-literal ((info cmp:cfunction))
  (ensure-function info))

(defmethod ensure-module-literal ((info cmp:ltv-info))
  (add-instruction
   (make-instance 'load-time-value-creator
     :function (add-form (cmp:ltv-info-form info))
     :read-only-p (cmp:ltv-info-read-only-p info)
     :form (cmp:ltv-info-form info)
     :info info)))

(defun ensure-fcell (name)
  (or (find-fcell name)
    (add-fcell name
               (make-instance 'fcell-lookup
                 :name (ensure-constant name)))))

(defmethod ensure-module-literal ((info cmp:fdefinition-info))
  (ensure-fcell (cmp:fdefinition-info-name info)))

(defun ensure-vcell (name)
  (or (find-vcell name)
      (add-vcell name
                 (make-instance 'vcell-lookup
                   :name (ensure-constant name)))))

(defmethod ensure-module-literal ((info cmp:value-cell-info))
  (ensure-vcell (cmp:value-cell-info-name info)))

(defmethod ensure-module-literal ((info cmp:env-info))
  (or (find-environment)
      (add-environment (make-instance 'environment-lookup))))

(defgeneric process-debug-info (info))

(defmethod process-debug-info ((info cmp:cfunction))
  (make-instance 'debug-info-function
    :function (ensure-function info)))

(defmethod process-debug-info ((info m:declarations-info))
  (make-instance 'debug-info-declarations
    :start (m:start info) :end (m:end info)
    :declarations (ensure-constant (m:declarations info))))

(defmethod process-debug-info ((info m:the-info))
  (make-instance 'debug-info-the
    :start (m:start info) :end (m:end info)
    :type (ensure-constant (m:the-type info))
    :receiving (m:receiving info)))

(defmethod process-debug-info ((info m:if-info))
  (make-instance 'debug-info-if
    :start (m:start info) :end (m:end info)
    :receiving (m:receiving info)))

(defmethod process-debug-info ((info m:tagbody-info))
  (make-instance 'debug-info-tagbody
    :start (m:start info) :end (m:end info)
    :tags (loop for (tag . ip) in (m:tags info)
                collect (cons (ensure-constant tag) ip))))

(defmethod process-debug-info ((info m:block-info))
  (make-instance 'debug-info-block
    :start (m:start info) :end (m:end info)
    :name (ensure-constant (m:name info))
    :receiving (m:receiving info)))

(defmethod process-debug-info ((info m:vars-info))
  (make-instance 'debug-info-vars
    :start (m:start info) :end (m:end info)
    :vars (loop for var in (m:bindings info)
                for adecls = (m:declarations var)
                for ignore = (loop for d in adecls
                                   when (member d '(cl:ignore cl:ignorable))
                                     return d)
                for inline = (loop for d in adecls
                                   when (member d '(cl:inline cl:notinline))
                                     return d)
                for decls = (set-difference adecls
                                            '(cl:dynamic-extent cl:ignore
                                              cl:ignorable cl:inline
                                              cl:notinline))
                collect (make-instance 'debug-info-var
                          :name (ensure-constant (m:name var))
                          :frame-index (m:index var) :cellp (m:cellp var)
                          :dxp (not (not (member 'cl:dynamic-extent adecls)))
                          :ignore ignore :inline inline
                          :declarations (mapcar #'ensure-constant decls)))))

(defun process-debug-infos (pc-map)
  ;; Doing source info will require a format for it, so we only dump some infos.
  (loop for info across pc-map
        if (typep info '(or cmp:cfunction m:program-structure-info))
          collect (process-debug-info info)))

(defvar *module-native-compiler* nil)
(defvar *module-native-id*)

;;; Native modules need to be correctly ordered, so make sure we don't
;;; start compiling a native module before setting up the previous one.
;;; This can only really happen from load-time-value constants, and those
;;; functions don't really need to be executed quickly anyway.
(defvar *recursive-native* nil)

(defun add-module (value)
  (multiple-value-bind (bytecode pc-map) (cmp:link value)
    ;; Add the module first to prevent recursion.
    (let ((mod
            (add-oob
             value
             (make-instance 'bytemodule-creator
               :prototype value :lispcode bytecode))))
      ;; Modules can indirectly refer to themselves recursively through
      ;; cfunctions, so we need to 2stage it here.
      (add-instruction
       (make-instance 'setf-literals
         :module mod :literals (map 'simple-vector #'ensure-module-literal
                                    (cmp:cmodule-literals value))))
      (add-instruction
       (make-instance 'module-debug-attr
         :module mod :infos (process-debug-infos pc-map)))
      ;; If the client is providing a native compiler, use it.
      (when (and *module-native-compiler* (not *recursive-native*))
        (handler-case
            (let* ((id *module-native-id*)
                   (*recursive-native* t)
                   (nmodule (funcall *module-native-compiler*
                                     bytecode (cmp:cmodule-literals value)
                                     pc-map id)))
              (incf *module-native-id*)
              (add-instruction
               (make-instance 'module-native-attr
                 :module mod :id id
                 :code (native-module-code nmodule)
                 :literals (map 'vector #'ensure-module-literal
                                (native-module-literals nmodule))))
              ;; Add attributes for the functions as well.
              ;; We do this here instead of with the functions
              ;; in order to skip any recursion problems with functions
              ;; referring to modules, etc.
              ;; It's possible that a bytecode function does not appear
              ;; in the fmap. This can occur because e.g. it was inlined
              ;; away. That's ok, it just means we don't dump an attr for it.
              (loop for (f . indices) in (native-module-fmap nmodule)
                    do (add-instruction
                        (make-instance 'function-native-attr
                          :function (ensure-function f)
                          :id id :indices indices))))
          (serious-condition (e)
            ;; native modules are just attributes and so fundamentally optional.
            ;; Compiling is also complicated and error prone. So if we get a
            ;; serious condition warn about it.
            ;; Hypothetically this could just be a compiler note, if it's about a
            ;; compiler bug.
            (warn "Unhandled serious condition while compiling native module:~%~a
Abandoning further work on it and moving on."
                  e))))
      mod)))

(defgeneric native-module-code (native-module))
(defgeneric native-module-literals (native-module))
(defgeneric native-module-fmap (native-module))

(defun ensure-module (module)
  (or (find-oob module) (add-module module)))
