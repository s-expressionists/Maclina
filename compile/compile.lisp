(in-package #:cvm.compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions, modules, LTV, contexts
;;;

(defstruct (cfunction (:constructor make-cfunction (cmodule)))
  cmodule
  ;; Bytecode vector for this function.
  (bytecode (make-array 0 :element-type '(unsigned-byte 8)
                          :fill-pointer 0 :adjustable t))
  ;; An ordered vector of annotations emitted in this function.
  (annotations (make-array 0 :fill-pointer 0 :adjustable t))
  (nlocals 0)
  (closed (make-array 0 :fill-pointer 0 :adjustable t))
  (entry-point (make-label))
  ;; The position of the start of this function in this module
  ;; (optimistic).
  position
  ;; How much to add to the bytecode vector length for increased fixup
  ;; sizes for the true length.
  (extra 0)
  ;; The index of this function in the containing module's function
  ;; vector.
  index
  info)

(defun cfunction-final-size (cfunction)
  (+ (length (cfunction-bytecode cfunction)) (cfunction-extra cfunction)))

(defstruct (cmodule (:constructor make-cmodule ()))
  (cfunctions (make-array 1 :fill-pointer 0 :adjustable t))
  ;; Each entry in this vector is either a constant-info, an ltv-info,
  ;; a global-function-reference, or a cfunction.
  ;; The compiler treats them all pretty identically, but the linker
  ;; needs to distinguish these things.
  ;; For example, a cfunction appearing literally in the code (for whatever
  ;; odd reason) gets a constant-info, distinguishing it from a cfunction
  ;; in the vector which will be linked to an actual function.
  (literals (make-array 0 :fill-pointer 0 :adjustable t)))

(defstruct (constant-info (:constructor make-constant-info (value)))
  value)

(defstruct (ltv-info (:constructor make-ltv-info (form read-only-p)))
  form read-only-p)

;;; Info about a name that we use for FDEFINITION.
;;; This is separate from CONSTANT-INFO because some clients can do better
;;; than a full call to CL:FDEFINITION on the actual name, e.g. with cells.
(defstruct (fdefinition-info (:constructor make-fdefinition-info (name)))
  name)

;;; Info about a global symbol value.
;;; For example, the linker may want to make it a cell.
(defstruct (value-cell-info (:constructor make-value-cell-info (name)))
  name)

;;; This info represents the loader environment. It is used for a few
;;; instructions that need to perform runtime name lookups.
;;; Any constants vector has at most one of these, since everything is
;;; after all being loaded into the same environment.
(defstruct (env-info (:constructor make-env-info ())))

;;; The context contains information about what the current form needs
;;; to know about what it is enclosed by.
(defstruct context
  ;; either an integer, meaning that many values, or T, meaning all
  receiving
  ;; A list of lexical variable infos and symbols. :special means a special
  ;; variable binding is in place, while a lexical variable info is the variable
  ;; for a tagbody or block dynenv. :catch means a catch.
  ;; Note that the symbol may not be the special variable in question, since
  ;; we don't really need that information.
  ;; Since this is only used for exits, it may not include specials bound by
  ;; a function's lambda list.
  (dynenv nil)
  ;; The next available register index.
  (frame-end 0)
  ;; The cfunction we're compiling.
  function)

(defun context-module (context)
  (cfunction-cmodule (context-function context)))

(defun context-assembly (context)
  (cfunction-bytecode (context-function context)))

(defun find-literal-index (literal literals)
  (loop for i from 0
        for info across literals
        when (and (constant-info-p info)
                  (eql (constant-info-value info) literal))
          return i))

(defun literal-index (literal context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (find-literal-index literal literals)
        (vector-push-extend (make-constant-info literal) literals))))

;;; Force a literal into the end of the literals even if it's already
;;; there. This is used in keyword argument parsing and load-time-value.
(defun new-literal-index (literal context)
  (vector-push-extend (make-constant-info literal)
                      (cmodule-literals (context-module context))))

(defun find-fdefinition-index (function-name literals)
  (loop for i from 0
        for info across literals
        when (and (fdefinition-info-p info)
                  (equal (fdefinition-info-name info) function-name))
          return i))

(defun fdefinition-index (function-name context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (find-fdefinition-index function-name literals)
        (vector-push-extend (make-fdefinition-info function-name) literals))))

;;; Like literal-index, but for cfunctions (that will be linked as functions)
(defun cfunction-literal-index (cfunction context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (position cfunction literals)
        (vector-push-extend cfunction literals))))

(defun find-value-cell-index (name literals)
  (loop for i from 0
        for info across literals
        when (and (value-cell-info-p info)
                  (equal (value-cell-info-name info) name))
          return i))

(defun value-cell-index (name context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (find-value-cell-index name literals)
        (vector-push-extend (make-value-cell-info name) literals))))

;;; Like literal-index but for LTVs.
;;; We don't bother coalescing load-time-value forms so this is trivial.
(defun ltv-index (ltv-info context)
  (vector-push-extend ltv-info (cmodule-literals (context-module context))))

(defun env-index (context)
  (let ((literals (cmodule-literals (context-module context))))
    (or (position-if (lambda (lit) (typep lit 'env-info)) literals)
        (vector-push-extend (make-env-info) literals))))

(defun closure-index (info context)
  (let ((closed (cfunction-closed (context-function context))))
    (or (position info closed)
        (vector-push-extend info closed))))

(defun new-context (parent &key (receiving (context-receiving parent))
                             (dynenv nil) ; prepended
                             (frame-end nil fep) ; added
                             (function (context-function parent)))
  (make-context :receiving receiving
                :dynenv (append dynenv (context-dynenv parent))
                :frame-end (+ (if fep frame-end 0)
                              (context-frame-end parent))
                :function function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembly
;;;

;; An annotation in the function.
(defstruct annotation
  ;; The function containing this annotation.
  function
  ;; The index of this annotation in its function's annotations.
  index
  ;; The current (optimistic) position of this annotation in this function.
  position
  ;; The initial position of this annotaiton in this function.
  initial-position)

(defstruct (label (:include annotation)))

(defstruct (fixup (:include annotation)
                  (:constructor make-fixup (label initial-size emitter resizer
                                            &aux (size initial-size))))
  ;; The label this fixup references.
  label
  ;; The current (optimistic) size of this fixup in bytes.
  size
  ;; The initial size of this fixup in bytes.
  initial-size
  ;; How to emit this fixup once sizes are resolved.
  emitter
  ;; How to resize this fixup. Returns the new size.
  resizer)

(defmethod print-object ((label label) stream)
  (print-unreadable-object (label stream :identity t)
    (format stream "LABEL :POSITION ~d" (annotation-position label))))

(defmethod print-object ((fixup fixup) stream)
  (print-unreadable-object (fixup stream :identity t)
    (format stream "FIXUP :POSITION ~d :SIZE ~d"
            (annotation-position fixup)
            (fixup-size fixup))))

;;; Optimistic positioning of ANNOTATION in its module.
(defun annotation-module-position (annotation)
  (+ (cfunction-position (annotation-function annotation))
     (annotation-position annotation)))

;;; The (module) displacement from this fixup to its label,
(defun fixup-delta (fixup)
  (- (annotation-module-position (fixup-label fixup))
     (annotation-module-position fixup)))

(defun emit-label (context label)
  (setf (label-position label) (length (context-assembly context)))
  (let ((function (context-function context)))
    (setf (label-function label) function)
    (setf (label-index label)
          (vector-push-extend label (cfunction-annotations function)))))

(defun values-less-than-p (values max)
  (dolist (value values t)
    (unless (<= 0 value (1- max)) (return-from values-less-than-p nil))))

(defun assemble-maybe-long (context opcode &rest values)
  (let ((assembly (context-assembly context)))
    (cond ((values-less-than-p values #.(ash 1 8))
           (vector-push-extend opcode assembly)
           (dolist (value values)
             (vector-push-extend value assembly)))
          ((values-less-than-p values #.(ash 1 16))
           (vector-push-extend m:long assembly)
           (vector-push-extend opcode assembly)
           (dolist (value values)
             (vector-push-extend (ldb (byte 8 0) value) assembly)
             (vector-push-extend (ldb (byte 8 8) value) assembly)))
          (t
           (error "Bytecode compiler limit reached: Indices too large! ~a" values)))))

(defun assemble (context &rest values)
  (let ((assembly (context-assembly context)))
    (dolist (value values)
      (vector-push-extend value assembly))))

(defun assemble-into (code position &rest values)
  (do ((values values (rest values))
       (position position (1+ position)))
      ((null values))
    (setf (aref code position) (first values))))

;;; Write WORD of bytesize SIZE to VECTOR at POSITION.
(defun write-le-unsigned (vector word size position)
  (let ((end (+ position size)))
    (do ((position position (1+ position))
         (word word (ash word -8)))
        ((>= position end))
      (setf (aref vector position) (logand word #xff)))))

;;; Emit FIXUP into CONTEXT.
(defun emit-fixup (context fixup)
  (let* ((assembly (context-assembly context))
         (cfunction (context-function context))
         (position (length assembly)))
    (setf (fixup-function fixup) cfunction)
    (setf (fixup-initial-position fixup) position)
    (setf (fixup-position fixup) position)
    (setf (fixup-index fixup)
          (vector-push-extend fixup (cfunction-annotations cfunction)))
    (dotimes (i (fixup-initial-size fixup))
      (vector-push-extend 0 assembly))))

(defun control+label-emitter (fixup position code opcode8 opcode16 opcode24)
  (let* ((size (fixup-size fixup))
         (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
    (setf (aref code position)
          (cond ((eql size 2) opcode8)
                ((eql size 3) opcode16)
                ((eql size 4) opcode24)
                (t (error "Unknown size ~d" size))))
    (write-le-unsigned code offset (1- size) (1+ position))))

(defun control+label-resizer (fixup)
  (let ((delta (fixup-delta fixup)))
    (cond ((typep delta '(signed-byte 8)) 2)
          ((typep delta '(signed-byte 16)) 3)
          ((typep delta '(signed-byte 24)) 4)
          (t (error "???? PC offset too big ????")))))

;;; Emit OPCODE and then a label reference.
(defun emit-control+label (context opcode8 opcode16 opcode24 label)
  (flet ((emitter (fixup position code)
           (control+label-emitter fixup position code
                                  opcode8 opcode16 opcode24)))
    (emit-fixup context (make-fixup label 2 #'emitter #'control+label-resizer))))

(defun emit-jump (context label)
  (emit-control+label context m:jump-8 m:jump-16 m:jump-24 label))
(defun emit-jump-if (context label)
  (emit-control+label context m:jump-if-8 m:jump-if-16 m:jump-if-24 label))
(defun emit-exit (context label)
  (emit-control+label context m:exit-8 m:exit-16 m:exit-24 label))
(defun emit-catch (context label)
  (emit-control+label context m:catch-8 m:catch-16 nil label))

(defun emit-jump-if-supplied (context index label)
  (flet ((emitter (fixup position code)
           (let* ((size (fixup-size fixup))
                  (offset (unsigned (fixup-delta fixup) (* 8 (1- size)))))
             (setf (aref code position)
                   (ecase size
                     (3 m:jump-if-supplied-8)
                     (4 m:jump-if-supplied-16)))
             (setf (aref code (1+ position)) index)
             (write-le-unsigned code offset (- size 2) (+ position 2))))
         (resizer (fixup)
           (typecase (fixup-delta fixup)
             ((signed-byte 8) 3)
             ((signed-byte 16) 4)
             (t (error "???? PC offset too big ????")))))
    (emit-fixup context (make-fixup label 3 #'emitter #'resizer))))

(defun emit-const (context index)
  (if (> index 255)
      (assemble context
        m:long m:const
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context m:const index)))

(defun emit-fdefinition (context index)
  (if (> index 255)
      (assemble context
        m:long m:fdefinition
        (logand index #xff) (logand (ash index -8) #xff))
      (assemble context m:fdefinition index)))

(defun emit-parse-key-args (context max-count key-count key-names aok-p)
  (if (<= key-count 127)
      (assemble context m:parse-key-args
                max-count
                (if aok-p (boole boole-ior 128 key-count) key-count)
                (literal-index (first key-names) context)
                (context-frame-end context))
      (error "Handle more than 127 keyword parameters - you need ~s" key-count)))

(defun emit-bind (context count offset)
  (cond ((= count 1) (assemble context m:set offset))
        ((= count 0))
        (t (assemble context m:bind count offset))))

(defun emit-call (context count)
  (let ((receiving (context-receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble context m:call count))
          ((eql receiving 1)
           (assemble context m:call-receive-one count))
          (t (assemble context m:call-receive-fixed count receiving)))))

(defun emit-mv-call (context)
  (let ((receiving (context-receiving context)))
    (cond ((or (eql receiving t) (eql receiving 0))
           (assemble context m:mv-call))
          ((eql receiving 1)
           (assemble context m:mv-call-receive-one))
          (t (assemble context m:mv-call-receive-fixed receiving)))))

(defun emit-special-bind (context symbol)
  (assemble context m:special-bind (value-cell-index symbol context)))

(defun emit-unbind (context count)
  (dotimes (_ count)
    (assemble context m:unbind)))

(flet ((maybe-emit (lexical-var opcode context)
         (assert lexical-var)
         (flet ((emitter (fixup position code)
                  (assert (= (fixup-size fixup) 1))
                  (setf (aref code position) opcode))
                (resizer (fixup)
                  (declare (ignore fixup))
                  (if (indirect-lexical-p lexical-var) 1 0)))
           (emit-fixup context
                       (make-fixup lexical-var 0 #'emitter #'resizer)))))
  (defun maybe-emit-make-cell (lexical-var context)
    (maybe-emit lexical-var m:make-cell context))
  (defun maybe-emit-cell-ref (lexical-var context)
    (maybe-emit lexical-var m:cell-ref context)))

;;; FIXME: This is probably a good candidate for a specialized
;;; instruction.
(defun maybe-emit-encage (lexical-var context)
  (let ((index (frame-offset lexical-var)))
    (flet ((emitter (fixup position code)
             (assert (= (fixup-size fixup) 5))
             (assemble-into code position
                            m:ref index m:make-cell m:set index))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-var) 5 0)))
      (emit-fixup context (make-fixup lexical-var 0 #'emitter #'resizer)))))

(defun emit-lexical-set (lexical-var context)
  (let ((index (frame-offset lexical-var)))
    (flet ((emitter (fixup position code)
             (if (= (fixup-size fixup) 3)
                 (assemble-into code position m:ref index m:cell-set)
                 (assemble-into code position m:set index)))
           (resizer (fixup)
             (declare (ignore fixup))
             (if (indirect-lexical-p lexical-var) 3 2)))
      (emit-fixup context (make-fixup lexical-var 2 #'emitter #'resizer)))))

(defun constant-resizer (n) (lambda (fixup) (declare (ignore fixup)) n))

(defun emit-entry-or-save-sp (context dynenv-info)
  (let ((index (frame-offset dynenv-info)))
    (flet ((emitter (fixup position code)
             (declare (ignore fixup))
             (if (closed-over-p dynenv-info)
                 (assemble-into code position m:entry index)
                 (assemble-into code position m:save-sp index))))
      (emit-fixup context (make-fixup dynenv-info 2 #'emitter (constant-resizer 2))))))

(defun emit-ref-or-restore-sp (context dynenv-info)
  (let ((index (frame-offset dynenv-info)))
    (flet ((emitter (fixup position code)
             (declare (ignore fixup))
             (if (closed-over-p dynenv-info)
                 (assemble-into code position m:ref index)
                 (assemble-into code position m:restore-sp index))))
      (emit-fixup context (make-fixup dynenv-info 2 #'emitter (constant-resizer 2))))))

(defun emit-exit-or-jump (context dynenv-info label)
  (flet ((emitter (fixup position code)
           (if (closed-over-p dynenv-info)
               (control+label-emitter fixup position code
                                      m:exit-8 m:exit-16 m:exit-24)
               (control+label-emitter fixup position code
                                      m:jump-8 m:jump-16 m:jump-24))))
    (emit-fixup context (make-fixup label 2 #'emitter #'control+label-resizer))))

(defun maybe-emit-entry-close (context dynenv-info)
  (flet ((emitter (fixup position code)
           (assert (= (fixup-size fixup) 1))
           (assemble-into code position m:entry-close))
         (resizer (fixup)
           (declare (ignore fixup))
           (if (closed-over-p dynenv-info) 1 0)))
    (emit-fixup context (make-fixup dynenv-info 0 #'emitter #'resizer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environments
;;;

(defstruct (lexical-environment (:constructor make-null-lexical-environment
                                    (global-environment))
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An alist of (var . var-info) in the current environment.
  (vars nil :type list)
  ;; An alist of (tag tag-dynenv . label) in the current environment.
  (tags nil :type list)
  ;; An alist of (block block-dynenv . label) in the current environment.
  (blocks nil :type list)
  ;; An alist of (fun . fun-var) in the current environment.
  (funs nil :type list)
  ;; Global environment, which we just pass to Trucler.
  global-environment)

;;; We don't use Trucler's augmentation protocol internally since we often
;;; want to add a bunch of stuff at once, which is awkward in Trucler.
(defun make-lexical-environment (parent &key (vars (vars parent))
                                          (tags (tags parent))
                                          (blocks (blocks parent))
                                          (funs (funs parent)))
  (%make-lexical-environment
   :vars vars :tags tags :blocks blocks :funs funs
   :global-environment (global-environment parent)))

;;; We don't actually use Trucler's query protocol internally, since the
;;; environments are necessarily ours (they include bytecode-specific
;;; information, etc.)
;;; But we do fall back to it when we hit the global environment.
;;; And we define the methods, to be nice to macros, so maybe we
;;; should use it internally after all.
;;; TODO: Once trucler actually implements augmentation we should
;;; maybe use that and not have our own environments at all.

(defmethod trucler:describe-variable
    (client (env lexical-environment) name)
  (or (cdr (assoc name (vars env)))
      (trucler:describe-variable client
                                 (global-environment env) name)))

(defmethod trucler:describe-function
    (client (env lexical-environment) name)
  (or (cdr (assoc name (funs env) :test #'equal))
      (trucler:describe-variable client
                                 (global-environment env) name)))

(defmethod trucler:describe-block
    (client (env lexical-environment) name)
  (cdr (assoc name (blocks env))))

(defmethod trucler:describe-tag
    (client (env lexical-environment) name)
  (cdr (assoc name (tags env))))

(defun var-info (name env)
  (or (cdr (assoc name (vars env)))
      (trucler:describe-variable m:*client* (global-environment env) name)))
(defun fun-info (name env)
  (or (cdr (assoc name (funs env) :test #'equal))
      (trucler:describe-function m:*client* (global-environment env) name)))

;; never actually called
(defun missing-arg () (error "missing arg"))

;;; Our info for lexical bindings (variable and function).
(defstruct (lexical-info
            (:constructor make-lexical-info
                (frame-offset cfunction)))
  ;; Register index for this lvar.
  (frame-offset (missing-arg) :read-only t :type (integer 0))
  ;; Cfunction this lvar belongs to (i.e. is bound by)
  (cfunction (missing-arg) :read-only t :type cfunction))

;;; Our info for specifically variable bindings.
;;; (while function bindings can be closed over, they can't be modified,
;;;  so we don't really care.)
(defstruct (lexical-variable-info
            (:constructor make-lexical-variable-info
                (frame-offset cfunction))
            (:include lexical-info))
  (closed-over-p nil :type boolean)
  (setp nil :type boolean))

(defun frame-offset (lex-desc)
  (lexical-info-frame-offset (trucler:identity lex-desc)))

(defun lvar-cfunction (lex-desc)
  (lexical-info-cfunction (trucler:identity lex-desc)))

(defun closed-over-p (lvar-desc)
  (lexical-variable-info-closed-over-p (trucler:identity lvar-desc)))

(defun (setf closed-over-p) (new lvar-desc)
  (setf (lexical-variable-info-closed-over-p (trucler:identity lvar-desc))
        new))

(defun setp (lvar-desc)
  (lexical-variable-info-setp (trucler:identity lvar-desc)))

(defun (setf setp) (new lvar-desc)
  (setf (lexical-variable-info-setp (trucler:identity lvar-desc)) new))

;;; Does the lexical variable need a cell?
(defun indirect-lexical-p (lvar)
  (and (closed-over-p lvar) (setp lvar)))

(defun make-lexical-variable (name frame-offset cfunction)
  (make-instance 'trucler:lexical-variable-description
    :name name
    :identity (make-lexical-variable-info frame-offset cfunction)))

(defun make-symbol-macro (name expansion)
  (make-instance 'trucler:symbol-macro-description
    :name name :expansion expansion))

(defun constantp (symbol env)
  (typep (var-info symbol env) 'trucler:constant-variable-description))

(defun globally-special-p (symbol env)
  (typep (var-info symbol env) 'trucler:global-special-variable-description))

(defun make-local-function (name frame-offset cfunction)
  (make-instance 'trucler:local-function-description
    :name name
    :identity (make-lexical-info frame-offset cfunction)))

(defun make-local-macro (name expander)
  (make-instance 'trucler:local-macro-description
    :name name :expander expander))

;;; Bind each variable to a stack location, returning a new lexical
;;; environment and new context.
;;; The max local count in the current function is also updated.
(defun bind-vars (vars env context)
  (let* ((frame-start (context-frame-end context))
         (var-count (length vars))
         (frame-end (+ frame-start var-count))
         (function (context-function context)))
    (setf (cfunction-nlocals function)
          (max (cfunction-nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (vars vars (rest vars))
         (new-vars (vars env)
                   (acons (first vars)
                          (make-lexical-variable (first vars) index function)
                          new-vars)))
        ((>= index frame-end)
         (values (make-lexical-environment env :vars new-vars)
                 (new-context context :frame-end var-count)))
      (when (constantp (first vars) env)
        (error "Cannot bind constant value ~a!" (first vars))))))

;;; Like the above, but function namespace.
(defun bind-fvars (funs env context)
  (let* ((frame-start (context-frame-end context))
         (fun-count (length funs))
         (frame-end (+ frame-start fun-count))
         (function (context-function context)))
    (setf (cfunction-nlocals function)
          (max (cfunction-nlocals function) frame-end))
    (do ((index frame-start (1+ index))
         (funs funs (rest funs))
         (new-vars (funs env)
                   (acons (first funs)
                          (make-local-function (first funs) index function)
                          new-vars)))
        ((>= index frame-end)
         (values (make-lexical-environment env :funs new-vars)
                 (new-context context :frame-end fun-count))))))

(deftype lambda-expression () '(cons (eql lambda) (cons list list)))
(deftype function-name () '(or symbol (cons (eql setf) (cons symbol null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation
;;;

(defun coerce-to-lexenv (env)
  (if (lexical-environment-p env)
      env
      ;; Assume we've been passed a global environment.
      ;; NOTE: Other than the external COMPILE(-INTO), EVAL,
      ;; all functions in this file expecting an environment
      ;; specifically want one of our lexical environments.
      (make-null-lexical-environment env)))

;;; Compile into an existing module. Don't link.
;;; Useful for the file compiler, and for the first stage of runtime COMPILE.
(defun compile-into (module lambda-expression env
                     &rest keys &key block-name forms-only)
  (declare (ignore block-name forms-only))
  (check-type lambda-expression lambda-expression)
  (let ((env (coerce-to-lexenv env))
        (lambda-list (cadr lambda-expression))
        (body (cddr lambda-expression)))
    (apply #'compile-lambda lambda-list body env module keys)))

(defun compile-link (lambda-expression env &rest keys &key block-name forms-only)
  (declare (ignore block-name forms-only))
  (link-function (apply #'compile-into (make-cmodule) lambda-expression env keys)
                 (if (lexical-environment-p env)
                     (global-environment env)
                     env)))

;;; As CL:COMPILE, but doesn't mess with function bindings.
(defun compile (lambda-expression &optional env (m:*client* m:*client*))
  (compile-link lambda-expression env))

;;; Evaluate FORMS as a progn without relying on PROGN to be bound.
(defun eval-progn (forms &optional env (m:*client* m:*client*))
  (funcall (compile-link `(lambda () ,@forms) env :forms-only t)))

;;; As CL:EVAL.
(defun eval (form &optional env (m:*client* m:*client*))
  (eval-progn `(,form) env))

(defun compile-form (form env context)
  (typecase form
    (symbol (compile-symbol (var-info form env) form env context))
    ((cons symbol)
     (compile-combination (fun-info (car form) env) form env context))
    (cons (compile-lambda-form form env context))
    (t (compile-literal form env context))))

(defun compile-literal (form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (case form
      ((nil) (assemble context m:nil))
      (t (emit-const context (literal-index form context))))
    (when (eql (context-receiving context) t)
      (assemble context m:pop))))

(defgeneric compile-symbol (info form env context))

(defun expand (expander form env)
  (funcall *macroexpand-hook* expander form env))

(defun symbol-macro-expansion (info symbol env)
  (let* ((expansion (trucler:expansion info))
         (expander (lambda (form env) (declare (ignore form env)) expansion)))
    (expand expander symbol env)))

;;; Not used in this compiler, but useful in various other places.
(defun macroexpand-1 (form &optional env)
  (typecase form
    (symbol
     (let ((info (trucler:describe-variable m:*client* env form)))
       (if (typep info 'trucler:symbol-macro-description)
           (values (symbol-macro-expansion info form env) t)
           (values form nil))))
    ((cons symbol)
     (let* ((head (car form))
            (info (if (symbolp head)
                      (trucler:describe-function m:*client* env head)
                      nil)))
       (if (typep info 'trucler:macro-description)
           (values (expand (trucler:expander info) form env) t)
           (values form nil))))
    (t (values form nil))))

;;; ditto.
(defun macroexpand (form &optional env)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp) (macroexpand-1 form env)
             (if expandedp
                 (setf ever-expanded t form expansion)
                 (return (values form ever-expanded))))))

(defmethod compile-symbol ((info trucler:symbol-macro-description)
                           form env context)
  (compile-form (symbol-macro-expansion info form env) env context))

(defmethod compile-symbol ((info trucler:lexical-variable-description)
                           form env context)
  (declare (ignore form env))
  (unless (eql (context-receiving context) 0)
    (cond ((eq (lvar-cfunction info) (context-function context))
           (assemble context m:ref (frame-offset info)))
          (t
           (setf (closed-over-p info) t)
           (assemble context m:closure (closure-index info context))))
    (maybe-emit-cell-ref info context)
    (when (eql (context-receiving context) 't)
      (assemble context m:pop))))

(defmethod compile-symbol ((info trucler:constant-variable-description)
                           form env context)
  (declare (ignore form))
  (compile-literal (trucler:value info) env context))

(defmethod compile-symbol ((info trucler:special-variable-description)
                           form env context)
  (declare (ignore env))
  (unless (eql (context-receiving context) 0)
    (assemble context m:symbol-value (value-cell-index form context))
    (when (eql (context-receiving context) 't)
      (assemble context m:pop))))

(defmethod compile-symbol ((info null) form env context)
  (warn "Unknown variable ~a: treating as special" form)
  (unless (eql (context-receiving context) 0)
    (assemble context m:symbol-value (value-cell-index form context))
    (when (eql (context-receiving context) 't)
      (assemble context m:pop))))

(defgeneric compile-combination (info form env context))

(defmethod compile-combination ((info trucler:macro-description)
                                form env context)
  (compile-form (expand (trucler:expander info) form env) env context))

;;; Compile a call, where the callee is already on the stack.
(defun compile-call (args env context)
  (do ((args args (rest args))
       (arg-count 0 (1+ arg-count)))
      ((endp args)
       (emit-call context arg-count))
    (compile-form (first args) env (new-context context :receiving 1))))

(defmethod compile-combination ((info trucler:global-function-description)
                                form env context)
  (let ((expander (trucler:compiler-macro info)))
    (when expander
      (let ((expansion (expand expander form env)))
        (unless (eq form expansion)
          (return-from compile-combination
            (compile-form expansion env context)))))
    (emit-fdefinition context (fdefinition-index (trucler:name info) context))
    (compile-call (rest form) env context)))

(defmethod compile-combination ((info null) form env context)
  (warn "Unknown operator ~a: treating as global function" (first form))
  (emit-fdefinition context (fdefinition-index (first form) context))
  (compile-call (rest form) env context))

(defmethod compile-combination ((info trucler:local-function-description)
                                form env context)
  (reference-lexical-variable info context)
  (compile-call (rest form) env context))

;;; Given a lambda expression, generate code to push it to the stack
;;; as you would for #'(lambda ...).
;;; CONTEXT's number of return values is ignored.
(defun compile-lambda-expression (lexpr env context
                                  &rest keys &key block-name forms-only)
  (declare (ignore block-name forms-only))
  (check-type lexpr lambda-expression)
  (let* ((cfunction (apply #'compile-lambda (cadr lexpr) (cddr lexpr)
                           env (context-module context) keys))
         (closed (cfunction-closed cfunction)))
    (loop for info across closed
          do (reference-lexical-variable info context))
    (if (zerop (length closed))
        (emit-const context (cfunction-literal-index cfunction context))
        (assemble context m:make-closure
          (cfunction-literal-index cfunction context)))))

(defun compile-lambda-form (form env context)
  ;; FIXME: We can probably handle this more efficiently (without consing
  ;; a closure) by using compile-with-lambda-list instead.
  (let ((lexpr (car form)) (args (rest form)))
    (compile-lambda-expression lexpr env context)
    (compile-call args env context)))

(defmethod compile-combination ((info trucler:special-operator-description)
                                form env context)
  (compile-special (trucler:name info) form env context))

(defgeneric compile-special (operator form env context))

(defun compile-progn (forms env context)
  (do ((forms forms (rest forms))
       (body-context (new-context context :receiving 0)))
      ((null (rest forms))
       ;; this subsumes (progn) because (rest nil) = (first nil) = nil.
       (compile-form (first forms) env context))
    (compile-form (first forms) env body-context)))

(defun compile-locally (body env context)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (compile-progn body (add-specials (extract-specials decls) env) context)))

(defun fun-name-block-name (fun-name)
  (if (symbolp fun-name)
      fun-name
      ;; setf name
      (second fun-name)))

(defmethod compile-special ((operator (eql 'progn)) form env context)
  (compile-progn (rest form) env context))

;;; Add VARS as specials in ENV.
(defun add-specials (vars env)
  (let* ((existing (vars env))
         (new-vars existing))
    (loop for var in vars
          for desc = (trucler:describe-variable m:*client* env var)
          for specialp
            = (typep desc 'trucler:special-variable-description)
          unless specialp ; already covered
            do (push (cons var (make-instance
                                   'trucler:local-special-variable-description
                                 :name var))
                     new-vars))
    (if (eq new-vars existing) ; nothing added
        env
        (make-lexical-environment env :vars new-vars))))

(defun extract-specials (declarations)
  (let ((specials '()))
    (dolist (declaration declarations)
      (dolist (specifier (cdr declaration))
        (case (first specifier)
          (special
           (dolist (var (rest specifier))
             (push var specials))))))
    specials))

(defun canonicalize-binding (binding)
  (if (consp binding)
      (values (first binding) (second binding))
      (values binding nil)))

(defmethod compile-special ((operator (eql 'let)) form env context)
  (multiple-value-bind (body decls)
      (alexandria:parse-body (cddr form))
    (let* ((specials (extract-specials decls))
           (lexical-binding-count 0)
           (special-binding-count 0)
           (post-binding-env (add-specials specials env))
           (frame-start (context-frame-end context))
           ;; The values are compiled in a context with no extra
           ;; frame slots used, since the BIND takes place after the
           ;; values are evaluated.
           (valf-context (new-context context :receiving 1)))
      (dolist (binding (second form))
        (multiple-value-bind (var valf) (canonicalize-binding binding)
          (compile-form valf env valf-context)
          (cond ((or (member var specials)
                     (globally-special-p var env))
                 (incf special-binding-count)
                 (emit-special-bind context var))
                (t
                 (setf (values post-binding-env context)
                       (bind-vars (list var) post-binding-env context))
                 (incf lexical-binding-count)
                 (maybe-emit-make-cell (var-info var post-binding-env)
                                       context)))))
      (emit-bind context lexical-binding-count frame-start)
      (compile-progn body post-binding-env context)
      (emit-unbind context special-binding-count))))

(defun compile-let* (bindings decls body env context
                     &key (block-name nil block-name-p))
  (let ((special-binding-count 0)
        (specials (extract-specials decls))
        (inner-context context))
    (dolist (binding bindings)
      (multiple-value-bind (var valf) (canonicalize-binding binding)
        (compile-form valf env (new-context inner-context :receiving 1))
        (cond ((or (member var specials) (globally-special-p var env))
               (incf special-binding-count)
               (setq env (add-specials (list var) env))
               (emit-special-bind inner-context var)
               (setq inner-context (new-context inner-context
                                                :dynenv '(:special))))
              (t
               (let ((frame-start (context-frame-end inner-context)))
                 (setf (values env inner-context)
                       (bind-vars (list var) env inner-context))
                 (maybe-emit-make-cell (var-info var env) inner-context)
                 (assemble-maybe-long inner-context m:set frame-start))))))
    (let ((new-env (if specials
                       ;; We do this to make sure special declarations get
                       ;; through even if this form doesn't bind them.
                       ;; This creates duplicate alist entries for anything
                       ;; that _is_ bound here, but that's not a big deal.
                       (add-specials specials env)
                       env)))
      (if block-name-p
          (compile-block block-name body new-env inner-context)
          (compile-progn body new-env inner-context)))
    (emit-unbind context special-binding-count)))

(defmethod compile-special ((operator (eql 'let*)) form env context)
  (let ((bindings (second form)) (body (cddr form)))
    (multiple-value-bind (body decls) (alexandria:parse-body body)
      (compile-let* bindings decls body env context))))

(defmethod compile-special ((operator (eql 'flet)) form env context)
  (destructuring-bind (definitions . body) (rest form)
    (loop for (name lambda-list . body) in definitions
          do (compile-lambda-expression
              `(lambda ,lambda-list ,@body)
              env context :block-name (fun-name-block-name name)))
    (emit-bind context (length definitions) (context-frame-end context))
    (multiple-value-call #'compile-locally body
      (bind-fvars (mapcar #'car definitions) env context))))

(defmethod compile-special ((operator (eql 'labels)) form env context)
  (destructuring-bind (definitions . body) (rest form)
    (multiple-value-bind (new-env new-context)
        (bind-fvars (mapcar #'first definitions) env context)
      (let* ((module (context-module context))
             (closures
               (loop for (name lambda-list . body) in definitions
                     for bname = (fun-name-block-name name)
                     for fun = (compile-lambda lambda-list body new-env module
                                               :block-name bname)
                     for literal-index = (cfunction-literal-index fun context)
                     if (zerop (length (cfunction-closed fun)))
                       do (emit-const context literal-index)
                     else
                       collect (cons fun (frame-offset (fun-info name new-env)))
                       and do (assemble-maybe-long context
                                                   m:make-uninitialized-closure
                                                   literal-index))))
        (emit-bind context (length definitions) (context-frame-end context))
        (dolist (closure closures)
          (loop for var across (cfunction-closed (car closure))
                do (reference-lexical-variable var new-context))
          (assemble-maybe-long context m:initialize-closure (cdr closure)))
        (compile-locally body new-env new-context)))))

(defgeneric compile-setq-1 (info var value-form environment context))

(defmethod compile-setq-1 ((info trucler:symbol-macro-description)
                           var valf env context)
  ;; SETF can do its own macroexpansion of the variable.
  (compile-form `(setf ,var ,valf) env context))

(defun compile-setq-1-special (var valf env context)
  (compile-form valf env (new-context context :receiving 1))
  ;; If we need to return the new value, dup on the stack.
  ;; We can't just read from the special, since some other thread may
  ;; alter it.
  (unless (eql (context-receiving context) 0)
    (assemble context m:dup))
  (assemble-maybe-long context m:symbol-value-set
                       (value-cell-index var context))
  (when (eql (context-receiving context) t)
    (assemble context m:pop)))

(defmethod compile-setq-1 ((info trucler:special-variable-description)
                           var valf env context)
  (compile-setq-1-special var valf env context))

(defmethod compile-setq-1 ((info null) var valf env context)
  (warn "Unknown variable ~a: treating as special" var)
  (compile-setq-1-special var valf env context))

(defmethod compile-setq-1 ((info trucler:lexical-variable-description)
                           var valf env context)
  (let ((localp (eq (lvar-cfunction info) (context-function context))))
    (unless localp
      (setf (closed-over-p info) t))
    (setf (setp info) t)
    (compile-form valf env (new-context context :receiving 1))
    ;; similar concerns to specials above.
    (unless (eql (context-receiving context) 0)
      (assemble context m:dup))
    (cond (localp
           (emit-lexical-set info context))
          ;; Don't emit a fixup if we already know we need a cell.
          (t
           (assemble-maybe-long context m:closure
                                (closure-index info context))
           (assemble context m:cell-set)))
    (when (eql (context-receiving context) t)
      (assemble context m:pop))))

(defmethod compile-special ((op (eql 'setq)) form env context)
  (let ((pairs (rest form)))
    (if (null pairs)
        (unless (eql (context-receiving context) 0)
          (assemble context m:nil))
        (do ((pairs pairs (cddr pairs)))
            ((endp pairs))
          (let ((var (car pairs))
                (valf (cadr pairs))
                (rest (cddr pairs)))
            (compile-setq-1 (var-info var env) var valf env
                            (if rest
                                (new-context context :receiving 0)
                                context)))))))

(defmethod compile-special ((op (eql 'if)) form env context)
  (destructuring-bind (condition then &optional else) (rest form)
    (compile-form condition env (new-context context :receiving 1))
    (let ((then-label (make-label))
          (done-label (make-label)))
      (emit-jump-if context then-label)
      (compile-form else env context)
      (emit-jump context done-label)
      (emit-label context then-label)
      (compile-form then env context)
      (emit-label context done-label))))

;;; Push the immutable value or cell of lexical in CONTEXT.
(defun reference-lexical-variable (info context)
  (if (eq (lvar-cfunction info) (context-function context))
      (assemble-maybe-long context m:ref (frame-offset info))
      (assemble-maybe-long context m:closure (closure-index info context))))

(defun compile-function-lookup (fnameoid env context)
  (etypecase fnameoid
    (lambda-expression (compile-lambda-expression fnameoid env context))
    (function-name
     (let ((info (fun-info fnameoid env)))
       (etypecase info
         (trucler:global-function-description
          (emit-fdefinition context (fdefinition-index fnameoid context)))
         (trucler:local-function-description
          (reference-lexical-variable info context))
         (null
          (warn "Unknown function ~a: treating as global function" fnameoid)
          (emit-fdefinition context (fdefinition-index fnameoid context))))))))

(defmethod compile-special ((op (eql 'function)) form env context)
  (unless (eql (context-receiving context) 0)
    (compile-function-lookup (second form) env context)
    (when (eql (context-receiving context) t)
      (assemble context m:pop))))

(defun go-tag-p (object) (typep object '(or symbol integer)))

(defmethod compile-special ((op (eql 'tagbody)) form env context)
  (let ((statements (rest form))
        (new-tags (tags env))
        (tagbody-dynenv (gensym "TAG-DYNENV")))
    (multiple-value-bind (env stmt-context-1)
        (bind-vars (list tagbody-dynenv) env context)
      (let* ((dynenv-info (var-info tagbody-dynenv env))
             (stmt-context (new-context stmt-context-1
                                        :receiving 0
                                        :dynenv (list dynenv-info))))
        (dolist (statement statements)
          (when (go-tag-p statement)
            (push (list* statement dynenv-info (make-label)) new-tags)))
        (let ((env (make-lexical-environment env :tags new-tags)))
          ;; Bind the dynamic environment.
          (emit-entry-or-save-sp context dynenv-info)
          ;; Compile the body, emitting the tag destination labels.
          (dolist (statement statements)
            (if (go-tag-p statement)
                (emit-label context (cddr (assoc statement (tags env))))
                (compile-form statement env stmt-context))))
        (maybe-emit-entry-close context dynenv-info))
      ;; return nil if we really have to
      (unless (eql (context-receiving context) 0)
        (assemble context m:nil)
        (when (eql (context-receiving context) t)
          (assemble context m:pop))))))

(defun compile-exit (exit-info context)
  (destructuring-bind (dynenv-info . label) exit-info
    (cond ((eq (lvar-cfunction dynenv-info) (context-function context))
           ;; Local unwind.
           (dolist (entry (context-dynenv context))
             (when (eq entry dynenv-info) (return))
             (etypecase entry
               ((eql :special) ; special binding
                ;; TODO: Doesn't matter now, but if we had an unbind-n
                ;; instruction we could leverage that here.
                (emit-unbind context 1))
               ((eql :catch)
                (assemble context m:catch-close))
               ((eql :protect) ; unwind protect
                (assemble context m:cleanup))
               (trucler:lexical-variable-description
                (maybe-emit-entry-close context entry))))
           ;; Exit.
           (emit-ref-or-restore-sp context dynenv-info)
           (emit-exit-or-jump context dynenv-info label))
          (t
           (setf (closed-over-p dynenv-info) t)
           (assemble-maybe-long context m:closure
                                (closure-index dynenv-info context))
           (emit-exit context label)))))

(defmethod compile-special ((op (eql 'go)) form env context)
  (let ((pair (assoc (second form) (tags env))))
    (if pair
        (compile-exit (cdr pair) context)
        (error "The GO tag ~a does not exist." (second form)))))

(defun compile-block (name body env context)
  (let ((block-dynenv (gensym "BLOCK-DYNENV")))
    (multiple-value-bind (env body-context-1)
        (bind-vars (list block-dynenv) env context)
      (let* ((dynenv-info (var-info block-dynenv env))
             (body-context (new-context body-context-1
                                        :dynenv (list dynenv-info)))
             (label (make-label))
             (normal-label (make-label)))
        ;; Bind the dynamic environment.
        (emit-entry-or-save-sp context dynenv-info)
        (let ((env (make-lexical-environment
                    env
                    :blocks (acons name (cons dynenv-info label)
                                   (blocks env)))))
          ;; Force single values into multiple
          ;; so that we can uniformly PUSH afterward.
          (compile-progn body env body-context))
        (when (eql (context-receiving context) 1)
          (emit-jump context normal-label))
        (emit-label context label)
        ;; When we need 1 value, we have to make sure that the
        ;; "exceptional" case pushes a single value onto the stack.
        (when (eql (context-receiving context) 1)
          (assemble context m:push)
          (emit-label context normal-label))
        (maybe-emit-entry-close context dynenv-info)))))

(defmethod compile-special ((op (eql 'block)) form env context)
  (compile-block (second form) (cddr form) env context))

(defmethod compile-special ((op (eql 'return-from)) form env context)
  (let ((name (second form)) (value (third form)))
    (compile-form value env (new-context context :receiving t))
    (let ((pair (assoc name (blocks env))))
      (if pair
          (compile-exit (cdr pair) context)
          (error "The block ~a does not exist." name)))))

(defmethod compile-special ((op (eql 'catch)) form env context)
  (let ((tag (second form)) (body (cddr form))
        (target (make-label)))
    (compile-form tag env (new-context context :receiving 1))
    (emit-catch context target)
    (compile-progn body env (new-context context :dynenv '(:catch)))
    (assemble context m:catch-close)
    (emit-label context target)))

(defmethod compile-special ((op (eql 'throw)) form env context)
  (let ((tag (second form)) (result (third form)))
    (compile-form tag env (new-context context :receiving 1))
    (compile-form result env (new-context context :receiving t))
    (assemble context m:throw)))

(defmethod compile-special ((op (eql 'progv)) form env context)
  (destructuring-bind (symbols values . body) (rest form)
    (compile-form symbols env (new-context context :receiving 1))
    (compile-form values env (new-context context :receiving 1))
    (assemble-maybe-long context m:progv (env-index context))
    (compile-progn body env context)
    (emit-unbind context 1)))

(defmethod compile-special ((op (eql 'unwind-protect))
                            form env context)
  (destructuring-bind (protected . cleanup) (rest form)
    ;; Build a cleanup thunk.
    ;; This will often/usually be a closure, which is why we
    ;; can't just give M:PROTECT a constant argument.
    ;; The 0 is a dumb KLUDGE to let the cleanup forms be compiled in
    ;; non-values contexts, which might be more efficient.
    ;; (We use 0 instead of NIL because NIL may not be bound.)
    (compile-lambda-expression `(lambda () ,@cleanup 0)
                               env context :forms-only t)
    (assemble context m:protect)
    (compile-form protected env
                  (new-context context :dynenv '(:protect)))
    (assemble context m:cleanup)))

(defmethod compile-special ((op (eql 'quote)) form env context)
  (compile-literal (second form) env context))

(defmethod compile-special ((op (eql 'load-time-value)) form env context)
  (destructuring-bind (form &optional read-only-p) (rest form)
    (check-type read-only-p boolean)
    ;; Stick info about the LTV into the literals vector. It will be handled
    ;; later by COMPILE or a file compiler.
    (let ((index (ltv-index (make-ltv-info form read-only-p) context)))
      ;; Maybe compile a literal load.
      ;; (Note that we do always need to register the LTV, since it may have
      ;;  some weird side effect. We could hypothetically save some space by
      ;;  not allocating a spot in the constants if the value isn't actually
      ;;  used, but that's a very marginal case.)
      (ecase (context-receiving context)
        ((0))
        ((1) (assemble-maybe-long context m:const index))
        ((t) (assemble-maybe-long context m:const index)
         (assemble context m:pop))))))

(defmethod compile-special ((op (eql 'symbol-macrolet)) form env context)
  (let* ((bindings (second form)) (body (cddr form))
         (smacros
           (loop for (name expansion) in bindings
                 collect (cons name (make-symbol-macro name expansion)))))
    (compile-locally body (make-lexical-environment
                           env
                           :vars (append (nreverse smacros) (vars env)))
                     context)))

(defun lexenv-for-macrolet (env)
  ;; Macrolet expanders need to be compiled in the local compilation environment,
  ;; so that e.g. their bodies can use macros defined in outer macrolets.
  ;; At the same time, they obviously do not have access to any runtime
  ;; environment. Taking out all runtime information is one way to do this but
  ;; it's slightly not-nice in that if someone writes a macroexpander that does
  ;; try to use local runtime information may fail silently by using global info
  ;; instead. So: FIXME.
  (make-lexical-environment
   env
   :vars (loop for pair in (vars env)
               when (typep (cdr pair) '(or trucler:constant-variable-description
                                        trucler:symbol-macro-description))
                 collect pair)
   :funs (loop for pair in (funs env)
               when (typep (cdr pair) 'trucler:macro-description)
                 collect pair)
   :tags nil :blocks nil))

;;; Given the arguments to parse-macro, return a macroexpander,
;;; i.e. an actual function. The environment must already be
;;; stripped by lexenv-for-macrolet (so that this can be done once
;;; for multiple definitions).
;;; Also used in cmpltv.
(defun compute-macroexpander (name lambda-list body env)
  ;; see comment in parse-macro for explanation
  ;; as to how we're using the host here
  (cl:compile nil (parse-macro name lambda-list body env
                               (lambda (lexpr env &rest keys)
                                 (apply #'compile-link lexpr env keys)))))

(defmethod compile-special ((op (eql 'macrolet)) form env context)
  (let* ((bindings (second form)) (body (cddr form))
         (macros
           (loop with env = (lexenv-for-macrolet env)
                 for (name lambda-list . body) in bindings
                 for macrof = (compute-macroexpander
                               name lambda-list body env)
                 for info = (make-local-macro name macrof)
                 collect (cons name info))))
    (compile-locally body (make-lexical-environment
                           env :funs (append macros (funs env)))
                     context)))

;;; Compile resolution of a function designator into a function.
;;; This is only one value, so CONTEXT's receiving is ignored.
;;; We pick off some special cases (e.g. #'foo) as a basic optimization,
;;; and otherwise fall back to a more complicated etypecase.
;;; With runtime support we could expand into (coerce-fdesignator form).
(defun compile-fdesignator (form env context)
  (typecase form
    ((cons (eql function) (cons t null)) ; #'foo or #'(lambda ...)
     (compile-function-lookup (second form) env context))
    ((cons (eql lambda))
     (compile-lambda-expression form env context))
    ((cons (eql quote) (cons symbol null)) ; 'foo
     ;; This is like compile-function-lookup but we ignore any local
     ;; environments. We also don't signal any unknown function
     ;; warnings as this is a very runtime sort of lookup.
     (emit-fdefinition context (fdefinition-index (second form) context)))
    (t
     (compile-form form env (new-context context :receiving 1))
     (assemble-maybe-long context m:fdesignator (env-index context)))))

(defmethod compile-special ((op (eql 'multiple-value-call)) form env context)
  (let ((function-form (second form)) (forms (cddr form)))
    (compile-fdesignator function-form env context)
    (if forms
        (let ((first (first forms))
              (rest (rest forms)))
          (compile-form first env (new-context context :receiving t))
          (assemble context m:push-values)
          (when rest
            (dolist (form rest)
              (compile-form form env (new-context context :receiving t))
              (assemble context m:append-values)))
          (emit-mv-call context))
        (emit-call context 0))))

(defmethod compile-special ((op (eql 'multiple-value-prog1)) form env context)
  (let ((first-form (second form)) (forms (cddr form)))
    (compile-form first-form env context)
    (unless (member (context-receiving context) '(0 1))
      (assemble context m:push-values))
    (dolist (form forms)
      (compile-form form env (new-context context :receiving 0)))
    (unless (member (context-receiving context) '(0 1))
      (assemble context m:pop-values))))

(defmethod compile-special ((op (eql 'locally)) form env context)
  (compile-locally (rest form) env context))

(defmethod compile-special ((op (eql 'eval-when)) form env context)
  (let ((situations (second form)) (body (cddr form)))
    (if (or (member 'cl:eval situations) (member :execute situations))
        (compile-progn body env context)
        (compile-literal nil env context))))

(defmethod compile-special ((op (eql 'the)) form env context)
  ;; ignore
  (compile-form (third form) env context))

;;; Deal with lambda lists. Compile the body with the lambda vars bound.
;;; Optional/key handling is done in two steps:
;;;
;;; 1. Bind any supplied optional/key vars to the passed values.
;;;
;;; 2. Default any unsupplied optional/key values and set the
;;; corresponding suppliedp var for each optional/key.
(defun compile-with-lambda-list (lambda-list body env context
                                 &key forms-only (block-name nil block-name-p))
  (multiple-value-bind (body decls documentation)
      (if forms-only
          (values body nil)
          (alexandria:parse-body body :documentation t))
    (declare (ignore documentation)) ; FIXME
    (multiple-value-bind (required optionals rest keys aok-p aux key-p)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (let* ((function (context-function context))
             (entry-point (cfunction-entry-point function))
             (min-count (length required))
             (optional-count (length optionals))
             (max-count (+ min-count optional-count))
             (key-count (length keys))
             (more-p (or rest key-p))
             new-env ; will be the body environment
             default-env ; environment for compiling default forms
             (context context)
             (specials (extract-specials decls))
             (special-binding-count 0)
             ;; An alist from optional and key variables to their local indices.
             ;; This is needed so that we can properly mark any that are special as
             ;; such while leaving them temporarily "lexically" bound during
             ;; argument parsing.
             (opt-key-indices nil))
        (setf (values new-env context) (bind-vars required env context))
        (emit-label context entry-point)
        ;; Generate argument count check.
        (cond ((and required (= min-count max-count) (not more-p))
               (assemble context m:check-arg-count-= min-count))
              (t
               (when required
                 (assemble context m:check-arg-count->= min-count))
               (when (not more-p)
                 (assemble context m:check-arg-count-<= max-count))))
        (unless (zerop min-count)
          (assemble-maybe-long context m:bind-required-args min-count)
          (dolist (var required)
            ;; We account for special declarations in outer environments/globally
            ;; by checking the original environment - not our new one - for info.
            (cond ((or (member var specials)
                       (globally-special-p var env))
                   (let ((info (var-info var new-env)))
                     (assemble-maybe-long context m:ref (frame-offset info))
                     (emit-special-bind context var))
                   (incf special-binding-count))
                  (t
                   (maybe-emit-encage (var-info var new-env) context))))
          (setq new-env (add-specials (intersection specials required) new-env)))
        ;; set the default env to have all the requireds bound,
        ;; but don't put in the optionals (yet).
        (setq default-env new-env)
        (unless (zerop optional-count)
          ;; Generate code to bind the provided optional args; unprovided args will
          ;; be initialized with the unbound marker.
          (assemble-maybe-long context m:bind-optional-args
                               min-count optional-count)
          (let ((optvars (mapcar #'first optionals)))
            ;; Mark the location of each optional. Note that we do this even if
            ;; the variable will be specially bound.
            (setf (values new-env context)
                  (bind-vars optvars new-env context))
            ;; Add everything to opt-key-indices.
            (dolist (var optvars)
              (push (cons var (frame-offset (var-info var new-env)))
                    opt-key-indices))))
        (when rest
          (assemble-maybe-long context m:listify-rest-args max-count)
          (assemble-maybe-long context m:set (context-frame-end context))
          (setf (values new-env context)
                (bind-vars (list rest) new-env context))
          (cond ((or (member rest specials)
                     (globally-special-p rest env))
                 (assemble-maybe-long
                  context m:ref (frame-offset (var-info rest new-env)))
                 (emit-special-bind context rest)
                 (incf special-binding-count 1)
                 (setq new-env (add-specials (list rest) new-env)))
                (t
                 (maybe-emit-encage (var-info rest new-env) context))))
        (when key-p
          ;; Generate code to parse the key args. As with optionals, we don't do
          ;; defaulting yet.
          (let ((key-names (mapcar #'caar keys)))
            (emit-parse-key-args context max-count key-count key-names aok-p)
            ;; emit-parse-key-args establishes the first key in the literals.
            ;; now do the rest.
            (dolist (key-name (rest key-names))
              (new-literal-index key-name context)))
          (let ((keyvars (mapcar #'cadar keys)))
            (setf (values new-env context)
                  (bind-vars keyvars new-env context))
            (dolist (var keyvars)
              (let ((info (var-info var new-env)))
                (push (cons var (frame-offset info)) opt-key-indices)))))
        ;; Generate defaulting code for optional args, and special-bind them
        ;; if necessary.
        (unless (zerop optional-count)
          (do ((optionals optionals (rest optionals))
               (optional-label (make-label) next-optional-label)
               (next-optional-label (make-label) (make-label)))
              ((endp optionals)
               (emit-label context optional-label))
            (emit-label context optional-label)
            (destructuring-bind (optional-var defaulting-form supplied-var)
                (first optionals)
              (let ((optional-special-p (or (member optional-var specials)
                                            (globally-special-p optional-var env)))
                    (index (cdr (assoc optional-var opt-key-indices)))
                    (supplied-special-p
                      (and supplied-var
                           (or (member supplied-var specials)
                               (globally-special-p supplied-var env)))))
                (setf (values new-env context)
                      (compile-optional/key-item optional-var defaulting-form
                                                 index
                                                 supplied-var next-optional-label
                                                 optional-special-p supplied-special-p
                                                 context new-env
                                                 default-env))
                ;; set the default env for later bindings.
                (let* ((ovar (cons optional-var
                                   (var-info optional-var new-env)))
                       (svar (when supplied-var
                               (cons supplied-var
                                     (var-info supplied-var new-env))))
                       (newvars
                         (if svar (list svar ovar) (list ovar))))
                  (setf default-env
                        (make-lexical-environment
                         default-env
                         :vars (append newvars (vars default-env)))))
                (when optional-special-p (incf special-binding-count))
                (when supplied-special-p (incf special-binding-count))))))
        ;; Generate defaulting code for key args, and special-bind them if necessary.
        (when key-p
          ;; Bind the rest parameter in the default env, if existent.
          (when rest
            (let ((rvar (cons rest (var-info rest new-env)))
                  (old (vars default-env)))
              (setf default-env
                    (make-lexical-environment
                     default-env :vars (cons rvar old)))))
          (do ((keys keys (rest keys))
               (key-label (make-label) next-key-label)
               (next-key-label (make-label) (make-label)))
              ((endp keys) (emit-label context key-label))
            (emit-label context key-label)
            (destructuring-bind ((key-name key-var) defaulting-form supplied-var)
                (first keys)
              (declare (ignore key-name))
              (let ((index (cdr (assoc key-var opt-key-indices)))
                    (key-special-p (or (member key-var specials)
                                       (globally-special-p key-var env)))
                    (supplied-special-p
                      (and supplied-var
                           (or (member supplied-var specials)
                               (globally-special-p supplied-var env)))))
                (setf (values new-env context)
                      (compile-optional/key-item key-var defaulting-form index
                                                 supplied-var next-key-label
                                                 key-special-p supplied-special-p
                                                 context new-env
                                                 default-env))
                ;; set the default env for later bindings.
                (let* ((ovar (cons key-var
                                   (var-info key-var new-env)))
                       (svar (when supplied-var
                               (cons supplied-var
                                     (var-info supplied-var new-env))))
                       (newvars
                         (if svar (list svar ovar) (list ovar))))
                  (setf default-env
                        (make-lexical-environment
                         default-env
                         :vars (append newvars (vars default-env)))))
                (when key-special-p (incf special-binding-count))
                (when supplied-special-p (incf special-binding-count))))))
        ;; Generate aux and the body as a let*.
        ;; We repeat the special declarations so that let* will know the auxs
        ;; are special, and so that any free special declarations are processed.
        (if block-name-p
            (compile-let* aux `((declare (special ,@specials))) body
                          new-env context :block-name block-name)
            (compile-let* aux `((declare (special ,@specials))) body
                          new-env context))
        (emit-unbind context special-binding-count)))))

;;; Compile an optional/key item and return the resulting environment
;;; and context.
(defun compile-optional/key-item (var defaulting-form var-index supplied-var next-label
                                  var-specialp supplied-specialp context env default-env)
  (flet ((default (suppliedp specialp var info)
           (cond (suppliedp
                  (cond (specialp
                         (assemble-maybe-long context m:ref var-index)
                         (emit-special-bind context var))
                        (t
                         (maybe-emit-encage info context))))
                 (t
                  ;; We compile in default-env but also context.
                  ;; The context already has space allocated for all
                  ;; the later lexical parameters, which have already
                  ;; been bound. Thus, we ensure that no bindings
                  ;; in the default form clobber later parameters.
                  (compile-form defaulting-form default-env
                                (new-context context :receiving 1))
                  (cond (specialp
                         (emit-special-bind context var))
                        (t
                         (maybe-emit-make-cell info context)
                         (assemble-maybe-long context m:set var-index))))))
         (supply (suppliedp specialp var info)
           (if suppliedp
               (compile-literal t env (new-context context :receiving 1))
               (assemble context m:nil))
           (cond (specialp
                  (emit-special-bind context var))
                 (t
                  (maybe-emit-make-cell info context)
                  (assemble-maybe-long
                   context m:set (frame-offset info))))))
    (let ((supplied-label (make-label))
          (var-info (var-info var env)))
      (when supplied-var
        (setf (values env context)
              (bind-vars (list supplied-var) env context)))
      (let ((supplied-info (var-info supplied-var env)))
        (emit-jump-if-supplied context var-index supplied-label)
        (default nil var-specialp var var-info)
        (when supplied-var
          (supply nil supplied-specialp supplied-var supplied-info))
        (emit-jump context next-label)
        (emit-label context supplied-label)
        (default t var-specialp var var-info)
        (when supplied-var
          (supply t supplied-specialp supplied-var supplied-info))
        (when var-specialp
          (setq env (add-specials (list var) env)))
        (when supplied-specialp
          (setq env (add-specials (list supplied-var) env)))
        (values env context)))))

;;; Compile the lambda in MODULE, returning the resulting
;;; CFUNCTION.
;;; If BLOCK-NAME is provided, a block with the given name will be provided
;;; around the body forms. If FORMS-ONLY is true, documentation and declarations
;;; will be treated as forms.
;;; These options are provided so that compilation can proceed as if the body
;;; is wrapped in CL:BLOCK or CL:PROGN (respectively) without requiring that
;;; those operators actually be available in the compilation environment.
(defun compile-lambda (lambda-list body env module
                       &rest keys &key block-name forms-only)
  (declare (ignore block-name forms-only))
  (let* ((function (make-cfunction module))
         (context (make-context :receiving t :function function))
         (env (make-lexical-environment env)))
    (setf (cfunction-index function)
          (vector-push-extend function (cmodule-cfunctions module)))
    (apply #'compile-with-lambda-list lambda-list body env context keys)
    (assemble context m:return)
    function))

;;;; linkage

(defun unsigned (x size)
  (logand x (1- (ash 1 size))))

;;; Use the optimistic bytecode vector sizes to initialize the optimistic cfunction position.
(defun initialize-cfunction-positions (cmodule)
  (let ((position 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let ((function (aref (cmodule-cfunctions cmodule) i)))
        (setf (cfunction-position function) position)
        (incf position (length (cfunction-bytecode function)))))))

;;; Update the positions of all affected functions and annotations
;;; from the effect of increasing the size of FIXUP by INCREASE. The
;;; resizer has already updated the size of the the fixup.
(defun update-positions (fixup increase)
  (let ((function (fixup-function fixup)))
    ;; Update affected annotation positions in this function.
    (let ((annotations (cfunction-annotations function)))
      (do ((index (1+ (fixup-index fixup)) (1+ index)))
          ((= index (length annotations)))
        (let ((annotation (aref annotations index)))
          (incf (annotation-position annotation) increase))))
    ;; Increase the size of this function to account for fixup growth.
    (incf (cfunction-extra function) increase)
    ;; Update module offsets for affected functions.
    (let ((functions (cmodule-cfunctions (cfunction-cmodule function))))
      (do ((index (1+ (cfunction-index function)) (1+ index)))
          ((= index (length functions)))
        (let ((function (aref functions index)))
          (incf (cfunction-position function) increase))))))

;;; With all functions and annotations initialized with optimistic
;;; sizes, resize fixups until no more expansion is needed.
(defun resolve-fixup-sizes (cmodule)
  (loop
    (let ((changed-p nil)
          (functions (cmodule-cfunctions cmodule)))
      (dotimes (i (length functions))
        (dotimes (j (length (cfunction-annotations (aref functions i))))
          (let ((annotation (aref (cfunction-annotations (aref functions i)) j)))
            (when (fixup-p annotation)
              (let ((old-size (fixup-size annotation))
                    (new-size (funcall (fixup-resizer annotation) annotation)))
                (unless (= old-size new-size)
                  (assert (>= new-size old-size))
                  (setf (fixup-size annotation) new-size)
                  (setq changed-p t)
                  (update-positions annotation (- new-size old-size))))))))
      (unless changed-p
        (return)))))

;;; The size of the module bytecode vector.
(defun module-bytecode-size (cmodule)
  (let* ((cfunctions (cmodule-cfunctions cmodule))
         (last-cfunction (aref cfunctions (1- (length cfunctions)))))
    (+ (cfunction-position last-cfunction)
       (length (cfunction-bytecode last-cfunction))
       (cfunction-extra last-cfunction))))

;;; Create the bytecode module vector. We scan over the fixups in the
;;; module and copy segments of bytecode between fixup positions.
(defun create-module-bytecode (cmodule)
  (let ((bytecode (make-array (module-bytecode-size cmodule)
                              :element-type '(unsigned-byte 8)))
        (index 0))
    (dotimes (i (length (cmodule-cfunctions cmodule)))
      (let* ((function (aref (cmodule-cfunctions cmodule) i))
             (cfunction-bytecode (cfunction-bytecode function))
             (position 0))
        (dotimes (i (length (cfunction-annotations function)))
          (let ((annotation (aref (cfunction-annotations function) i)))
            (when (fixup-p annotation)
            (unless (zerop (fixup-size annotation))
              (assert (= (fixup-size annotation)
                         (funcall (fixup-resizer annotation) annotation)))
              ;; Copy bytes in this segment.
              (let ((end (fixup-initial-position annotation)))
                (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
                (incf index (- end position))
                (setf position end))
              (assert (= index (annotation-module-position annotation)))
              ;; Emit fixup.
              (funcall (fixup-emitter annotation)
                       annotation
                       index
                       bytecode)
              (incf position (fixup-initial-size annotation))
              (incf index (fixup-size annotation))))))
        ;; Copy any remaining bytes from this function to the module.
        (let ((end (length cfunction-bytecode)))
          (replace bytecode cfunction-bytecode :start1 index :start2 position :end2 end)
          (incf index (- end position)))))
    bytecode))

;;; Finish fixups for a module and return its final bytecode.
(defun link (cmodule)
  (initialize-cfunction-positions cmodule)
  (resolve-fixup-sizes cmodule)
  (create-module-bytecode cmodule))

;;; The compiler works with compilation environments, but for loading
;;; in constants and stuff it may need a run-time environment.
;;; This function is called to retrieve that environment.
;;; The argument is a global compilation environment, e.g. the
;;; argument provided to COMPILE or EVAL.
(defgeneric run-time-environment (client compilation-environment)
  ;; By default, the compilation environment is assumed to be
  ;; identical to the run-time environment. E.g. for the native client
  ;; they're both NIL.
  (:method (client cmpenv)
    (declare (ignore client))
    cmpenv))

;;; Given info about a literal, return an object corresponding to it for an
;;; actual runtime constants vector.
;;; ENVIRONMENT is a global compilation environment.
(defgeneric load-literal-info (client literal-info environment))

(defmethod load-literal-info (client (info cfunction) env)
  (declare (ignore client env))
  (cfunction-info info))
(defmethod load-literal-info (client (info ltv-info) env)
  (eval (ltv-info-form info) env client))
(defmethod load-literal-info (client (info constant-info) env)
  (declare (ignore client env))
  (constant-info-value info))
(defmethod load-literal-info (client (info fdefinition-info) env)
  (m:link-function client (run-time-environment m:*client* env)
                   (fdefinition-info-name info)))
(defmethod load-literal-info (client (info value-cell-info) env)
  (m:link-variable client (run-time-environment m:*client* env)
                   (value-cell-info-name info)))
(defmethod load-literal-info (client (info env-info) env)
  (m:link-environment client (run-time-environment m:*client* env)))

;;; Run down the hierarchy and link the compile time representations
;;; of modules and functions together into runtime objects.
(defun link-load (cmodule env)
  (let* ((bytecode (link cmodule))
         (cmodule-literals (cmodule-literals cmodule))
         (literal-length (length cmodule-literals))
         (literals (make-array literal-length))
         (bytecode-module
           (m:make-bytecode-module
            :bytecode bytecode
            :literals literals))
         (client m:*client*))
    ;; Create the real function objects.
    (loop for cfunction across (cmodule-cfunctions cmodule)
          do (setf (cfunction-info cfunction)
                   (m:make-bytecode-function
                    m:*client*
                    bytecode-module
                    (cfunction-nlocals cfunction)
                    (length (cfunction-closed cfunction))
                    (annotation-module-position
                     (cfunction-entry-point cfunction))
                    (cfunction-final-size cfunction))))
    ;; Now replace the cfunctions in the cmodule literal vector with
    ;; real bytecode functions.
    ;; Also replace the load-time-value infos with the evaluated forms.
    (map-into literals
              (lambda (info) (load-literal-info client info env))
              cmodule-literals))
  (values))

;;; Given a cfunction, link constants and return an actual function.
;;; ENV must be a global environment.
(defun link-function (cfunction env)
  (link-load (cfunction-cmodule cfunction) env)
  (cfunction-info cfunction))
