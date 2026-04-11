(in-package #:maclina.compile)

;; never actually called
(defun missing-arg () (error "missing arg"))

(defstruct (lexical-environment (:constructor make-null-lexical-environment
                                    (global-environment))
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An list of variable-descriptions in the current environment.
  (vars nil :type list :read-only t)
  ;; A list of tag descriptions in the current environment.
  (tags nil :type list :read-only t)
  ;; A list of block descriptions in the current environment.
  (blocks nil :type list :read-only t)
  ;; A list of operator-descriptions in the current environment.
  (funs nil :type list :read-only t)
  ;; Global environment, which we just pass to Trucler.
  (global-environment (missing-arg) :read-only t))

;;; We don't use Trucler's augmentation protocol internally since we often
;;; want to add a bunch of stuff at once, which is awkward in Trucler.
;;; But we do define some of it, to be nice to macros.
(defun make-lexical-environment (parent &key (vars (vars parent))
                                          (tags (tags parent))
                                          (blocks (blocks parent))
                                          (funs (funs parent)))
  (%make-lexical-environment
   :vars vars :tags tags :blocks blocks :funs funs
   :global-environment (global-environment parent)))

(defun make-null-lexenv (global-compilation-environment)
  (%make-lexical-environment
   :global-environment global-compilation-environment))

;;; We don't actually use Trucler's query protocol internally, since the
;;; environments are necessarily ours (they include bytecode-specific
;;; information, etc.)
;;; But we do fall back to it when we hit the global environment.
;;; And we define the methods, to be nice to macros, so maybe we
;;; should use it internally after all.
;;; TODO: Once trucler actually implements augmentation we should
;;; maybe use that and not have our own environments at all.

(defmethod trucler:global-environment (client (env lexical-environment))
  (trucler:global-environment client (global-environment env)))

(defmethod trucler:describe-variable
    (client (env lexical-environment) name)
  (or (find name (vars env) :key #'trucler:name)
      (trucler:describe-variable client (global-environment env) name)))

(defmethod trucler:describe-function
    (client (env lexical-environment) name)
  (or (find name (funs env) :key #'trucler:name :test #'equal)
      (trucler:describe-function client (global-environment env) name)))

(defmethod trucler:describe-block
    (client (env lexical-environment) name)
  (declare (ignore client))
  (find name (blocks env) :key #'trucler:name))

(defmethod trucler:describe-tag
    (client (env lexical-environment) name)
  (declare (ignore client))
  (find name (tags env) :key #'trucler:name))

(defmethod trucler:describe-declarations (client (env lexical-environment))
  (trucler:describe-declarations client (global-environment env)))

(defmethod trucler:add-local-special-variable
    (client (env lexical-environment) name)
  (if (typep (trucler:describe-variable client env name)
             'trucler:special-variable-description)
      env
      (make-lexical-environment
       env
       :vars (cons (make-instance 'trucler:local-special-variable-description
                     :name name)
                   (vars env)))))

(defmethod trucler:add-local-symbol-macro
    (client (env lexical-environment) name expansion)
  (declare (ignore client))
  (make-lexical-environment
   env
   :vars (cons (make-symbol-macro name expansion) (vars env))))

(defmethod trucler:add-local-macro (client (env lexical-environment) name expander)
  (declare (ignore client))
  (make-lexical-environment
   env
   :funs (cons (make-local-macro name expander) (funs env))))

(defmethod trucler:add-inline (client (env lexical-environment) name inline)
  ;; FIXME
  (declare (ignore client name inline))
  env)

(defun var-info (name env)
  (trucler:describe-variable m:*client* env name))
(defun fun-info (name env)
  (trucler:describe-function m:*client* env name))
(defun block-info (name env)
  (trucler:describe-block m:*client* env name))
(defun tag-info (name env)
  (trucler:describe-tag m:*client* env name))

;;; Our info for lexical bindings (variable, function, block, tagbody).
(defstruct (lexical-info
            (:constructor make-lexical-info (frame-offset cfunction)))
  ;; Register index for this lvar.
  (frame-offset (missing-arg) :read-only t :type (integer 0))
  ;; Cfunction this lvar belongs to (i.e. is bound by)
  (cfunction (missing-arg) :read-only t :type cfunction))

;;; For variables and functions, where we want to note unused bindings.
(defstruct (ignorable-lexical-info
            (:constructor make-ignorable-lexical-info (frame-offset cfunction))
            (:include lexical-info))
  ;; Has the variable been read (for cl:ignore tracking).
  (readp nil :type boolean))

;;; Our info for specifically variable bindings.
;;; (while function bindings can be closed over, they can't be modified,
;;;  so we don't need to worry about cells, and thus closed-over-p is unneeded.)
(defstruct (lexical-variable-info
            (:constructor make-lexical-variable-info (frame-offset cfunction))
            (:include ignorable-lexical-info))
  (closed-over-p nil :type boolean)
  (setp nil :type boolean))

;;; Info for blocks and tagbody, where we don't want to note unusedness,
;;; but do need to track if they're closed over.
(defstruct (lexical-exit-info
            (:constructor make-lexical-exit-info (frame-offset cfunction))
            (:include lexical-info))
  (closed-over-p nil :type boolean))

(defstruct (lexical-destination
            (:constructor make-lexical-destination (exit-info label)))
  (exit-info (missing-arg) :type lexical-exit-info)
  (label (missing-arg)))

(defun frame-offset (lex-desc)
  (lexical-info-frame-offset (trucler:identity lex-desc)))
(defun lvar-cfunction (lex-desc)
  (lexical-info-cfunction (trucler:identity lex-desc)))
(defun lvar-readp (lex-desc)
  (ignorable-lexical-info-readp (trucler:identity lex-desc)))
(defun (setf lvar-readp) (new lex-desc)
  (setf (ignorable-lexical-info-readp (trucler:identity lex-desc)) new))

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

(defun make-block (name frame-offset cfunction label)
  (make-instance 'trucler:block-description
    :name name
    :identity (make-lexical-destination
               (make-lexical-exit-info frame-offset cfunction)
               label)))
(defun make-tag (name exit-info label)
  (make-instance 'trucler:tag-description
    :name name
    :identity (make-lexical-destination exit-info label)))

(defun make-lexical-variable (name frame-offset cfunction
                              &key ignore (type t) dynamic-extent)
  (make-instance 'trucler:lexical-variable-description
    :name name
    :identity (make-lexical-variable-info frame-offset cfunction)
    :ignore ignore :type type :dynamic-extent dynamic-extent))

;;; Return a new trucler desc, given an existing desc and some declarations
;;; (as returned from PROCESS-DECLARATIONS).
;;; This should probably use TRUCLER:QUASI-CLONE, except that only takes one pair
;;; of initarg and value, for some reason?
(defgeneric augment-description (variable-description variable-declarations))
(defmethod augment-description ((desc trucler:lexical-variable-description)
                                vdecls)
  (make-instance 'trucler:lexical-variable-description
    :name (trucler:name desc) :identity (trucler:identity desc)
    :ignore (or (cdr (assoc 'ignore vdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type vdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))
    :dynamic-extent (or (not (null (assoc 'dynamic-extent vdecls)))
                      (trucler:dynamic-extent desc))))
(defmethod augment-description
    ((desc trucler:local-special-variable-description) vdecls)
  (make-instance 'trucler:local-special-variable-description
    :name (trucler:name desc)
    :ignore (or (cdr (assoc 'ignore vdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type vdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))
    :dynamic-extent (or (not (null (assoc 'dynamic-extent vdecls)))
                      (trucler:dynamic-extent desc))))
(defmethod augment-description
    ((desc trucler:global-special-variable-description) vdecls)
  (make-instance 'trucler:global-special-variable-description
    :name (trucler:name desc)
    :ignore (or (cdr (assoc 'ignore vdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type vdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))
    :dynamic-extent (or (not (null (assoc 'dynamic-extent vdecls)))
                      (trucler:dynamic-extent desc))))

(defmethod augment-description
    ((desc trucler:global-symbol-macro-description) vdecls)
  (make-instance 'trucler:global-symbol-macro-description
    :name (trucler:name desc) :expansion (trucler:expansion desc)
    :ignore (or (cdr (assoc 'ignore vdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type vdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))))
(defmethod augment-description
    ((desc trucler:local-symbol-macro-description) vdecls)
  (make-instance 'trucler:local-symbol-macro-description
    :name (trucler:name desc) :expansion (trucler:expansion desc)
    :ignore (or (cdr (assoc 'ignore vdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type vdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))))

(defmethod augment-description
    ((desc trucler:global-function-description) fdecls)
  (make-instance 'trucler:global-function-description
    :name (trucler:name desc) :compiler-macro (trucler:compiler-macro desc)
    :ignore (or (cdr (assoc 'ignore fdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type fdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))
    :dynamic-extent (or (not (null (assoc 'dynamic-extent fdecls)))
                      (trucler:dynamic-extent desc))
    :inline (or (cdr (assoc 'inline fdecls)) (trucler:inline desc))
    :inline-data (trucler:inline-data desc)))
(defmethod augment-description
    ((desc trucler:local-function-description) fdecls)
  (make-instance 'trucler:local-function-description
    :name (trucler:name desc) :identity (trucler:identity desc)
    :ignore (or (cdr (assoc 'ignore fdecls)) (trucler:ignore desc))
    :type (let ((existing (trucler:type desc))
                (newpair (assoc 'type fdecls)))
            (cond ((null newpair) existing)
                  ((eq existing 't) (cdr newpair))
                  (t `(and ,(cdr newpair) ,existing))))
    :dynamic-extent (or (not (null (assoc 'dynamic-extent fdecls)))
                      (trucler:dynamic-extent desc))
    :inline (or (cdr (assoc 'inline fdecls)) (trucler:inline desc))
    :inline-data (trucler:inline-data desc)))

(defun make-symbol-macro (name expansion &key ignore (type t))
  (make-instance 'trucler:local-symbol-macro-description
    :name name :expansion expansion :ignore ignore :type type))

(defun globally-special-p (symbol env)
  (typep (var-info symbol env) 'trucler:global-special-variable-description))

(defun make-local-function (name frame-offset cfunction
                            &key ignore (type t) dynamic-extent
                              inline inline-data)
  (make-instance 'trucler:local-function-description
    :name name :ignore ignore :type type :dynamic-extent dynamic-extent
    :inline inline :inline-data inline-data
    :identity (make-ignorable-lexical-info frame-offset cfunction)))

(defun make-local-macro (name expander &key ignore)
  (make-instance 'trucler:local-macro-description
    :name name :expander expander :ignore ignore))

(defun add-macros (env macros)
  (make-lexical-environment env :funs (append macros (funs env))))

(defun add-symbol-macros (env symbol-macros)
  (make-lexical-environment env :vars (append symbol-macros (vars env))))
