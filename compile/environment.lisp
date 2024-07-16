(in-package #:maclina.compile)

;; never actually called
(defun missing-arg () (error "missing arg"))

(defstruct (lexical-environment (:constructor make-null-lexical-environment
                                    (global-environment))
                                (:constructor %make-lexical-environment)
                                (:conc-name nil))
  ;; An alist of (var . lvar-desc) in the current environment.
  (vars nil :type list :read-only t)
  ;; An alist of (tag dynenv-desc . label) in the current environment.
  (tags nil :type list :read-only t)
  ;; An alist of (block block-dynenv . label) in the current environment.
  (blocks nil :type list :read-only t)
  ;; An alist of (fun . lfun-desc) in the current environment.
  (funs nil :type list :read-only t)
  ;; Global environment, which we just pass to Trucler.
  (global-environment (missing-arg) :read-only t))

;;; We don't use Trucler's augmentation protocol internally since we often
;;; want to add a bunch of stuff at once, which is awkward in Trucler.
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
  (declare (ignore client))
  (global-environment env))

(defmethod trucler:describe-variable
    (client (env lexical-environment) name)
  (or (cdr (assoc name (vars env)))
      (trucler:describe-variable client (global-environment env) name)))

(defmethod trucler:describe-function
    (client (env lexical-environment) name)
  (or (cdr (assoc name (funs env) :test #'equal))
      (trucler:describe-function client (global-environment env) name)))

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

;;; Our info for lexical bindings (variable and function).
(defstruct (lexical-info
            (:constructor make-lexical-info (frame-offset cfunction)))
  ;; Register index for this lvar.
  (frame-offset (missing-arg) :read-only t :type (integer 0))
  ;; Cfunction this lvar belongs to (i.e. is bound by)
  (cfunction (missing-arg) :read-only t :type cfunction)
  ;; Has the variable been read (for cl:ignore tracking).
  (readp nil :type boolean))

;;; Our info for specifically variable bindings.
;;; (while function bindings can be closed over, they can't be modified,
;;;  so we don't really care.)
(defstruct (lexical-variable-info
            (:constructor make-lexical-variable-info (frame-offset cfunction))
            (:include lexical-info))
  (closed-over-p nil :type boolean)
  (setp nil :type boolean))

(defun frame-offset (lex-desc)
  (lexical-info-frame-offset (trucler:identity lex-desc)))
(defun lvar-cfunction (lex-desc)
  (lexical-info-cfunction (trucler:identity lex-desc)))
(defun lvar-readp (lex-desc)
  (lexical-info-readp (trucler:identity lex-desc)))
(defun (setf lvar-readp) (new lex-desc)
  (setf (lexical-info-readp (trucler:identity lex-desc)) new))

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

(defun make-lexical-variable (name frame-offset cfunction &key ignore)
  (make-instance 'trucler:lexical-variable-description
    :name name
    :identity (make-lexical-variable-info frame-offset cfunction)
    :ignore ignore))

(defun make-symbol-macro (name expansion)
  (make-instance 'trucler:symbol-macro-description
    :name name :expansion expansion))

(defun globally-special-p (symbol env)
  (typep (var-info symbol env) 'trucler:global-special-variable-description))

(defun make-local-function (name frame-offset cfunction &key ignore)
  (make-instance 'trucler:local-function-description
    :name name :ignore ignore
    :identity (make-lexical-info frame-offset cfunction)))

(defun make-local-macro (name expander)
  (make-instance 'trucler:local-macro-description
    :name name :expander expander))

(defun add-macros (env macros)
  (make-lexical-environment env :funs (append macros (funs env))))

(defun add-symbol-macros (env symbol-macros)
  (make-lexical-environment env :vars (append symbol-macros (vars env))))
