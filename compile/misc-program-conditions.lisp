(in-package #:maclina.compile)

(define-condition bind-constant (compiler-program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "Attempt to bind constant variable ~s"
                     (name condition)))))

(define-condition go-tag-not-tag (compiler-program-error)
  ((%tag :initarg :tag :reader tag))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid ~s tag" (tag condition) 'go))))

(define-condition no-go (compiler-program-error)
  ((%tag :initarg :tag :reader tag))
  (:report (lambda (condition stream)
             (format stream "Attempt to ~s to unknown tag ~s"
                     'go (tag condition)))))

(define-condition block-name-not-symbol (compiler-program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid ~s name"
                     (name condition) 'block))))

(define-condition no-return (compiler-program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "Attempt to ~s unknown block ~s"
                     'return-from (name condition)))))

(define-condition invalid-eval-when-situation (compiler-program-error)
  ((%situation :initarg :situation :reader situation))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid ~s situation"
                     (situation condition) 'eval-when))))

(define-condition variable-not-symbol (compiler-program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid variable name"
                     (name condition)))))

(define-condition used (compiler-program-style-warning)
  ((%name :initarg :name :reader name)
   (%kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "~:(~a~) ~s was declared ~s, but was still used"
                     (kind condition) (name condition) 'cl:ignore))))

(define-condition unused (compiler-program-style-warning)
  ((%name :initarg :name :reader name)
   (%kind :initarg :kind :reader kind :type (member function variable)))
  (:report (lambda (condition stream)
             (format stream "Unused ~(~a~) ~s"
                     (kind condition) (name condition)))))

(define-condition set-unused (compiler-program-style-warning)
  ((%name :initarg :name :reader name)
   ;; In practice local function bindings cannot be modified,
   ;; so this field is a bit pointless. It's in for symmetry.
   (%kind :initarg :kind :reader kind :type (member function variable)))
  (:report (lambda (condition stream)
             (format stream "~:(~a~) ~s set but not used"
                     (kind condition) (name condition)))))

(define-condition not-function-name (compiler-program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid function name"
                     (name condition)))))

(define-condition not-fnameoid (compiler-program-error)
  ((%fnameoid :initarg :fnameoid :reader fnameoid))
  (:report (lambda (condition stream)
             (format stream "Parameter to ~s is not a valid function name or lambda expression: ~s"
                     'cl:function (fnameoid condition)))))

(define-condition not-declaration (compiler-program-error)
  ((%specifier :initarg :specifier :reader specifier))
  (:report (lambda (condition stream)
             (format stream "~s is not a valid declaration specifier"
                     (specifier condition)))))

(define-condition setq-uneven (compiler-program-error)
  ((%remainder :initarg :remainder :reader remainder))
  (:report (lambda (condition stream)
             (format stream "~s given uneven number of variables and values: ~s"
                     'setq (remainder condition)))))

(define-condition improper-body (compiler-program-error)
  ((%body :initarg :body :reader body))
  (:report (lambda (condition stream)
             (format stream "Body forms are not a proper list: ~s"
                     (body condition)))))

(define-condition improper-arguments (compiler-program-error)
  ((%args :initarg :args :reader args))
  (:report (lambda (condition stream)
             (format stream "Arguments are not a proper list: ~s"
                     (args condition)))))

(define-condition improper-bindings (compiler-program-error)
  ((%bindings :initarg :bindings :reader bindings))
  (:report (lambda (condition stream)
             (format stream "Bindings are not a proper list: ~s"
                     (bindings condition)))))

(define-condition improper-situations (compiler-program-error)
  ((%situations :initarg :situations :reader situations))
  (:report (lambda (condition stream)
             (format stream "~a situations are not a proper list: ~s"
                     'eval-when (situations condition)))))

(define-condition improper-declarations (compiler-program-error)
  ((%declarations :initarg :declarations :reader declarations))
  (:report (lambda (condition stream)
             (format stream "Declarations are not a proper list: ~s"
                     (declarations condition)))))

;;; from cleavir
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

;;; this is alexandria:parse-body, but checks for properness first.
(defun parse-body (body &rest keys &key documentation whole)
  (declare (ignore documentation whole))
  (if (proper-list-p body)
      (apply #'alexandria:parse-body body keys)
      (error 'improper-body :body body)))
