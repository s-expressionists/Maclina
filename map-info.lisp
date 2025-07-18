(in-package #:maclina.machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Map infos: Things in the PC map.
;;; These have a start and end index as well as accessory information.
;;; They are used for debug information as well as information about the
;;; original structure of Lisp programs.
;;; Not everything in the info map is actually an info: it also includes
;;; bytecode-functions. But they still have the START and END readers.

;;; The compiler generates infos with a few convenient restrictions:
;;; 1) Info ranges are nested. That is, if one range intersects another,
;;;    at least one of the ranges contains the entirety of the other.
;;; 2) Infos are sorted in the map by start and then by end. That is, iff
;;;    info1 appears before info2 in the map, either info1's start is < that of
;;;    info2, or they are = and info1's end is <= that of info2. Infos with
;;;    the same start and end are ordered indeterminately.

(defclass map-info ()
  (;; During compilation, these will be labels. The accessors are them used
   ;; to make them into actual indices. The writers should not be used
   ;; outside of link-pc-map in the compiler.
   (%start :initarg :start :accessor start)
   (%end :initarg :end :accessor end)))

(defclass source-info (map-info)
  ((%source :initarg :source :reader source)))

;;; These structure-info objects record some details of the original Lisp code
;;; that are otherwise lost in bytecode compilation. Recording this information
;;; makes it easier to performance further analysis and compilation.
(defclass program-structure-info (map-info) ())

(defclass declarations-info (program-structure-info)
  ((%declarations :initarg :declarations :reader declarations)))

(defclass the-info (program-structure-info)
  ((%type :initarg :type :reader the-type)
   ;; Indicates what values are typed. As in the bytecode compiler.
   (%receiving :initarg :receiving :reader receiving)))

(defclass if-info (program-structure-info)
  (;; Indicates what values are returned by the IF form.
   (%receiving :initarg :receiving :reader receiving)))

(defclass tagbody-info (program-structure-info)
  (;; An alist (go-tag . label)
   (%tags :initarg :tags :reader tags)))

;; Also used for information about CATCH.
(defclass block-info (program-structure-info)
  (;; Name of the block. For CATCH forms, this will be the symbol CL:CATCH.
   (%name :initarg :name :reader name)
   ;; Indicates what values are returned by the BLOCK or CATCH form.
   (%receiving :initarg :receiving :reader receiving)))

;;; This one is also pretty useful for debugging, since it lets you reconstruct
;;; the current lexical variable and function bindings at any IP.
(defclass vars-info (program-structure-info)
  (;; A list of var-infos.
   ;; During compilation, temporarily a list of lexical variable infos.
   (%bindings :initarg :bindings :reader bindings)))

(defclass var-info ()
  ((%name :initarg :name :reader name)
   (%index :initarg :index :reader index :type (unsigned-byte 16))
   (%cellp :initarg :cellp :reader cellp :type boolean)
   (%declarations :initarg :declarations :reader declarations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allow bytecode functions to be infos.
;;;

(defmethod start ((info bytecode-function))
  (bytecode-function-entry-pc info))
(defmethod end ((info bytecode-function))
  (+ (bytecode-function-entry-pc info) (bytecode-function-size info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience accessors
;;;

;;; Return a list of all infos for the given PC. Most specific infos first.
(defun info-at (module pc)
  (loop with result = ()
        for info across (bytecode-module-pc-map module)
        until (< pc (start info))
        when (< pc (end info))
          do (cl:push info result)
        finally (cl:return result)))

;;; Get the most specific info matching the predicate, for the given PC.
(defun most-specific-info-at (module pc predicate)
  (loop with best = ()
        for info across (bytecode-module-pc-map module)
        for end = (end info)
        until (< pc (start info))
        when (and (< pc end)
                  (funcall predicate info)
                  (or (null best) (< end (end best))))
          do (setf best info)
        finally (cl:return info)))

(defun source-at (module pc)
  (let ((info (most-specific-info-at
               module pc (lambda (info) (typep info 'source-info)))))
    (if info
        (source info)
        nil)))

(defun first-info-at (module pc predicate)
  (loop for info across (bytecode-module-pc-map module)
        when (and (<= (start info) pc) (< pc (end info))
                  (funcall predicate info))
          return info))

(defun function-at (module pc)
  (first-info-at module pc (lambda (info) (typep info 'bytecode-function))))
