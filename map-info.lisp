(in-package #:maclina.machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Map infos: Things in the PC map.
;;; These have a start and end index as well as accessory information.
;;; They are used for debug information as well as information about the
;;; original structure of Lisp programs.

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
