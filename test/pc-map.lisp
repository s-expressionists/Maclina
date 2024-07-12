(in-package #:maclina.test)

;;; Tests of PC-mapped information, like source locations.

(5am:def-suite pc-map :in maclina)
(5am:in-suite pc-map)

;;; Pending a clean way to get PCs corresponding to executing forms
;;; (like from backtraces), we basically just test that infos are
;;; present and properly nested.

(5am:test source
  (let* ((sources (make-hash-table))
         (add '(+ x y))
         (add2 `(+ ,add z)))
    (setf (gethash add sources) 13 ; arbitrary "source locations"
          (gethash add2 sources) 9)
    (let* ((maclina.compile:*source-locations* sources)
           (f (ccompile nil `(lambda (x y z) ,add2)))
           (mod (maclina.machine:bytecode-function-module f))
           (outerp nil) (innerp nil))
      ;; Weird state machine. We want :before :add2 :add :add2-after :after,
      ;; except these can be abbreviated (if sources begin or end simultaneously).
      (loop with state = :before
            with success = t
            for pc below (length (maclina.machine:bytecode-module-bytecode mod))
            for source = (maclina.machine:source-at mod pc)
            do (case source
                 ((nil)
                  (case state
                    ((:before :after))
                    ((:add2)
                     (5am:fail "Missing source info for inner form")
                     (setf success nil) (loop-finish))
                    ((:add :add2-after) (setf state :after))))
                 (9
                  (setf outerp t)
                  (case state
                    ((:before) (setf state :add2))
                    ((:add2 :add2-after))
                    ((:add) (setf state :add2-after))
                    ((:after)
                     (5am:fail "Source info resumes after ending")
                     (setf success nil) (loop-finish))))
                 (13
                  (setf innerp t)
                  (case state
                    ((:before :add2) (setf state :add))
                    ((:add))
                    ((:add2-after :after)
                     (5am:fail "Source info resumes after ending")
                     (setf success nil) (loop-finish))))
                 (otherwise
                  (5am:fail "Unexpected source location ~s" source)
                  (setf success nil) (loop-finish)))
            finally (when success (5am:pass)))
      (if outerp
          (5am:pass)
          (5am:fail "Missing source info for outer call"))
      (if innerp
          (5am:pass)
          (5am:fail "Missing source info for inner call")))))
