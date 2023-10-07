(in-package #:maclina.compile)

#|
Our basic philosophy for compiler conditions is as follows.
The compiler interface (COMPILE, COMPILE-FILE) never signals errors intentionally. Any syntax "errors" are only signaled out as warnings at most. They may originate as errors internally, but handlers in the compiler must handle them and signal a warning instead.
If an error does get signaled, it's because the compiler actually cannot continue in any obviously reasonable way, for example because there's a bug in the compiler, or if there's a READER-ERROR in COMPILE-FILE.. As such we let it go to the debugger. If it is handled, the error is suppressed and compilation proceeds without failing.
Warnings (and style warnings) are actually signaled by the compiler. Muffling them does _not_ suppress them for the purposes of reckoning failure. The compiler may however establish additional restarts which do actually suppress them. Most obviously, the compiler will signal a full warning for unknown variables and a style warning for unknown functions. A surrounding WITH-COMPILATION-UNIT will then suppress them while keeping them noted, and display them at the end of the compilation unit if they have not been resolved.

About compilation units and failure values: if the compiler encounters a resolvable problem that is not resolved by the time it finishes, its return values depend on whether it's in an outer compilation unit or not. If it's not, it reports failure/warnings. If it is, it doesn't, since something else later in the compilation unit might fix it. Therefore WITH-COMPILATION-RESULTS goes _outside_ the compiler's inner WITH-COMPILATION-UNIT.
If the compiler encounters an unresolvable problem it can of course fail immediately and report that.
|#

;;;; TODO: Make all this client extensible as soon as I can think of a model
;;;; for why a client would want to do that.

;;; Evaluate BODY as a progn. Return three values: the primary value returned
;;; by the BODY, warnings-p, and failure-p.
;;; warnings-p will be true if the body signaled an unhandled ERROR or WARNING.
;;; failure-p will be true if the body signaled an unhandled ERROR, or a WARNING
;;;  other than a STYLE-WARNING.
;;; Useful for implementing COMPILE and COMPILE-FILE return values.
(defmacro with-compilation-results (&body body)
  (let ((warningsp (gensym "WARNINGSP")) (failurep (gensym "FAILUREP")))
    `(let ((,warningsp nil) (,failurep nil))
       ;; We resignal to see if anything outside wants to handle or otherwise
       ;; react. For warnings we use WARN, so that mere MUFFLE-WARNING does
       ;; not stop us from treating it as a failure/warning.
       (handler-bind ((style-warning
                        (lambda (w)
                          (warn w)
                          (setq ,warningsp t)
                          (muffle-warning w)))
                      ((and warning (not style-warning))
                        (lambda (w)
                          (warn w)
                          (setq ,warningsp t ,failurep t)
                          (muffle-warning w)))
                      (error
                        (lambda (e)
                          (signal e)
                          (setq ,warningsp t ,failurep t))))
         (values ,@body ,warningsp ,failurep)))))

(defvar *in-compilation-unit* nil)

(defun call-with-compilation-unit (thunk &key override)
  (if (or (not *in-compilation-unit*) override)
      (let ((*in-compilation-unit* t)
            (unknown-references nil)
            (nerrors 0) (nwarnings 0) (nstylewarnings 0)
            (abortedp t))
        (unwind-protect
             (multiple-value-prog1
                 (handler-bind
                     ((unknown-reference
                        (lambda (r)
                          (push r unknown-references)
                          (continue r)))
                      (style-warning
                        (lambda (w) (signal w) (incf nstylewarnings)))
                      ((and warning (not style-warning))
                        (lambda (w) (signal w) (incf nwarnings)))
                      (error
                        (lambda (e) (signal e) (incf nerrors))))
                   ;; resolve-reference may signal warnings, which we want
                   ;; to be handled like any other warning, above.
                   (handler-bind
                       ((unknown-reference-resolution
                          (lambda (ru)
                            (setf unknown-references
                                  (delete ru unknown-references
                                          :test #'resolve-reference)))))
                     (funcall thunk)))
               (setq abortedp nil))
          (compilation-unit-finished abortedp unknown-references
                                     nerrors nwarnings nstylewarnings)))
      (funcall thunk)))

(defun compilation-unit-finished (abortedp unknown-references
                                  nerrors nwarnings nstylewarnings)
  (handler-bind ((style-warning
                   (lambda (w) (declare (ignore w)) (incf nstylewarnings)))
                 ((and warning (not style-warning))
                   (lambda (w) (declare (ignore w)) (incf nwarnings))))
    (mapc #'warn unknown-references))
  (unless (and (not abortedp)
               (zerop nerrors) (zerop nwarnings) (zerop nstylewarnings))
    (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
      (format *error-output* "~&compilation unit ~:[finished~;aborted~]"
              abortedp)
      (format *error-output* "~[~:;~:*~&  caught ~w ERROR condition~:P~]~
                              ~[~:;~:*~&  caught ~w WARNING condition~:P~]~
                              ~[~:;~:*~&  caught ~w STYLE-WARNING condition~:P~]"
              nerrors nwarnings nstylewarnings))
    (terpri *error-output*)
    (force-output *error-output*)))

(defmacro with-compilation-unit ((&rest options &key override) &body body)
  (declare (ignore override))
  `(call-with-compilation-unit (lambda () (progn ,@body)) ,@options))
