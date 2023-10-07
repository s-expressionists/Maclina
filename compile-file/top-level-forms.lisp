(in-package #:maclina.compile-file)

;;; COMPILE-TOPLEVEL will process a toplevel form in an environment.
;;; It uses CMP:EVAL to carry out compile time evaluations.
;;; For dumping, it will call back into the rest of the file compiler via
;;; COMPILE-FILE-FORM.

(defvar *compile-time-too*)

(defun compile-toplevel-progn (forms env)
  (dolist (form forms)
    (compile-toplevel form env)))

(defun compile-toplevel-eval-when (situations forms env)
  (let ((ct (or (member :compile-toplevel situations)
                (member 'cl:compile situations)))
        (lt (or (member :load-toplevel situations)
                (member 'cl:load situations)))
        (e (or (member :execute situations)
               (member 'cl:eval situations)))
        (ctt *compile-time-too*))
    ;; Following CLHS figure 3-7 pretty exactly.
    (cond ((or (and ct lt) (and lt e ctt)) ; process compile-time-too
           (let ((*compile-time-too* t))
             (compile-toplevel-progn forms env)))
          ((or (and lt e (not ctt)) (and (not ct) lt (not e)))
           ;; process not-compile-time
           (let ((*compile-time-too* nil))
             (compile-toplevel-progn forms env)))
          ((or (and ct (not lt)) (and (not ct) (not lt) e ctt))
           ;; evaluate
           (cmp:eval-progn forms env))
          (t
           ;; (or (and (not ct) (not lt) e (not ctt)) (and (not ct) (not lt) (not e)))
           ;; discard
           nil))))

(defun compile-toplevel-locally (body env)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let* ((new-env (cmp:add-declarations env decls)))
      (compile-toplevel-progn body new-env))))

(defun compile-toplevel-macrolet (bindings body env)
  (let ((macros nil)
        (aenv (cmp:lexenv-for-macrolet env)))
    (dolist (binding bindings)
      (let* ((name (car binding)) (lambda-list (cadr binding))
             (body (cddr binding))
             (expander (cmp:compute-macroexpander
                        name lambda-list body aenv))
             (info (cmp:make-local-macro name expander)))
        (push (cons name info) macros)))
    (compile-toplevel-locally
     body (cmp:add-macros env macros))))

(defun compile-toplevel-symbol-macrolet (bindings body env)
  (let ((smacros
          (loop for (name expansion) in bindings
                for info = (cmp:make-symbol-macro name expansion)
                collect (cons name info))))
    (compile-toplevel-locally
     body (cmp:add-symbol-macros env (nreverse smacros)))))

(defun compile-toplevel (form &optional env)
  (let ((form (cmp:macroexpand form env)))
    (if (consp form)
        (case (car form)
          ((progn) (compile-toplevel-progn (cdr form) env))
          ((eval-when)
           (compile-toplevel-eval-when (cadr form) (cddr form) env))
          ((locally) (compile-toplevel-locally (cdr form) env))
          ((macrolet)
           (compile-toplevel-macrolet (cadr form) (cddr form) env))
          ((symbol-macrolet)
           (compile-toplevel-symbol-macrolet (cadr form) (cddr form) env))
          (otherwise
           (when *compile-time-too* (cmp:eval form env))
           (compile-file-form form env)))
        (progn
          (when *compile-time-too* (cmp:eval form env))
          (compile-file-form form env)))))
