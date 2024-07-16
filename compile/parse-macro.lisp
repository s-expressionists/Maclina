(in-package #:maclina.compile)

#|
Here's the skinny.
Any lisp compiler needs to handle MACROLET. To handle macrolet, a compiler needs to be able to take the code for the local macro definitions, evaluate them _at compile time_, and get an _actual function it can call_ for macroexpansion. Furthermore the local macro definitions must be evaluated in the environment a compiler is working with - because for example macrolet definitions can refer to macros defined in outer macrolets.
Plus, the local macro definitions have macro lambda lists, which can be quite complicated. When the macro runs it performs a complex destructuring operation that will in general involve complex loops and many function calls.
Traditionally, this all is accomplished with a function called PARSE-MACRO. PARSE-MACRO takes as input the macro definition (lambda-list, body, etc.). It returns a lambda expression. The compiler then evaluates this lambda expression in its environment to get the function.
This does not work for us. The reason lies in first class global environments. We want to support compilation in _any_ environment. If the environment contains MACROLET, compilation in it should work even if standard functions, macros, or even special operators are not available in that environment, or have different names, etc. Therefore, with the usual PARSE-MACRO approach, the form returned by PARSE-MACRO cannot contain any operators not intrinsic to evaluation, which means all you get is lambda forms - obviously not enough.
Instead, what we do is use a _host_ function as our macroexpander. The compiler calls PARSE-MACRO and gets back a lambda expression. This lambda expression is then evaluated in a null lexical environment _in the host_ (e.g., by CL:COMPILE or CL:EVAL, rather than anything necessarily Maclina-related). The resulting function parses the macro arguments _with host code_, and therefore it doesn't matter what environment the compiler is working with.
Of course, the actual macro body, as well as any default forms in the lambda list, must still be compiled by our compiler in the appropriate environment, rather than by the host. To accomplish this, PARSE-MACRO receives a callback, called COMPILER below. This is a function of two arguments and one keyword: a lambda expression, a compiler environment, and a :BLOCK-NAME. The function must compile the lambda in the environment and return a callable function. If :BLOCK-NAME is provided the body will have a block of that name wrapped around it.
And here's the really funky part: PARSE-MACRO then includes this function as a literal object in the expression it returns. So the expression will have code looking something like `(funcall #<BYTECODE-FUNCTION ...> a b c)`. This lets the host function call our bytecode-compiled function even though they were compiled by completely different means in completely different kinds of environments. Literal functions are quite unusual in code, but since the macroexpander only needs to go through CL:COMPILE or CL:EVAL, and not CL:COMPILE-FILE (as macroexpanders are not dumped), this works out.
End result: a callable host function that does lambda list processing independently of bytecode anything, but still calls into the bytecode when semantics demand it.

We also reuse this machinery to bind subexpressions of forms in the compiler. See DESTRUCTURE-SYNTAX.
|#

;;; returns four values:
;;; 1) a list of bindings in the host
;;; 2) a list of variables to declare ignorable after those bindings
;;; 3) a list of arguments in the host to the cross function
;;; 4) a list of parameters for the cross function
(defun process-lambda-list (lambda-list compiler environment
                            target etarget toplevelp
                            &key arguments parameters source)
  (let* ((bindings nil) (ignorables nil)
         (arguments arguments) (parameters parameters)
         (whole (ecclesia:whole lambda-list))
         (required (ecclesia:required lambda-list))
         (nreq (length required))
         (%optionals (ecclesia:optionals lambda-list))
         ;; we treat () and (&optional) the same.
         (optionals (if (eq %optionals :none) nil %optionals))
         (nopt (length optionals))
         (rest (ecclesia:rest-body lambda-list))
         (restp (not (eq rest :none)))
         (%keys (ecclesia:keys lambda-list))
         (keysp (not (eq %keys :none)))
         (keys (if keysp %keys nil))
         (aokp (ecclesia:allow-other-keys lambda-list))
         (nmax (if (or keysp restp) nil (+ nreq nopt)))
         (nargs (gensym "NARGS"))
         (%aux (ecclesia:aux lambda-list))
         ;; and () and (&aux) the same
         (aux (if (eq %aux :none) () %aux))
         (eparam (ecclesia:environment lambda-list)))
    (labels
        ((destructure (sub-lambda-list new-target-form)
           (let ((targ (gensym "SUBARGS"))
                 ;; ecclesia sometimes returns lists
                 ;; and other times returns a LAMBDA-LIST object.
                 (sub-lambda-list
                   (etypecase sub-lambda-list
                     (list (ecclesia:parse-macro-lambda-list
                            sub-lambda-list))
                     (ecclesia:lambda-list sub-lambda-list))))
             (push `(,targ ,new-target-form) bindings)
             (push targ ignorables)
             (multiple-value-bind (%binds %ign %args %params)
                 (process-lambda-list sub-lambda-list
                                      compiler environment
                                      targ etarget nil
                                      :source source
                                      :arguments arguments
                                      :parameters parameters)
               (setf bindings (append (reverse %binds) bindings)
                     ignorables (append %ign ignorables)
                     arguments (reverse %args)
                     parameters (reverse %params)))))
         (bind-presentp (listparam testf)
           (if (null listparam)
               ;; no -p provided: make one
               (let ((-p (gensym "PRESENTP")))
                 (push `(,-p ,testf) bindings)
                 -p)
               ;; provided: use it and add to args etc
               (let* ((-p (first listparam))
                      (s-p (gensym (symbol-name -p))))
                 (push `(,s-p ,testf) bindings)
                 (push s-p arguments)
                 (push -p parameters)
                 s-p)))
         (bind (thing valuef)
           (etypecase thing
             ((or list ecclesia:lambda-list)
              (destructure thing valuef))
             (symbol (let ((arg (gensym (symbol-name thing))))
                       (push `(,arg ,valuef) bindings)
                       (push arg arguments)
                       (push thing parameters)))))
         (default (form)
           (let* ((rparams (reverse parameters))
                  (rargs (reverse arguments))
                  (lexpr `(lambda (,@rparams)
                            (declare (ignorable ,@rparams))
                            ,form))
                  (thunk (funcall compiler lexpr environment)))
             `(funcall ,thunk ,@rargs))))
      ;; Environment parameter
      (unless (eq eparam :none) (bind eparam etarget))
      ;; Whole parameter
      (unless (eq whole :none) (bind whole target))
      ;; If we're at toplevel, take the rest
      (when toplevelp
        (let ((new-target (gensym "ARGS")))
          (push `(,new-target (rest ,target)) bindings)
          (push new-target ignorables)
          (setf target new-target)))
      ;; Argument propriety
      (let ((propriety (gensym "CHECK-PROPRIETY")))
        (push `(,propriety (unless (proper-list-p ,target)
                             (error 'improper-arguments :args ,target
                                                        :source ,source)))
              bindings)
        (push propriety ignorables))
      ;; Argument count
      (push `(,nargs (length ,target)) bindings)
      (push nargs ignorables)
      ;; Push arg count checks if needed
      ;; do this before evaluating any default forms, to be nice
      (when (or (> nreq 0) nmax)
        (let ((s (gensym "ARGCOUNT-CHECK")))
          (push `(,s
                  (unless (<= ,nreq ,nargs ,@(when nmax `(,nmax)))
                    (error 'wrong-number-of-arguments
                           :source ,source
                           :given-nargs ,nargs
                           :min-nargs ,nreq
                           ,@(when nmax `(:max-nargs ,nmax)))))
                bindings)
          (push s ignorables)))
      (when keysp
        (let ((s (gensym "EVEN-KEYS-CHECK")))
          (push `(,s (unless (evenp (- ,nargs ,(+ nreq nopt)))
                       (error 'odd-keywords :source ,source)))
                bindings)
          (push s ignorables)))
      ;; Required parameters
      (loop for r in required
            for i from 0
            do (bind r `(nth ,i ,target)))
      ;; Optional parameters
      (loop for (o dform . -p) in optionals
            for i from nreq
            for def = (default dform)
            for s-p = (bind-presentp -p `(> ,nargs ,i))
            do (bind o `(if ,s-p (nth ,i ,target) ,def)))
      ;; Rest parameter
      (when restp (bind rest `(nthcdr ,(+ nreq nopt) ,target)))
      ;; Key parameters
      (when keysp
        (let ((keytarg (gensym "KEYS")))
          (push `(,keytarg (nthcdr ,(+ nreq nopt) ,target)) bindings)
          (push keytarg ignorables)
          ;; check for unknown keys
          (unless aokp
            (let ((key-check (gensym "UNKNOWN-KEYS-CHECK"))
                  (valid-keys (mapcar #'caar keys)))
              (push `(,key-check
                      (check-keywords ',valid-keys ,keytarg ,source))
                    bindings)
              (push key-check ignorables)))
          ;; Bind keys
          (loop for ((kw var) dform . -p) in keys
                for def = (default dform)
                for s-p = (bind-presentp -p `(keyword-presentp ',kw ,keytarg))
                do (bind var `(if ,s-p (keyword-find ',kw ,keytarg) ,def)))))
      ;; Aux parameters
      (loop for (a dform) in aux
            do (bind a (default dform))))
    (values (reverse bindings) ignorables
            (reverse arguments) (reverse parameters))))

;;; Given the list of valid keys and a plist, signal an error if the
;;; plist contains an invalid keyword, while respecting
;;; that most fun of party tricks, :allow-other-keys.
;;; It is guaranteed that plist is a list with an even length.
;;; This function is only called when &allow-other-keys is
;;; not present.
;;; Return value undefined.
(defun check-keywords (valid-keys plist &optional source)
  (loop with seen-aok = nil ; see 3.4.1.4.1.1
        for (key val) on plist by #'cddr
        when (and (not seen-aok) (eq key :allow-other-keys))
          do (when val
               ;; we're not doing anything else with the keywords,
               ;; so just leave
               (return-from check-keywords))
             (setf seen-aok t)
        unless (or (member key valid-keys)
                   (eq key :allow-other-keys)) ; always valid
          collect key into unknown-keys
        finally (when unknown-keys
                  (error 'unrecognized-keyword-argument
                         :source source
                         :unrecognized-keywords unknown-keys))))

;;; Check if a keyword is in the plist. The plist is valid and has
;;; even length.
;;; Obviously it would be more efficient to check for presence and
;;; get the value in one go, but this file is messy enough as-is.
(defun keyword-presentp (key plist)
  (loop for (pkey) on plist by #'cddr
        when (eq key pkey)
          return t
        finally (return nil)))

;;; Get the value associated with a keyword in the plist.
;;; The plist is valid and has even length.
(defun keyword-find (key plist) (getf plist key))

(defun parse-macro (name lambda-list body environment compiler)
  (multiple-value-bind (bindings ignorables arguments parameters)
      (process-lambda-list
       (ecclesia:parse-macro-lambda-list lambda-list)
       compiler environment 'form 'environment t)
    (let ((bodyf (funcall compiler `(lambda (,@parameters) ,@body) environment
                          :block-name name)))
      `(lambda (form environment)
         (declare (ignorable environment))
         (let* (,@bindings)
           (declare (ignorable ,@ignorables))
           (funcall ,bodyf ,@arguments))))))

(defmacro destructure-syntax ((op &rest lambda-list) (form &key (rest t) source)
                              &body body)
  (declare (ignore op))
  (alexandria:once-only (form)
    (multiple-value-bind (bindings ignorables arguments parameters)
        (process-lambda-list
         (ecclesia:parse-macro-lambda-list lambda-list)
         ;; since this is executing in the host, nothing fancy is required.
         ;; this is only used for default forms, so block-name is never provided.
         (lambda (lexpr env &key (block-name nil block-name-p))
           (declare (ignore env block-name))
           (assert (not block-name-p))
           lexpr)
         nil form nil rest :source source)
      `(let* (,@bindings
              ,@(mapcar #'list parameters arguments))
         (declare (ignorable ,@ignorables))
         ,@body))))
