(in-package #:maclina.introspect)

;;; Return a list of all IPs that are jumped to.
(defun gather-labels (bytecode ip end)
  (let ((result ()))
    (do-instructions (mnemonic ip longp args :start ip :end end) bytecode
      (declare (ignore mnemonic ip longp))
      (loop for (type . n) in args
            when (eq type :label)
              do (push n result)))
    (sort result #'<)))

(defun %display-instruction (name longp args textify-operand)
  (if (string= name "PARSE-KEY-ARGS")
      ;; We special case this despite the keys-arg thing because it's
      ;; just pretty weird all around.
      (let* ((more-start (cdr (first args)))
             (kci (cdr (second args)))
             (aokp (logbitp 0 kci))
             (key-count (ash kci -1))
             (keystart (third args)))
        ;; Print
        (format t "~&  ~:[~;long ~]~(~a~)~:[~;-aok~] ~d ~d ~a"
                longp name aokp more-start key-count
                (funcall textify-operand keystart key-count)))
      ;; Normal case
      (format t "~&  ~:[~;long ~]~(~a~)~{ ~a~}"
              longp name (mapcar textify-operand args))))

(defun operand-textifier (literals)
  (flet ((textify-operand (thing &optional key-count)
           (destructuring-bind (kind . value) thing
             (cond ((eq kind :constant)
                    (format () "'~s" (aref literals value)))
                   ((eq kind :label) (format () "L~a" value))
                   ((eq kind :operand) (format () "~d" value))
                   ((eq kind :keys)
                    (let ((keys ()) (keystart value))
                      (do ((i 0 (1+ i)))
                          ((= i key-count) (setq keys (nreverse keys)))
                        (push (aref literals (+ keystart i)) keys))
                      (format () "'~s" keys)))
                   (t (error "Illegal kind ~a" kind))))))
    #'textify-operand))

;;; Used externally by tracers.
(defun display-instruction (bytecode literals ip)
  (destructuring-bind (name longp . args)
      (disassemble-instruction bytecode ip)
    (%display-instruction name longp args (operand-textifier literals))))

(defun %disassemble-bytecode (bytecode start end)
  (let* ((labels (gather-labels bytecode start end))
         (result ()))
    (do-instructions (mnemonic ip longp args :start start :end end) bytecode
      ;; collect a label if this is a destination.
      (let ((labelpos (position ip labels)))
        (when labelpos
          (push (write-to-string labelpos) result)))
      ;; Record the instruction. Resolve labels to an ID.
      (push (list* mnemonic longp
                   (loop for arg in args
                         for (type . n) = arg
                         collect (if (eq type :label)
                                     (let ((lpos (position n labels)))
                                       (assert lpos)
                                       (cons :label lpos))
                                     arg)))
            result))
    (nreverse result)))

(defun disassemble-bytecode (bytecode literals
                             &key (start 0) (end (length bytecode)))
  (let ((dis (%disassemble-bytecode bytecode start end))
        (textify-operand (operand-textifier literals)))
    (format t "~&---module---~%")
    (dolist (item dis)
      (cond
        ((consp item)
         ;; instruction
         (destructuring-bind (name longp . args) item
           (%display-instruction name longp args textify-operand)))
        ((or (stringp item) (symbolp item))
         ;; label
         (format t "~&L~a:~%" item))
        (t (error "Illegal item ~a" item)))))
  (values))

(defgeneric disassemble (object))

(defmethod disassemble ((object m:module))
  (disassemble-bytecode (m:bytecode object) (m:literals object)))

(defmethod disassemble ((object m:function))
  (let ((module (m:module object))
        (entry-pc (m:entry-pc object)))
    (disassemble-bytecode (m:bytecode module)
                          (m:literals module)
                          :start entry-pc
                          :end (+ entry-pc (m:size object)))))

(defmethod disassemble ((object m:closure))
  (disassemble (m:template object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structured disassembly
;;;

(defclass sdisassemble-context ()
  (;; Nesting depth, for printing spaces on each line
   (%depth :accessor depth :initform 0)
   ;; An alist (pc type name)
   ;; where type is either GO or RETURN-FROM and name is the name of the label.
   (%label-alist :accessor label-alist :initform ())))

(defun find-label (pc context) (rest (assoc pc (label-alist context))))
(defun push-label (pc type name context)
  (push (list pc type name) (label-alist context)))

(defgeneric sdisassemble-start (annotation context)
  (:method (a c) (declare (ignore a c))))
(defgeneric sdisassemble-end (annotation context)
  (:method (a c) (declare (ignore a c))))

(defgeneric sdisassemble-comment (mnemonic context &rest args)
  (:method (m c &rest a) (declare (ignore m c a)) nil))

(defun sdisassemble-instruction (mnemonic context &rest args)
  (format t "~vt~(~a~)~{ ~s~}~@[ ; ~a~]~%" (depth context) mnemonic args
          (apply #'sdisassemble-comment mnemonic context args)))

(defun jump-comment (context args)
  (destructuring-bind (dest) args
    (let ((label (find-label dest context)))
      (if label
          (format nil "~a ~a" (first label) (second label))
          nil))))
(defmethod sdisassemble-comment ((mnemonic (eql 'm:jump-8)) context &rest args)
  (jump-comment context args))
(defmethod sdisassemble-comment ((mnemonic (eql 'm:jump-16)) context &rest args)
  (jump-comment context args))
(defmethod sdisassemble-comment ((mnemonic (eql 'm:jump-24)) context &rest args)
  (jump-comment context args))

(defmethod sdisassemble-start ((annot m:function) context)
  (format t "function ~a~%" (m:name annot))
  (setf (depth context) 1))
(defmethod sdisassemble-end ((annot m:function) context)
  (setf (depth context) 0))

(defmethod sdisassemble-start ((annot m:block-info) context)
  (format t "~vt(BLOCK ~a~%" (depth context) (m:name annot))
  (push-label (m:end annot) 'return-from (m:name annot) context)
  (incf (depth context)))
(defmethod sdisassemble-end ((annot m:block-info) context)
  (decf (depth context))
  (format t "~vt)~%" (depth context)))

(defmethod sdisassemble-start ((annot m:tagbody-info) context)
  (format t "~vt(TAGBODY~%" (depth context))
  (loop with end = (m:end annot)
        for (name . ip) in (m:tags annot)
        do (push-label ip 'go name context)
        unless (= ip end) ; handled specially in the other method
          do (delay ip name))
  (incf (depth context) 2))
(defmethod sdisassemble-end ((annot m:tagbody-info) context)
  (let ((endtag (rassoc (m:end annot) (m:tags annot))))
    ;; print the end tag now rather than with the delay,
    ;; so that it can be printed within the tagbody context
    (format t "~vt~a:~%" (1- (depth context)) (car endtag)))
  (decf (depth context) 2)
  (format t "~vt) ; TAGBODY~%" (depth context)))

(defmethod sdisassemble-start ((annot m:vars-info) context)
  (format t "~vt(LET (~:{(~a #~d)~^ ~})~%" (depth context)
          (loop for var in (m:bindings annot)
                collect (list (m:name var) (m:index var))))
  (incf (depth context) 2))
(defmethod sdisassemble-end ((annot m:vars-info) context)
  (decf (depth context) 2)
  (format t "~vt) ; LET~%" (depth context)))

(defun sdisassemble-delayed (label context)
  ;; Print the label.
  (format t "~vt~a:~%" (1- (depth context)) label))

(defun %sdisassemble (bytecode literals annotations
                      &key (start 0) (end (length bytecode)))
  (format t "~&---module---~%")
  (map-annotated-instructions-literals
   #'sdisassemble-instruction
   #'sdisassemble-start #'sdisassemble-end
   #'sdisassemble-delayed
   bytecode literals annotations
   :start start :end end :context (make-instance 'sdisassemble-context)))

(defgeneric sdisassemble (object))
(defmethod sdisassemble ((mod m:module))
  (%sdisassemble (m:bytecode mod) (m:literals mod) (m:pc-map mod)))

(defmethod sdisassemble ((object m:function))
  (let ((module (m:module object))
        (entry-pc (m:entry-pc object)))
    (%sdisassemble (m:bytecode module) (m:literals module) (m:pc-map module)
                   :start entry-pc
                   :end (+ entry-pc (m:size object)))))

(defmethod sdisassemble ((object m:closure))
  (sdisassemble (m:template object)))
