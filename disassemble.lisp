(in-package #:maclina.machine)

;;; Return a list of all IPs that are jumped to.
(defun gather-labels (bytecode ip end)
  (let ((result ()))
    (do-instructions (mnemonic ip longp args :start ip :end end) bytecode
      (declare (ignore mnemonic ip longp))
      (loop for (type n) in args
            when (cl:eq type :label)
              do (cl:push n result)))
    (sort result #'<)))

(defun %display-instruction (name longp args textify-operand)
  (if (string= name "PARSE-KEY-ARGS")
      ;; We special case this despite the keys-arg thing because it's
      ;; just pretty weird all around.
      (let* ((more-start (second (first args)))
             (kci (second (second args)))
             (aokp (logbitp 0 kci))
             (key-count (ash kci -1))
             (keys (third args))
             (framestart (second (fourth args))))
        ;; Print
        (format t "~&  ~:[~;long ~]~(~a~)~:[~;-aok~] ~d ~d ~a ~d"
                longp name aokp more-start key-count
                (funcall textify-operand keys key-count) framestart))
      ;; Normal case
      (format t "~&  ~:[~;long ~]~(~a~)~{ ~a~}"
              longp name (mapcar textify-operand args))))

(defun operand-textifier (literals)
  (flet ((textify-operand (thing &optional key-count)
           (destructuring-bind (kind value) thing
             (cond ((cl:eq kind :constant)
                    (format () "'~s" (aref literals value)))
                   ((cl:eq kind :label) (format () "L~a" value))
                   ((cl:eq kind :operand) (format () "~d" value))
                   ((cl:eq kind :keys)
                    (let ((keys cl:nil) (keystart value))
                      (do ((i 0 (1+ i)))
                          ((= i key-count) (setq keys (nreverse keys)))
                        (cl:push (aref literals (+ keystart i)) keys))
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
          (cl:push (write-to-string labelpos) result)))
      ;; Record the instruction. Resolve labels to an ID.
      (cl:push (list* mnemonic longp
                      (loop for arg in args
                            for (type n) = arg
                            collect (if (cl:eq type :label)
                                        (let ((lpos (position n labels)))
                                          (assert lpos)
                                          (list :label lpos))
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

(defmethod disassemble ((object bytecode-module))
  (disassemble-bytecode (bytecode-module-bytecode object)
                        (bytecode-module-literals object)))

;; TODO: Record function boundaries, so that among other things we can
;; disassemble only the region for the function being disassembled.
(defmethod disassemble ((object bytecode-function))
  (let ((module (bytecode-function-module object))
        (entry-pc (bytecode-function-entry-pc object)))
    (disassemble-bytecode (bytecode-module-bytecode module)
                          (bytecode-module-literals module)
                          :start entry-pc
                          :end (+ entry-pc (bytecode-function-size object)))))

(defmethod disassemble ((object bytecode-closure))
  (disassemble (bytecode-closure-template object)))
