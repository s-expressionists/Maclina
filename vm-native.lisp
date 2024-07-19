(defpackage #:maclina.vm-native
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:vm #:maclina.vm-shared)
                    (#:arg #:maclina.argparse))
  (:export #:initialize-vm)
  (:export #:*trace*))

(in-package #:maclina.vm-native)

(defstruct vm
  (values nil :type list)
  (stack #() :type simple-vector)
  (stack-top 0 :type (and unsigned-byte fixnum))
  (frame-pointer 0 :type (and unsigned-byte fixnum))
  (args 0 :type (and unsigned-byte fixnum))
  (arg-count 0 :type (and unsigned-byte fixnum))
  (pc 0 :type (and unsigned-byte fixnum)))

(defvar *vm*)

(declaim (type vm *vm*))

(defun bytecode-call (template env args)
  (declare (optimize speed)
           (type list args))
  (let* ((entry-pc (m:bytecode-function-entry-pc template))
         (frame-size (m:bytecode-function-locals-frame-size template))
         (module (m:bytecode-function-module template))
         (bytecode (m:bytecode-module-bytecode module))
         (literals (m:bytecode-module-literals module)))
    (declare (type (unsigned-byte 16) frame-size))
    ;; Set up the stack, then call VM.
    (let* ((vm *vm*)
           (stack (vm-stack vm)))
      (setf (vm-args vm) (vm-stack-top vm))
      ;; Pass the argments on the stack.
      (dolist (arg args)
        (setf (aref stack (vm-stack-top vm)) arg)
        (incf (vm-stack-top vm)))
      (setf (vm-arg-count vm) (length args))
      ;; Save the previous frame pointer and pc
      (let ((old-fp (vm-frame-pointer vm))
            (old-pc (vm-pc vm)))
        (setf (vm-frame-pointer vm) (vm-stack-top vm))
        (setf (vm-pc vm) entry-pc)
        (setf (vm-stack-top vm) (+ (vm-frame-pointer vm) frame-size))
        ;; set up the stack, then call vm
        (vm bytecode env literals frame-size)
        ;; tear down the frame.
        (setf (vm-stack-top vm) (- (vm-frame-pointer vm) (length args)))
        (setf (vm-frame-pointer vm) old-fp)
        (setf (vm-pc vm) old-pc))
      (values-list (vm-values vm)))))

(defun initialize-vm (stack-size)
  (setf *vm*
        (make-vm :stack (make-array stack-size)
                 :frame-pointer 0
                 :stack-top 0))
  (values))

(declaim (inline signed))
(defun signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defstruct (cell (:constructor make-cell (value))) value)

(defvar *trace* nil)

(defstruct dynenv)
(defstruct (entry-dynenv (:include dynenv)
                         (:constructor make-entry-dynenv (fun)))
  (fun (error "missing arg") :type function))

(defun instruction-trace (bytecode literals stack ip bp sp frame-size)
  (declare (ignorable stack bp sp frame-size))
  (fresh-line *trace-output*)
  (let ((*standard-output* *trace-output*))
    (maclina.machine:display-instruction bytecode literals ip))
  ;;#+(or)
  (let ((frame-end (+ bp frame-size)))
    (format *trace-output* " ; bp ~d sp ~d locals ~s stack ~s~%"
            bp sp (subseq stack bp frame-end)
            ;; We take the max for partial frames.
            (subseq stack frame-end (max sp frame-end)))))

(defun vm (bytecode closure constants frame-size)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
           (type (simple-array t (*)) closure constants)
           (type (unsigned-byte 16) frame-size)
           (optimize speed))
  (let* ((vm *vm*)
         (stack (vm-stack vm)))
    (declare (type (simple-array t (*)) stack))
    (symbol-macrolet ((ip (vm-pc vm))
                      (sp (vm-stack-top vm))
                      (bp (vm-frame-pointer vm)))
      (declare (type (and unsigned-byte fixnum) ip sp bp))
      (labels ((stack (index)
                 ;;(declare (optimize (safety 0))) ; avoid bounds check
                 (svref stack index))
               ((setf stack) (object index)
                 ;;(declare (optimize (safety 0)))
                 ;; I do not understand SBCL's complaint, so
                 #+sbcl
                 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                 (setf (svref stack index) object))
               (local (index)
                 (svref stack (+ bp index)))
               ((setf local) (object index)
                 (declare (type (unsigned-byte 16) index))
                 (setf (svref stack (+ bp index)) object))
               (spush (object)
                 (prog1 (setf (stack sp) object) (incf sp)))
               (spop () (stack (decf sp)))
               (bind (nvars base)
                 ;; Most recent push goes to the last local.
                 (loop repeat nvars
                       for bsp downfrom (+ base nvars -1)
                       do (setf (local bsp) (spop))))
               (code ()
                 ;;(declare (optimize (safety 0)))
                 (aref bytecode ip))
               (next-code ()
                 ;;(declare (optimize safety 0))
                 (aref bytecode (incf ip)))
               (next-code-signed ()
                 (signed (aref bytecode (+ ip 1)) 8))
               (next-long ()
                 (logior (next-code) (ash (next-code) 8)))
               (next-code-signed-16 ()
                 (signed (+ (aref bytecode (+ ip 1))
                            (ash (aref bytecode (+ ip 2)) 8))
                         16))
               (next-code-signed-24 ()
                 (signed (+ (aref bytecode (+ ip 1))
                            (ash (aref bytecode (+ ip 2)) 8)
                            (ash (aref bytecode (+ ip 3)) 16))
                         24))
               (constant (index)
                 ;;(declare (optimize (safety 0)))
                 (aref constants index))
               (closure (index)
                 ;;(declare (optimize (safety 0)))
                 (aref closure index))
               (gather (n)
                 (declare (type (unsigned-byte 16) n))
                 (let ((result nil)) ; put the most recent value on the end
                   (loop repeat n do (push (spop) result))
                   result))
               (call (nargs)
                 (let ((args (gather nargs)) (callee (spop)))
                   (declare (type function callee))
                   (setf (vm-stack-top vm) sp)
                   (apply callee args)))
               (call-fixed (nargs mvals)
                 (case mvals
                   ((0) (call nargs))
                   (t (mapcar #'spush (subseq (multiple-value-list (call nargs))
                                              0 mvals)))))
               (mv-call () (call (spop))))
        (declare (inline stack (setf stack) spush spop bind
                         code next-code next-long constant closure
                         call mv-call call-fixed
                         next-code-signed next-code-signed-16
                         next-code-signed-24))
        (loop with end = (length bytecode)
              with trace = *trace*
              until (eql ip end)
              when trace
                do (instruction-trace bytecode constants stack
                                      ip bp sp frame-size)
              do (case (code)
                   ((#.m:ref) (spush (local (next-code))) (incf ip))
                   ((#.m:const) (spush (constant (next-code))) (incf ip))
                   ((#.m:closure) (spush (closure (next-code))) (incf ip))
                   ((#.m:call)
                    (setf (vm-values vm)
                          (multiple-value-list (call (next-code))))
                    (incf ip))
                   ((#.m:call-receive-one)
                    (spush (call (next-code)))
                    (incf ip))
                   ((#.m:call-receive-fixed)
                    (call-fixed (next-code) (next-code))
                    (incf ip))
                   ((#.m:bind) (bind (next-code) (next-code)) (incf ip))
                   ((#.m:set) (setf (local (next-code)) (spop)) (incf ip))
                   ((#.m:make-cell) (spush (make-cell (spop))) (incf ip))
                   ((#.m:cell-ref) (spush (cell-value (spop))) (incf ip))
                   ((#.m:cell-set)
                    (setf (cell-value (spop)) (spop))
                    (incf ip))
                   ((#.m:make-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              m:*client*
                              template
                              (coerce (gather
                                       (m:bytecode-function-environment-size template))
                                      'simple-vector))))
                    (incf ip))
                   ((#.m:make-uninitialized-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              m:*client*
                              template
                              (make-array
                               (m:bytecode-function-environment-size template)))))
                    (incf ip))
                   ((#.m:initialize-closure)
                    (let ((env (m:bytecode-closure-env (local (next-code)))))
                      (declare (type simple-vector env))
                      (loop for i from (1- (length env)) downto 0 do
                        (setf (aref env i) (spop))))
                    (incf ip))
                   ((#.m:return)
                    ;; Assert that all temporaries are popped off..
                    (locally
                        ;; SBCL complains that ASSERT is inefficient.
                        #+sbcl(declare (sb-ext:muffle-conditions
                                        sb-ext:compiler-note))
                      (assert (eql sp (+ bp frame-size))))
                    (return))
                   ((#.m:jump-8) (incf ip (next-code-signed)))
                   ((#.m:jump-16) (incf ip (next-code-signed-16)))
                   ((#.m:jump-24) (incf ip (next-code-signed-24)))
                   ((#.m:jump-if-8) (incf ip (if (spop) (next-code-signed) 2)))
                   ((#.m:jump-if-16) (incf ip (if (spop) (next-code-signed-16) 3)))
                   ((#.m:jump-if-24) (incf ip (if (spop) (next-code-signed-24) 4)))
                   ((#.m:check-arg-count-<=)
                    (vm:check-arg-count-<= (vm-arg-count vm) (next-code))
                    (incf ip))
                   ((#.m:check-arg-count->=)
                    (vm:check-arg-count->= (vm-arg-count vm) (next-code))
                    (incf ip))
                   ((#.m:check-arg-count-=)
                    (vm:check-arg-count-= (vm-arg-count vm) (next-code))
                    (incf ip))
                   ((#.m:jump-if-supplied-8)
                    (let ((arg (spop)))
                      (incf ip (cond ((typep arg 'vm:unbound-marker) 2)
                                     (t (spush arg) (next-code-signed))))))
                   ((#.m:jump-if-supplied-16)
                    (let ((arg (spop)))
                      (incf ip (cond ((typep arg 'vm:unbound-marker) 3)
                                     (t (spush arg) (next-code-signed-16))))))
                   ((#.m:bind-required-args)
                    (vm:bind-required-args (next-code) stack bp (vm-args vm))
                    (incf ip))
                   ((#.m:bind-optional-args)
                    (setf sp
                          (vm:bind-optional-args (next-code) (next-code)
                                                 stack sp (vm-args vm)
                                                 (vm-arg-count vm)))
                    (incf ip))
                   ((#.m:listify-rest-args)
                    (spush
                     (vm:listify-rest-args
                      (next-code) stack (vm-args vm) (vm-arg-count vm)))
                    (incf ip))
                   ((#.m:parse-key-args)
                    (let ((nfixed (next-code)) (key-count-info (next-code))
                          (key-literal-start (next-code)))
                      (setf sp
                            (vm:parse-key-args
                             nfixed
                             (ash key-count-info -1)
                             (logbitp 0 key-count-info)
                             key-literal-start stack sp
                             (vm-arg-count vm) (vm-args vm) constants)))
                    (incf ip))
                   ((#.m:save-sp)
                    (setf (local (next-code)) sp)
                    (incf ip))
                   ((#.m:restore-sp)
                    (setf sp (local (next-code)))
                    (incf ip))
                   ((#.m:entry)
                      (tagbody
                         (setf (local (next-code))
                               (make-entry-dynenv
                                (let ((old-sp sp)
                                      (old-bp bp))
                                  (lambda ()
                                    ;; We know that this GO is never out of
                                    ;; extent, but that is hard to express.
                                    (declare (optimize (safety 0)))
                                    (setf sp old-sp
                                          bp old-bp)
                                    (go loop)))))
                         (incf ip)
                       loop
                         (vm bytecode closure constants frame-size)))
                   ((#.m:catch-8)
                    (let ((target (+ ip (next-code-signed)))
                          (tag (spop))
                          (old-sp sp)
                          (old-bp bp))
                      (incf ip 2)
                      (catch tag
                        (vm bytecode closure constants frame-size)
                        ;; since catch-close is used for local unwinds
                        ;; as well as normal exit of the catch block,
                        ;; don't jump to the end of the catch
                        ;; unless something actually threw.
                        (setf target ip))
                      (setf ip target)
                      (setf sp old-sp)
                      (setf bp old-bp)))
                   ((#.m:catch-16)
                    (let ((target (+ ip (next-code-signed-16)))
                          (tag (spop))
                          (old-sp sp)
                          (old-bp bp))
                      (incf ip 3)
                      (catch tag
                        (vm bytecode closure constants frame-size)
                        (setf target ip))
                      (setf ip target)
                      (setf sp old-sp)
                      (setf bp old-bp)))
                   ((#.m:throw) (throw (spop) (values)))
                   ((#.m:catch-close)
                    (incf ip)
                    (return))
                   ((#.m:exit-8)
                    (incf ip (next-code-signed))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:exit-16)
                    (incf ip (next-code-signed-16))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:exit-24)
                    (incf ip (next-code-signed-24))
                    (funcall (entry-dynenv-fun (spop))))
                   ((#.m:entry-close)
                    (incf ip)
                    (return))
                   ((#.m:special-bind)
                    (progv (list (constant (next-code))) (list (spop))
                      (incf ip)
                      (vm bytecode closure constants frame-size)))
                   ((#.m:symbol-value)
                    (spush (symbol-value (constant (next-code))))
                    (incf ip))
                   ((#.m:symbol-value-set)
                    (setf (symbol-value (constant (next-code))) (spop))
                    (incf ip))
                   ((#.m:progv)
                    (incf ip) ; ignore environment
                    (let ((values (spop)))
                      (progv (spop) values
                        (incf ip)
                        (vm bytecode closure constants frame-size))))
                   ((#.m:unbind)
                    ;; NOTE: used for both progv and special-bind
                    (incf ip)
                    (return))
                   ((#.m:push-values)
                    (dolist (value (vm-values vm))
                      (spush value))
                    (spush (length (vm-values vm)))
                    (incf ip))
                   ((#.m:append-values)
                    (let ((n (spop)))
                      (declare (type (and unsigned-byte fixnum) n))
                      (dolist (value (vm-values vm))
                        (spush value))
                      (spush (+ n (length (vm-values vm))))
                      (incf ip)))
                   ((#.m:pop-values)
                    (setf (vm-values vm) (gather (spop)))
                    (incf ip))
                   ((#.m:mv-call)
                    (setf (vm-values vm)
                          (multiple-value-list (mv-call)))
                    (incf ip))
                   ((#.m:mv-call-receive-one)
                    (spush (mv-call))
                    (incf ip))
                   ((#.m:mv-call-receive-fixed)
                    (let ((mvals (next-code)))
                      (case mvals
                        ((0) (mv-call))
                        (t (mapcar #'spush (subseq (multiple-value-list (mv-call))
                                                   0 mvals)))))
                    (incf ip))
                   ((#.m:fdefinition #.m:called-fdefinition)
                    (spush (fdefinition (constant (next-code)))) (incf ip))
                   ((#.m:nil) (spush nil) (incf ip))
                   ((#.m:eq) (spush (eq (spop) (spop))) (incf ip))
                   ((#.m:pop) (setf (vm-values vm) (list (spop))) (incf ip))
                   ((#.m:push) (spush (first (vm-values vm))) (incf ip))
                   ((#.m:dup)
                    (let ((v (spop))) (spush v) (spush v)) (incf ip))
                   ((#.m:fdesignator)
                    ;; we ignore the environment but still need to
                    ;; advance the IP.
                    (incf ip)
                    (let ((fdesig (spop)))
                      (spush
                       (etypecase fdesig
                         (function fdesig)
                         (symbol (fdefinition fdesig)))))
                    (incf ip))
                   ((#.m:protect)
                    (let* ((template (constant (next-code)))
                           (envsize
                             (m:bytecode-function-environment-size template))
                           (cleanup-thunk
                             (if (zerop envsize)
                                 template
                                 (m:make-bytecode-closure
                                  m:*client* template
                                  (coerce (gather envsize) 'simple-vector)))))
                      (declare (type (unsigned-byte 16) envsize)
                               (type function cleanup-thunk))
                      (incf ip)
                      (unwind-protect
                           (vm bytecode closure constants frame-size)
                        (let ((values (vm-values vm)))
                          (funcall cleanup-thunk)
                          (setf (vm-values vm) values)))))
                   ((#.m:cleanup)
                    (incf ip)
                    (return))
                   ((#.m:encell)
                    (let ((index (next-code)))
                      (setf (local index) (make-cell (local index)))))
                   ((#.m:long)
                    (case (next-code)
                      (#.m:ref (spush (local (next-long))) (incf ip))
                      (#.m:const (spush (constant (next-long))) (incf ip))
                      (#.m:closure (spush (closure (next-long))) (incf ip))
                      (#.m:call
                       (setf (vm-values vm)
                             (multiple-value-list (call (next-long))))
                       (incf ip))
                      (#.m:call-receive-one (spush (call (next-long))) (incf ip))
                      (#.m:call-receive-fixed
                       (call-fixed (next-long) (next-long)) (incf ip))
                      (#.m:bind (bind (next-long) (next-long)) (incf ip))
                      (#.m:set (setf (local (next-long)) (spop)) (incf ip))
                      (#.m:bind-required-args
                       (vm:bind-required-args (next-long)
                                              stack bp (vm-args vm))
                       (incf ip))
                      (#.m:bind-optional-args
                       (setf sp
                             (vm:bind-optional-args
                              (next-long) (next-long)
                              stack sp (vm-args vm) (vm-arg-count vm)))
                       (incf ip))
                      (#.m:listify-rest-args
                       (spush
                        (vm:listify-rest-args
                         (next-long) stack (vm-args vm) (vm-arg-count vm)))
                       (incf ip))
                      (#.m:parse-key-args
                       (let ((nfixed (next-long)) (key-count-info (next-long))
                             (key-literal-start (next-long)))
                         (setf sp (vm:parse-key-args
                                   nfixed
                                   (ash key-count-info -1)
                                   (logbitp 0 key-count-info)
                                   key-literal-start stack sp
                                   (vm-arg-count vm) (vm-args vm) constants)))
                       (incf ip))
                      (#.m:check-arg-count-<=
                       (vm:check-arg-count-<= (vm-arg-count vm) (next-long))
                       (incf ip))
                      (#.m:check-arg-count->=
                       (vm:check-arg-count->= (vm-arg-count vm) (next-long))
                       (incf ip))
                      (#.m:check-arg-count-=
                       (vm:check-arg-count-= (vm-arg-count vm) (next-long))
                       (incf ip))
                      (otherwise
                       (error "Unknown long opcode #x~x" (code)))))
                   (otherwise
                    (error "Unknown opcode #x~x" (code)))))))))

(defmethod m:compute-instance-function ((client trucler-native:client)
                                        (closure m:bytecode-closure))
  (let ((template (m:bytecode-closure-template closure))
        (env (m:bytecode-closure-env closure)))
    (lambda (&rest args)
      (bytecode-call template env args))))

(defmethod m:compute-instance-function ((client trucler-native:client)
                                        (closure m:bytecode-function))
  (lambda (&rest args)
    (bytecode-call closure #() args)))

(defmethod m:symbol-value ((client trucler-native:client) env symbol)
  (declare (ignore env))
  (symbol-value symbol))
(defmethod (setf m:symbol-value) (new (client trucler-native:client) env symbol)
  (declare (ignore env))
  (setf (symbol-value symbol) new))
(defmethod m:boundp ((client trucler-native:client) env symbol)
  (declare (ignore env))
  (boundp symbol))
(defmethod m:makunbound ((client trucler-native:client) env symbol)
  (declare (ignore env))
  (makunbound symbol))

(defmethod m:call-with-progv ((client trucler-native:client)
			      env symbols values thunk)
  (declare (ignore env))
  (progv symbols values (funcall thunk)))

(defmethod m:fboundp ((client trucler-native:client) env name)
  (declare (ignore env))
  (fboundp name))
(defmethod m:fdefinition ((client trucler-native:client) env name)
  (declare (ignore env))
  (fdefinition name))
(defmethod (setf m:fdefinition) (new (client trucler-native:client) env name)
  (declare (ignore env))
  (setf (fdefinition name) new))
(defmethod m:fmakunbound ((client trucler-native:client) env name)
  (declare (ignore env))
  (fmakunbound name))

(defmethod m:multiple-values-limit ((client trucler-native:client))
  ;; we use host values, therefore
  multiple-values-limit)
