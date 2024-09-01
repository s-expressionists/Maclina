(defpackage #:maclina.vm-cross
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:vm #:maclina.vm-shared)
                    (#:arg #:maclina.argparse)
                    (#:cmp #:maclina.compile))
  (:export #:client)
  (:export #:initialize-vm)
  (:export #:*trace*)
  (:export #:make-variable-access-closures)
  (:export #:with-timeout #:timeout))

(in-package #:maclina.vm-cross)

(defclass client () ())

(defmethod cmp:run-time-environment ((client client) (env clostrum:compilation-environment))
  (loop while (typep env 'clostrum:compilation-environment)
        do (setf env (clostrum:parent client env))
        finally (return env)))
(defmethod cmp:run-time-environment ((client client) (env clostrum:run-time-environment))
  env)

(defmethod m:link-function ((client client) env fname)
  (clostrum:ensure-operator-cell client env fname))

(defmethod m:link-variable ((client client) env name)
  (cons name (clostrum:ensure-variable-cell client env name)))

(defstruct vm
  (values nil :type list)
  (stack #() :type simple-vector)
  (stack-top 0 :type (and unsigned-byte fixnum))
  (frame-pointer 0 :type (and unsigned-byte fixnum))
  (args 0 :type (and unsigned-byte fixnum))
  (arg-count 0 :type (and unsigned-byte fixnum))
  (pc 0 :type (and unsigned-byte fixnum))
  (dynenv-stack nil :type list)
  (client (error "missing arg")))

(defvar *vm*)
(declaim (type vm *vm*))

(defvar *trace* nil)

(defstruct (cell (:constructor make-cell (value))) value)
(defvar *unbound* (vm:make-unbound-marker))

(defvar *timeout* nil)
(declaim (type (or null (and fixnum unsigned-byte)) *timeout*))
(defvar *odometer* nil)
(declaim (type (or null (and fixnum unsigned-byte)) *odometer*))

(defmacro with-timeout ((n) &body body)
  `(let ((*timeout* ,n) (*odometer* 0)) ,@body))

(define-condition timeout (error)
  ((%timeout :initarg :timeout :reader timeout))
  (:report (lambda (condition stream)
             (format stream "VM timeout at ~d steps" (timeout condition)))))

(defstruct dynenv)
(defstruct (entry-dynenv (:include dynenv)
                         (:constructor make-entry-dynenv (tag)))
  (tag (error "missing arg")))
(defstruct (sbind-dynenv (:include dynenv)
                         (:constructor %make-sbind-dynenv
                             (global-cell cell)))
  ;; global-cell is the symbol's global Clostrum value cell,
  ;; whereas cell is local a local binding cell.
  ;; We bind etc. using global cells as keys, rather than symbol
  ;; names, so that the same symbol can have distinct local bindings
  ;; in distinct global environments.
  global-cell cell)
(defstruct (progv-dynenv (:include dynenv)
                         (:constructor %make-progv-dynenv (mapping)))
  ;; Alist from global cells to local cells.
  mapping)
(defstruct (catch-dynenv (:include dynenv)
                         (:constructor make-catch-dynenv
                             (tag dest-tag dest)))
  ;; the actual catch tag
  (tag (error "missing arg"))
  ;; the catch tag established by bytecode-vm, representing the
  ;; frame to return to
  (dest-tag (error "missing arg"))
  ;; the new IP to jump to
  (dest (error "missing arg")))
;;; unwind-protect
(defstruct (protection-dynenv (:include dynenv)
                              (:constructor make-protection-dynenv
                                  (cleanup)))
  (cleanup (error "missing arg") :type function))

;;; For uniformity, we put a Clostrum-style cell into these structs.
(defun make-sbind-dynenv (global-cell value)
  (%make-sbind-dynenv global-cell (cons value *unbound*)))
(defun make-progv-dynenv (global-cells values)
  ;; Per CLHS:
  ;; If we have too few values, the remaining symbols are unbound.
  ;; If we have too many, the excess are ignored.
  (loop for global-cell in global-cells
        for value = (if (null values) *unbound* (pop values))
        for cell = (cons value *unbound*)
        collect (cons global-cell cell) into mapping
        finally (return (%make-progv-dynenv mapping))))

(defun %progv (client env varnames values)
  (let* ((global-cells
           (loop for symbol in varnames
                                   collect (clostrum:ensure-variable-cell
                                            client env symbol)))
         (de (make-progv-dynenv global-cells values)))
    (push de (vm-dynenv-stack *vm*))))

(defun bytecode-call (template closure-env args)
  (declare (optimize speed)
           (type list args))
  (let ((entry-pc (m:bytecode-function-entry-pc template))
        (frame-size (m:bytecode-function-locals-frame-size template))
        (module (m:bytecode-function-module template)))
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
            (old-pc (vm-pc vm))
            (old-de-stack (vm-dynenv-stack vm)))
        (setf (vm-frame-pointer vm) (vm-stack-top vm))
        (setf (vm-pc vm) entry-pc)
        (setf (vm-stack-top vm) (+ (vm-frame-pointer vm) frame-size))
        ;; set up the stack, then call vm
        (unwind-protect
             (vm module closure-env frame-size)
          (setf (vm-dynenv-stack vm) old-de-stack))
        ;; tear down the frame.
        (setf (vm-stack-top vm) (- (vm-frame-pointer vm) (length args)))
        (setf (vm-frame-pointer vm) old-fp)
        (setf (vm-pc vm) old-pc))
      (values-list (vm-values vm)))))

(defun initialize-vm (stack-size &optional (client m:*client*))
  (setf *vm*
        (make-vm :stack (make-array stack-size)
                 :frame-pointer 0
                 :stack-top 0
                 :client client))
  (values))

(declaim (inline signed))
(defun signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun symbol-cell (global-cell)
  (loop for de in (vm-dynenv-stack *vm*)
        do (typecase de
             (sbind-dynenv
              (when (eq global-cell (sbind-dynenv-global-cell de))
                (return (sbind-dynenv-cell de))))
             (progv-dynenv
              (let ((pair (assoc global-cell
                                 (progv-dynenv-mapping de))))
                (when pair
                  (return (cdr pair))))))
        finally (return global-cell)))

(defun %symbol-value (symbol global-cell)
  (let* ((cell (symbol-cell global-cell))
         (value (car cell)))
    (if (eq value (cdr cell))
        (error 'unbound-variable :name symbol)
        value)))

(defun (setf %symbol-value) (new symbol global-cell)
  (declare (ignore symbol))
  (let ((cell (symbol-cell global-cell)))
    (setf (car cell) new)))

(defun %boundp (symbol global-cell)
  (declare (ignore symbol))
  (let ((cell (symbol-cell global-cell)))
    (not (eq (car cell) (cdr cell)))))

(defun %makunbound (symbol global-cell)
  (let ((cell (symbol-cell global-cell)))
    (setf (car cell) (cdr cell)))
  symbol)

;;; Unwind to the VM frame represented by rtag at ip new-ip,
;;; set the de stack to the given de stack, and execute cleanups
;;; along the way.
(defun unwind-to (vm rtag new-ip new-de-stack)
  ;; Pop off dynenvs until we reach the destination.
  ;; Note that we have to actually pop the de-stack rather than
  ;; use a local variable or whatever, so that any cleanup thunks
  ;; are executed in the correct dynamic environment.
  ;; Also note that per CLHS 5.2 point 1, it is illegal for a cleanup
  ;; to escape to a point between it and the ultimate destination -
  ;; here, that would be some entry or catch between the de-stack and
  ;; the new-de-stack. But we don't have to go through the extra
  ;; effort of enforcing this by signaling an error, so we don't.
  ;; This is like the failed X3J13 EXIT-EXTENT:MEDIUM.
  ;; If we did want to signal an error, the obvious procedure would
  ;; be to go through and mark any intervening exits invalid by
  ;; setting some slot in them, and then checking that slot when
  ;; initiating a nonlocal exit.
  ;; (Simply changing the de-stack to new-de-stack would not work
  ;;  because then e.g. all special bindings would be undone.)
  (loop until (eq (vm-dynenv-stack vm) new-de-stack)
        do (let ((de (pop (vm-dynenv-stack vm))))
             (typecase de
               (protection-dynenv
                ;; Preserve values
                (let ((values (vm-values vm)))
                  (funcall (protection-dynenv-cleanup de))
                  (setf (vm-values vm) values))))))
  (throw rtag new-ip))

(define-condition out-of-extent-unwind (control-error)
  ())

(defun exit-to (vm entry-dynenv new-ip)
  ;; Make sure the entry is still on the DE stack.
  ;; If it is, reset the DE stack, and throw.
  ;; Otherwise complain.
  (let ((old-de-stack (member entry-dynenv (vm-dynenv-stack vm))))
    (if (null old-de-stack)
        (error 'out-of-extent-unwind)
        (unwind-to vm (entry-dynenv-tag entry-dynenv) new-ip
                   old-de-stack))))

(define-condition no-catch-tag (control-error)
  ((%tag :initarg :tag :reader tag)))

(defun throw-to (vm tag)
  (let ((catch-de-stack
          (member-if (lambda (de)
                       (and (catch-dynenv-p de)
                            (eq (catch-dynenv-tag de) tag)))
                     (vm-dynenv-stack vm))))
    (if (null catch-de-stack)
        (error 'no-catch-tag :tag tag)
        (let* ((de (first catch-de-stack))
               (rtag (catch-dynenv-dest-tag de))
               (dest (catch-dynenv-dest de)))
          (unwind-to vm rtag dest (rest catch-de-stack))))))

(defun instruction-trace (bytecode literals stack ip bp sp frame-size)
  (fresh-line *trace-output*)
  (let ((*standard-output* *trace-output*))
    (maclina.machine:display-instruction bytecode literals ip))
  (let ((frame-end (+ bp frame-size)))
    (format *trace-output* " ; bp ~d sp ~d locals ~s stack ~s~%"
            bp sp (subseq stack bp frame-end)
            ;; We take the max for partial frames.
            (subseq stack frame-end (max sp frame-end)))))

(defun vm (module closure frame-size)
  (declare (type (simple-array t (*)) closure)
           (type (unsigned-byte 16) frame-size)
           (optimize debug))
  (let* ((bytecode (m:bytecode-module-bytecode module))
         (constants (m:bytecode-module-literals module))
         (vm *vm*)
         (stack (vm-stack vm))
         (ip (vm-pc vm))
         (sp (vm-stack-top vm))
         (bp (vm-frame-pointer vm))
         (timeout *timeout*))
    (declare (type (simple-array (unsigned-byte 8) (*)) bytecode)
             (type (simple-array t (*)) constants stack)
             (type (and unsigned-byte fixnum) ip sp bp))
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
      (declare (inline stack (setf stack) local (setf local) spush spop bind
                       code next-code next-long constant closure
                       call mv-call call-fixed
                       next-code-signed next-code-signed-16
                       next-code-signed-24))
      (prog ((end (length bytecode))
             (trace *trace*)
             ;; KLUDGE: we can't use bp directly since catch uses eq.
             (tag (list bp)))
       loop
         (when (>= ip end)
           (error "Invalid bytecode: Reached end"))
         (when timeout
           (when (> (incf *odometer*) timeout)
             (error 'timeout :timeout timeout)))
         (when trace
           (instruction-trace bytecode constants stack ip bp sp frame-size))
         ;; The catch is for NLX. Without NLX, a (go loop) at the
         ;; bottom skips back up to the loop without setting IP.
         ;; When something NLXs to this frame, we throw the new IP
         ;; to the tag, set the IP, and then jump up to the loop.
         ;; We use CATCH instead of BLOCK on the theory that BLOCK
         ;; will have to allocate each loop, but well, I suspect
         ;; CATCH will too generally.
         (setf ip
               (catch tag
                 (case (code)
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
                    (call-fixed (next-code) (next-code)) (incf ip))
                   ((#.m:bind) (bind (next-code) (next-code)) (incf ip))
                   ((#.m:set)
                    (setf (local (next-code)) (spop))
                    (incf ip))
                   ((#.m:make-cell) (spush (make-cell (spop))) (incf ip))
                   ((#.m:cell-ref) (spush (cell-value (spop))) (incf ip))
                   ((#.m:cell-set)
                    (setf (cell-value (spop)) (spop))
                    (incf ip))
                   ((#.m:make-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              (vm-client vm)
                              template
                              (coerce (gather
                                       (m:bytecode-function-environment-size template))
                                      'simple-vector))))
                    (incf ip))
                   ((#.m:make-uninitialized-closure)
                    (spush (let ((template (constant (next-code))))
                             (m:make-bytecode-closure
                              (vm-client vm)
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
                      (incf ip
                            (cond ((typep arg 'vm:unbound-marker) 2)
                                  (t (spush arg) (next-code-signed))))))
                   ((#.m:jump-if-supplied-16)
                    (let ((arg (spop)))
                      (incf ip
                            (cond ((typep arg 'vm:unbound-marker) 3)
                                  (t (spush arg) (next-code-signed-16))))))
                   ((#.m:bind-required-args)
                    (vm:bind-required-args (next-code) stack bp (vm-args vm))
                    (incf ip))
                   ((#.m:bind-optional-args)
                    (setf sp (vm:bind-optional-args (next-code) (next-code)
                                                    stack sp
                                                    (vm-args vm)
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
                    (let ((de (make-entry-dynenv tag)))
                      (push de (vm-dynenv-stack vm))
                      (setf (local (next-code)) de)
                      (incf ip)))
                   ((#.m:catch-8)
                    (let* ((target (+ ip (next-code-signed)))
                           (dest-tag tag)
                           (tag (spop))
                           (de (make-catch-dynenv tag dest-tag target)))
                      (declare (type (and unsigned-byte fixnum) target))
                      (push de (vm-dynenv-stack vm))
                      (incf ip 2)))
                   ((#.m:catch-16)
                    (let* ((target (+ ip (next-code-signed-16)))
                           (dest-tag tag)
                           (tag (spop))
                           (de (make-catch-dynenv tag dest-tag target)))
                      (declare (type (and unsigned-byte fixnum) target))
                      (push de (vm-dynenv-stack vm))
                      (incf ip 3)))
                   ((#.m:throw) (throw-to vm (spop)))
                   ((#.m:catch-close)
                    (pop (vm-dynenv-stack vm))
                    (incf ip))
                   ((#.m:exit-8)
                    (incf ip (next-code-signed))
                    (exit-to vm (spop) ip))
                   ((#.m:exit-16)
                    (incf ip (next-code-signed-16))
                    (exit-to vm (spop) ip))
                   ((#.m:exit-24)
                    (incf ip (next-code-signed-24))
                    (exit-to vm (spop) ip))
                   ((#.m:entry-close)
                    (pop (vm-dynenv-stack vm))
                    (incf ip))
                   ((#.m:special-bind)
                    (let ((de (make-sbind-dynenv
                               (cdr (constant (next-code))) (spop))))
                      (push de (vm-dynenv-stack vm)))
                    (incf ip))
                   ((#.m:symbol-value)
                    (let ((vcell (constant (next-code))))
                      (spush (%symbol-value (car vcell) (cdr vcell))))
                    (incf ip))
                   ((#.m:symbol-value-set)
                    (let ((vcell (constant (next-code))))
                      (setf (%symbol-value (car vcell) (cdr vcell))
                            (spop)))
                    (incf ip))
                   ((#.m:progv)
                    (let* ((env (constant (next-code)))
                           (values (spop)) (varnames (spop)))
		      (%progv (vm-client vm) env varnames values))
                    (incf ip))
                   ((#.m:unbind)
                    ;; NOTE: used for both special-bind and progv
                    (pop (vm-dynenv-stack vm))
                    (incf ip))
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
                    (spush (car (constant (next-code)))) (incf ip))
                   ((#.m:nil) (spush nil) (incf ip))
                   ((#.m:eq) (spush (eq (spop) (spop))) (incf ip))
                   ((#.m:pop) (setf (vm-values vm) (list (spop))) (incf ip))
                   ((#.m:push) (spush (first (vm-values vm))) (incf ip))
                   ((#.m:dup)
                    (let ((v (spop))) (spush v) (spush v)) (incf ip))
                   ((#.m:fdesignator)
                    (let ((desig (spop)))
                      (spush
                       (etypecase desig
                         ;; have to advance the IP for the env
                         ;; when we don't use it.
                         (function (incf ip) desig)
                         (symbol
                          (clostrum:fdefinition
                           (vm-client vm) (constant (next-code))
                           desig)))))
                    (incf ip))
                   ((#.m:protect)
                    (let* ((template (constant (next-code)))
                           (envsize
                             (m:bytecode-function-environment-size template))
                           (cleanup-thunk
                             (m:make-bytecode-closure
                              (vm-client vm) template
                              (coerce (gather envsize) 'simple-vector)))
                           (de (make-protection-dynenv cleanup-thunk)))
                      (push de (vm-dynenv-stack vm)))
                    (incf ip))
                   ((#.m:cleanup)
                    (let ((de (pop (vm-dynenv-stack vm)))
                          ;; Preserve values,
                          ;; in case the thunk messes with them.
                          (values (vm-values vm)))
                      (setf (vm-stack-top vm) sp)
                      (funcall (protection-dynenv-cleanup de))
                      (setf (vm-values vm) values))
                    (incf ip))
                   ((#.m:encell)
                    (let ((index (next-code)))
                      (setf (local index) (make-cell (local index))))
                    (incf ip))
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
                       (setf sp (vm:bind-optional-args
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
                         (setf sp
                               (vm:parse-key-args
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
                    (error "Unknown opcode #x~x" (code))))
                 (go loop)))
         (go loop)))))

(defmethod m:compute-instance-function ((client client)
                                        (closure m:bytecode-closure))
  (let ((template (m:bytecode-closure-template closure))
        (env (m:bytecode-closure-env closure)))
    (lambda (&rest args)
      (bytecode-call template env args))))

(defmethod m:compute-instance-function ((client client)
                                        (fun m:bytecode-function))
  (lambda (&rest args)
    (bytecode-call fun #() args)))

;;; Given a client and environment, return closures that implement,
;;; respectively, CL:SYMBOL-VALUE, (SETF CL:SYMBOL-VALUE),
;;; CL:BOUNDP, and CL:MAKUNBOUND.
(defun make-variable-access-closures (client environment)
  (labels ((cell (symbol)
             (clostrum:ensure-variable-cell client environment symbol))
           (#1=#:symbol-value (symbol)
             (%symbol-value symbol (cell symbol)))
           ((setf #1#) (value symbol)
             (setf (%symbol-value symbol (cell symbol)) value))
           (#2=#:boundp (symbol)
             (%boundp symbol (cell symbol)))
           (#3=#:makunbound (symbol)
             (%makunbound symbol (cell symbol))))
    (values #'#1# #'(setf #1#) #'#2# #'#3#)))

(defmethod m:symbol-value ((client client) env symbol)
  (let ((cell (clostrum:ensure-variable-cell client env symbol)))
    (%symbol-value symbol cell)))
(defmethod (setf m:symbol-value) (new (client client) env symbol)
  (let ((cell (clostrum:ensure-variable-cell client env symbol)))
    (setf (%symbol-value symbol cell) new)))
(defmethod m:boundp ((client client) env symbol)
  (%boundp symbol (clostrum:ensure-variable-cell client env symbol)))
(defmethod m:makunbound ((client client) env symbol)
  (%makunbound symbol (clostrum:ensure-variable-cell client env symbol)))

(defmethod m:call-with-progv ((client client) env symbols values thunk)
  (%progv client env symbols values)
  (unwind-protect (funcall thunk)
    (pop (vm-dynenv-stack *vm*))))

(defmethod m:fboundp ((client client) env name)
  (clostrum:fboundp client env name))
(defmethod m:fdefinition ((client client) env name)
  (clostrum:fdefinition client env name))
(defmethod (setf m:fdefinition) (new (client client) env name)
  (setf (clostrum:fdefinition client env name) new))
(defmethod m:fmakunbound ((client client) env name)
  (clostrum:fmakunbound client env name))

(defmethod m:multiple-values-limit ((client client))
  ;; we use host values, therefore
  multiple-values-limit)
