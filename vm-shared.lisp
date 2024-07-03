(defpackage #:maclina.vm-shared
  (:use #:cl)
  (:local-nicknames (#:arg #:maclina.argparse))
  (:export #:unbound-marker #:make-unbound-marker)
  (:export #:check-arg-count-<= #:check-arg-count->= #:check-arg-count-=)
  (:export #:bind-required-args #:bind-optional-args
           #:listify-rest-args #:parse-key-args))

(in-package #:maclina.vm-shared)

(defstruct (unbound-marker (:constructor make-unbound-marker ())))

(declaim (inline stack (setf stack)))
(defun stack (stack index)
  (declare (type simple-vector stack) (type (and unsigned-byte fixnum) index))
  (svref stack index))
(defun (setf stack) (new stack index)
  (declare (type simple-vector stack) (type (and unsigned-byte fixnum) index))
  (setf (svref stack index) new))

(declaim (inline local (setf local)))
(defun local (stack bp index)
  (declare (type simple-vector stack)
           (type (and unsigned-byte fixnum) bp index))
  (svref stack (+ bp index)))
(defun (setf local) (new stack bp index)
  (declare (type simple-vector stack)
           (type (and unsigned-byte fixnum) bp index))
  (setf (svref stack (+ bp index)) new))

(defun constant (constants index)
  (declare (type simple-vector constants) (type (unsigned-byte 16) index))
  (svref constants index))

(declaim (inline check-arg-count-<= check-arg-count->= check-arg-count-=))
(defun check-arg-count-<= (nargs expected)
  (declare (type (unsigned-byte 16) nargs expected))
  (unless (<= nargs expected)
    (error 'arg:wrong-number-of-arguments
           :given-nargs nargs :max-nargs expected)))
(defun check-arg-count->= (nargs expected)
  (declare (type (unsigned-byte 16) nargs expected))
  (unless (>= nargs expected)
    (error 'arg:wrong-number-of-arguments
           :given-nargs nargs :min-nargs expected)))
(defun check-arg-count-= (nargs expected)
  (declare (type (unsigned-byte 16) nargs expected))
  (unless (= nargs expected)
    (error 'arg:wrong-number-of-arguments
           :given-nargs nargs :min-nargs expected :max-nargs expected)))

(declaim (inline bind-required-args))
(defun bind-required-args (nreq stack bp argsi)
  ;; Use memcpy for this.
  (let ((args-end (+ argsi nreq)))
    (do ((arg-index argsi (1+ arg-index))
         (frame-slot 0 (1+ frame-slot)))
        ((>= arg-index args-end))
      (setf (local stack bp frame-slot) (stack stack arg-index)))))

(declaim (inline bind-optional-args))
(defun bind-optional-args (required-count optional-count stack bp argsi nargs)
  (let* ((optional-start (+ argsi required-count))
         (args-end (+ argsi nargs))
         (end (+ optional-start optional-count))
         (optional-frame-offset required-count)
         (optional-frame-end (+ optional-frame-offset optional-count)))
    (if (<= args-end end)
        ;; Could be coded as memcpy in C.
        (do ((arg-index optional-start (1+ arg-index))
             (frame-slot optional-frame-offset (1+ frame-slot)))
            ((>= arg-index args-end)
             ;; memcpy or similar. (blit bit
             ;; pattern?)
             (do ((frame-slot frame-slot (1+ frame-slot)))
                 ((>= frame-slot optional-frame-end))
               (setf (local stack bp frame-slot) (make-unbound-marker))))
          (setf (local stack bp frame-slot) (stack stack arg-index)))
        ;; Could also be coded as memcpy.
        (do ((arg-index optional-start (1+ arg-index))
             (frame-slot optional-frame-offset (1+ frame-slot)))
            ((>= arg-index end))
          (setf (local stack bp frame-slot) (stack stack arg-index))))))

(declaim (inline parse-key-args))
(defun listify-rest-args (nfixed stack bp argsi nargs)
  (setf (local stack bp nfixed)
        (loop for index from nfixed below nargs
              collect (stack stack (+ argsi index)))))

(declaim (inline parse-key-args))
(defun parse-key-args (nfixed key-count ll-aok-p key-literal-start key-frame-start
                       stack bp nargs argsi constants)
  (declare (type (unsigned-byte 16) nfixed key-count
                 key-literal-start key-frame-start nargs)
           (type (simple-array t (*)) stack)
           (type (and unsigned-byte fixnum) bp argsi))
  (let* ((end (+ argsi nargs))
         (more-start (+ argsi nfixed))
         (key-literal-end (+ key-literal-start key-count))
         (unknown-keys nil)
         (allow-other-keys-p nil))
    ;; Initialize all key values to #<unbound-marker>
    (loop for index from key-frame-start below (+ key-frame-start key-count)
          do (setf (local stack bp index) (make-unbound-marker)))
    (when (> end more-start)
      (do ((arg-index (- end 1) (- arg-index 2)))
          ((< arg-index more-start)
           (cond ((= arg-index (1- more-start)))
                 ((= arg-index (- more-start 2))
                  (error 'arg:odd-keywords))
                 (t
                  (error "BUG! This can't happen!"))))
        (let ((key (stack stack (1- arg-index))))
          (when (eq key :allow-other-keys)
            (setf allow-other-keys-p (stack stack arg-index)))
          (loop for key-index from key-literal-start
                  below key-literal-end
                for offset of-type (unsigned-byte 16)
                from key-frame-start
                do (when (eq (constant constants key-index) key)
                     (setf (local stack bp offset) (stack stack arg-index))
                     (return))
                finally (unless (or allow-other-keys-p
                                    ;; aok is always allowed
                                    (eq key :allow-other-keys))
                          (push key unknown-keys))))))
    (when (and (not (or ll-aok-p allow-other-keys-p))
               unknown-keys)
      (error 'arg:unrecognized-keyword-argument
             :unrecognized-keywords unknown-keys))))
