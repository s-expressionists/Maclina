(defpackage #:maclina.vm-shared
  (:use #:cl)
  (:local-nicknames (#:arg #:maclina.argparse))
  (:export #:unbound-marker #:make-unbound-marker)
  (:export #:check-arg-count-<= #:check-arg-count->= #:check-arg-count-=)
  (:export #:bind-required-args #:bind-optional-args
           #:listify-rest-args #:parse-key-args))

(in-package #:maclina.vm-shared)

(defstruct (unbound-marker (:constructor make-unbound-marker ())))
(defvar *unbound* (make-unbound-marker))

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

;;; super overkill, but might as well get it right just in case
;;; this is like C's x++
(defmacro dincf (place &optional (delta 1) &environment env)
  (multiple-value-bind (vars vals stores write read)
      (get-setf-expansion place env)
    (let ((temp (gensym "TEMP")))
      `(let* (,@(mapcar #'list vars vals)
              (,temp ,read)
              (,(first stores) (+ ,temp ,delta)))
         ,write
         ,temp))))

(defmacro spush (obj stack sp)
  `(setf (svref ,stack (dincf ,sp)) ,obj))

(declaim (inline bind-optional-args))
(defun bind-optional-args (required-count optional-count stack sp argsi nargs)
  (let ((nfixed (+ required-count optional-count)))
    (loop for i from nfixed above nargs
          do (spush *unbound* stack sp))
    (loop for i from (1- (min nfixed nargs)) downto required-count
          do (spush (stack stack (+ argsi i)) stack sp)))
  sp)

(declaim (inline parse-key-args))
(defun listify-rest-args (nfixed stack argsi nargs)
  (loop for index from nfixed below nargs
        collect (stack stack (+ argsi index))))

(declaim (inline parse-key-args))
(defun parse-key-args (nfixed key-count ll-aok-p key-literal-start
                       stack sp nargs argsi constants)
  (declare (type (unsigned-byte 16) nfixed key-count
                 key-literal-start nargs)
           (type (simple-array t (*)) stack)
           (type (and unsigned-byte fixnum) sp argsi))
  (when (and (> nargs nfixed) (oddp (- nargs nfixed)))
    (error 'arg:odd-keywords))
  (let* ((end (+ argsi nargs))
         (more-start (+ argsi nfixed))
         (key-literal-end (+ key-literal-start key-count))
         (unknown-keys nil)
         (allow-other-keys-p nil)
         (argstemp (make-array key-count :initial-element *unbound*)))
    (declare (dynamic-extent argstemp))
    (loop for arg-aindex from (- end 1) above more-start by 2
          for key-aindex = (- arg-aindex 1)
          do (let ((akey (stack stack key-aindex))
                   (arg (stack stack arg-aindex)))
               (when (eq akey :allow-other-keys)
                 (setf allow-other-keys-p arg))
               (loop for key-index from key-literal-start
                       below key-literal-end
                     for key = (constant constants key-index)
                     for offset of-type (unsigned-byte 16) from 0
                     do (when (eq akey key)
                          (setf (aref argstemp offset) arg)
                          (return))
                     finally (unless (eq akey :allow-other-keys)
                               (push akey unknown-keys)))))
    (when (and (not (or ll-aok-p allow-other-keys-p))
               unknown-keys)
      (error 'arg:unrecognized-keyword-argument
             :unrecognized-keywords unknown-keys))
    (loop for i from (1- key-count) downto 0
          do (spush (aref argstemp i) stack sp))
    sp))
