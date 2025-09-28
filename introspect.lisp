(in-package #:maclina.machine)

(defun dis-signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun bc-unsigned (bytecode ip nbytes)
  ;; Read NBYTES of little-endian integer.
  (do* ((i 0 (1+ i))
        (s 0 (+ 8 s))
        (sum 0))
       ((= i nbytes) sum)
    (incf sum (ash (aref bytecode (+ ip i)) s))))

(defun bc-signed (bytecode ip nbytes)
  (dis-signed (bc-unsigned bytecode ip nbytes)
              (* 8 nbytes)))

;;; Return the instruction description for OPCODE.
(defun decode-instr (opcode)
  (let ((res (member opcode *full-codes* :key #'second)))
    (if res (first res) (error 'unknown-opcode :opcode opcode))))

(defun map-instructions (function bytecode &key (start 0) (end (length bytecode)))
  (loop with ip = start
        with longp = ()
        for desc = (decode-instr (aref bytecode ip))
        if (cl:eq (first desc) 'long)
          do (setf longp t) (incf ip)
        else
          do (let* ((opip (prog1 ip (incf ip)))
                    (argdescs (if longp (fourth desc) (third desc)))
                    (args (loop
                            for argd in argdescs
                            for nbytes = (logandc2 argd +mask-arg+)
                            for unsigned = (bc-unsigned bytecode ip nbytes)
                            collect
                            (cond ((constant-arg-p argd)
                                   (list :constant unsigned))
                                  ((label-arg-p argd)
                                   (list :label
                                         (+ opip (dis-signed unsigned
                                                             (* 8 nbytes)))))
                                  ((keys-arg-p argd)
                                   (list :keys unsigned))
                                  (t (list :operand unsigned)))
                            do (incf ip nbytes))))
               (setf longp ())
               (apply function (first desc) opip longp args))
        until (>= ip end)))

(defmacro do-instructions ((mnemonic ip longp args
                            &key (start nil startp) (end nil endp))
                           bytecode &body body)
  `(map-instructions (lambda (,mnemonic ,ip ,longp &rest ,args) ,@body) ,bytecode
                     ,@(when startp `(:start ,start))
                     ,@(when endp `(:end ,end))))
