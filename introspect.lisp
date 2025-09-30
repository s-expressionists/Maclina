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

;;; Return two values: (mnemonic longp . args), and the ip of the next instruction.
(defun disassemble-instruction (bytecode ip)
  (let ((desc (decode-instr (aref bytecode ip)))
        (longp ()) (opip ip))
    (when (cl:eq (first desc) 'long)
      (setf longp t desc (decode-instr (aref bytecode (incf opip)))))
    (setf ip (1+ opip))
    (loop with argdescs = (if longp (fourth desc) (third desc))
          for argd in argdescs
          for nbytes = (logandc2 argd +mask-arg+)
          for unsigned = (bc-unsigned bytecode ip nbytes)
          collect (cond ((constant-arg-p argd) (cons :constant unsigned))
                        ((label-arg-p argd)
                         (cons :label
                               (+ opip (dis-signed unsigned (* 8 nbytes)))))
                        ((keys-arg-p argd) (cons :keys unsigned))
                        (t (cons :operand unsigned)))
            into args
          do (incf ip nbytes)
          finally (cl:return (values (list* (first desc) longp args) ip)))))

(defun map-instructions (function bytecode &key (start 0) (end (length bytecode)))
  (loop with ip = start
        do (multiple-value-bind (inst new-ip)
               (disassemble-instruction bytecode ip)
             (apply function (first inst) ip (second inst) (cddr inst))
             (setf ip new-ip))
        until (>= ip end)))

(defmacro do-instructions ((mnemonic ip longp args
                            &key (start nil startp) (end nil endp))
                           bytecode &body body)
  `(map-instructions (lambda (,mnemonic ,ip ,longp &rest ,args) ,@body) ,bytecode
                     ,@(when startp `(:start ,start))
                     ,@(when endp `(:end ,end))))
