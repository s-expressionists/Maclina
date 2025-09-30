(in-package #:maclina.machine)

;;;; This file defines two ways to run through bytecode.
;;;; The first way is unstructured. It just presents all the instructions in order,
;;;; making no effort to resolve labels or literals.
;;;; This is very simplistic, but can be done on the bytecode alone. It can be used
;;;; for simple analyses such as a shallow who-calls analysis, and is also used
;;;; in the disassembler.
;;;; The second way is structured and requires annotations. It can be used to
;;;; reconstruct the original program to an extent and is useful for more involved
;;;; analyses such as further compilation.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defun annotation< (annot1 annot2)
  ;; We keep ANNOTS sorted by end IP.
  ;; If they tie on end IP, we use the reverse order of the starts.
  ;; Imagine we have an annotation 5-13 and another 11-13.
  ;; We want to start the 5-13, then start the 11-13, then end the 11-13.
  ;; That's why we reverse order of the starts.
  ;; If two annotations start and end at the same point there may be a
  ;; problem. I don't think this can actually arise.
  (let ((end1 (end annot1)) (end2 (end annot2)))
    (cond ((< end1 end2) t)
          ((= end1 end2) (> (start annot1) (end annot2)))
          (t nil))))

(defun end-annotations (ip annotations f-end context)
  (loop with result = annotations
        for a in annotations
        while (<= (end a) ip)
        do (funcall f-end a context)
           (setf result (cdr result))
        finally (cl:return result)))

(defun start-annotations (annotations new-annotations f-start context)
  (loop for a in new-annotations do (funcall f-start a context))
  (let ((new (sort new-annotations #'annotation<)))
    (merge 'list annotations new #'annotation<)))

(defun initial-annotations (annotations start)
  (loop for annot across annotations
        while (<= (start annot) start)
        when (= (start annot) start)
          collect annot))

(defun map-annotated-instructions (f-instruction f-start f-end
                                   bytecode annotations
                                   &key (start 0) (end (length bytecode)) context)
  (let ((next-annotation-index 0)
        (active-annotations (initial-annotations annotations start))
        (nannotations (length annotations)))
    (do-instructions (mnemonic ip longp args :start start :end end) bytecode
      ;; End annotations that have run out.
      (setf active-annotations (end-annotations ip active-annotations
                                                f-end context))
      ;; Start annotations that are coming into effect.
      (let ((new-annotations
              (loop while (< next-annotation-index nannotations)
                    while (<= (start (aref annotations next-annotation-index)) ip)
                    when (= (start (aref annotations next-annotation-index)) ip)
                      collect (aref annotations next-annotation-index)
                    do (incf next-annotation-index))))
        (setf active-annotations
              (start-annotations active-annotations new-annotations
                                 f-start context)))
      ;; Process the instruction
      (apply f-instruction mnemonic ip longp context args))))

(defun literalizer (function literals)
  (lambda (mnemonic ip longp context &rest args)
    (declare (ignore ip longp))
    (let ((args (if (cl:eq mnemonic :parse-key-args)
                    (destructuring-bind ((t1 . more-args)
                                         (t2 . key-count-info)
                                         (t3 . key-literals-start))
                        args
                      (declare (ignore t1 t2 t3))
                      (let ((aokp (logbitp 0 key-count-info))
                            (key-count (ash key-count-info -1)))
                        (list more-args (cons key-count aokp)
                              (loop for i from key-literals-start
                                    repeat key-count
                                    collect (elt literals i)))))
                    (loop for (type . n) in args
                          collect (ecase type
                                    ((:constant) (elt literals n))
                                    ((:label :operand) n))))))
      (apply function mnemonic context args))))

(defun map-annotated-instructions-literals (f-instruction f-start f-end
                                            bytecode literals annotations
                                            &key (start 0) (end (length bytecode))
                                              context)
  (map-annotated-instructions-literals
   (literalizer f-instruction literals)
   f-start f-end bytecode literals annotations
   :start start :end end :context context))
