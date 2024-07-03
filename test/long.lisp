(in-package #:maclina.test)

;;;; Tests of long (two byte operand) instructions.

(5am:def-suite long :in maclina)
(5am:in-suite long)

(5am:test constants
  (5am:is (eql 299 (ceval `(progn ,@(loop for i below 300 collect i)))))
  ;; make sure maclina actually generates code for the constants
  (5am:is (equal '(299)
                 (ceval `(progn ,@(loop for i below 300 collect `(list ,i)))))))

(5am:test calls
  (5am:is (eql 300 (length (ceval `(list ,@(make-list 300))))))
  (5am:is (eql 300 (length (ceval `(multiple-value-call #'list ,@(make-list 300))))))
  (5am:is (eql 300 (length (ceval `(multiple-value-call #'list
                                     (values ,@(make-list 300))))))))

(5am:test vars
  (let ((syms (loop repeat 300 collect (gensym "VAR"))))
    (5am:finishes (ceval `(let (,@syms) (declare (ignore ,@syms)))))
    (5am:is (eql 244 (ceval `(let* (,@(loop for s in syms
                                            for p in (cons 244 syms)
                                            collect `(,s ,p)))
                               ,(first (last syms))))))))

(5am:test leap
  (5am:finishes
    (ccompile nil
              `(lambda (x)
                 (if x
                     (progn ,@(make-list 300 :initial-element '(list nil)))
                     (progn ,@(make-list 300 :initial-element '(list nil))))))))

(5am:test parameters
  (let* ((reqs (loop repeat 300 collect (gensym "ARG")))
         (opts (loop repeat 300 collect (gensym "ARG")))
         (args (make-list 300 :initial-element 398))
         (reql `(lambda (,@reqs)
                  (declare (ignore ,@(rest reqs)))
                  ,(first reqs)))
         (optl1 `(lambda (&optional ,@(loop for s in opts
                                            collect `(,s 983)))
                   (declare (ignore ,@(rest opts)))
                   ,(first opts)))
         ;; Make sure it also works if we start from a long index.
         (optl2 `(lambda (,@reqs
                          &optional ,@(loop for s in opts
                                            collect `(,s 983)))
                   (declare (ignore ,@reqs)
                            (ignore ,@(rest opts)))
                   ,(first opts)))
         (restl `(lambda (,@reqs &rest rest)
                   (declare (ignore ,@reqs))
                   rest))
         (kps (loop repeat 300 collect (gensym "ARG")))
         #+(or)
         (keys (loop for kp in kps collect (intern (symbol-name kp) "KEYWORD")))
         (keyl1 `(lambda (&key ,@kps)
                  (declare (ignore ,@(rest kps)))
                   ,(first kps))))
    (5am:finishes (ccompile nil reql))
    (5am:is (eql 398 (ceval `(,reql ,@args))))
    (5am:finishes (ccompile nil optl1))
    (5am:is (eql 893 (ceval `(,optl1 893))))
    (5am:is (eql 983 (ceval `(,optl1))))
    (5am:finishes (ccompile nil optl2))
    (5am:is (eql 893 (ceval `(,optl2 ,@args 893))))
    (5am:is (eql 983 (ceval `(,optl2 ,@args))))
    (5am:finishes (ccompile nil restl))
    (5am:is (zerop (length (ceval `(,restl ,@args)))))
    (5am:is (equal '(999 0) (ceval `(,restl ,@args 999 0))))
    (5am:finishes (ccompile nil keyl1))))
