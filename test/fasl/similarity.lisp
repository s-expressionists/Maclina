(in-package #:maclina.test)

;;; See 3.2.4.2.2

(defvar *seen*)

(defgeneric %similarp (S C)
  (:method :around (S C)
    (cond ((member C (gethash S *seen*))
           t) ; recursed
          (t
           (push C (gethash S *seen*))
           (call-next-method))))
  ;; default
  (:method (S C) (declare (ignore S C)) nil))

(defun similarp (S C)
  (let ((*seen* (make-hash-table :test #'eq)))
    (%similarp S C)))

(defmethod %similarp ((S number) (C number)) (eql S C))

(defmethod %similarp ((S character) (C character))
  (%similarp (char-code S) (char-code C)))

(defmethod %similarp ((S symbol) (C symbol))
  ;; this is not just eql because #:FOO and #:FOO are similar.
  (and (%similarp (symbol-name S) (symbol-name C))
       (if (null (symbol-package S))
           (null (symbol-package C))
           (%similarp (symbol-package S) (symbol-package C)))))

(defmethod %similarp ((S package) (C package))
  (%similarp (package-name S) (package-name C)))

;;; random state we can't portably test

(defmethod %similarp ((S cons) (C cons))
  (and (%similarp (car S) (car C)) (%similarp (cdr S) (cdr C))))

(defmethod %similarp ((S array) (C array))
  (let ((rank (array-rank S)))
    (and (%similarp rank (array-rank C))
         (%similarp (array-element-type S) (array-element-type C))
         (if (typep S 'simple-array) (typep C 'simple-array) T)
         (if (= rank 1)
             (and (%similarp (length S) (length C))
                  (every #'similarp S C))
             (and (%similarp (array-dimensions S) (array-dimensions C))
                  (loop for i below (array-total-size S)
                        always (%similarp (row-major-aref S i)
                                         (row-major-aref C i))))))))

(defmethod %similarp ((S hash-table) (C hash-table))
  (and (eql (hash-table-test S) (hash-table-test C))
       (= (hash-table-count S) (hash-table-count C))
       (loop for k1 being the hash-keys of S using (hash-value v1)
             always (loop for k2 being the hash-keys of C using (hash-value v2)
                          when (%similarp k1 k2)
                            return (%similarp v1 v2)
                          finally (return nil)))))

(defmethod %similarp ((S pathname) (C pathname))
  (and (%similarp (pathname-host S) (pathname-host C))
       (%similarp (pathname-device S) (pathname-device C))
       (%similarp (pathname-directory S) (pathname-directory C))
       (%similarp (pathname-name S) (pathname-name C))
       (%similarp (pathname-type S) (pathname-type C))
       (%similarp (pathname-version S) (pathname-version C))))
