(in-package #:maclina.test)

;;;; This file tests more pedantic behavior on IGNORE:
;;;; that Maclina complains when a variable is unused, and so on.
;;;; This is not required by the standard so it is not in the ANSI tests.

;;; we use .warn in order to avoid conflict with the ANSI ignore suite.
(5am:def-suite ignore.warn :in maclina)
(5am:in-suite ignore.warn)

(defmacro test-style (lambda-expression)
  `(progn
     (5am:signals style-warning (ccompile nil ',lambda-expression))
     (5am:is (not (null (nth-value 1 (ccompile nil ',lambda-expression))))
             "~s compiled with no warnings" ',lambda-expression)
     (5am:is (null (nth-value 2 (ccompile nil ',lambda-expression)))
             "~s compiled with errors" ',lambda-expression)))

(defmacro test-nostyle (lambda-expression)
  `(progn
     (5am:is (eql t (handler-case (progn (ccompile nil ',lambda-expression) t)
                      (style-warning () nil)))
             "Compiling ~s signaled a style-warning" ',lambda-expression)
     (5am:is (null (nth-value 1 (ccompile nil ',lambda-expression)))
             "~s compiled with warnings" ',lambda-expression)
     (5am:is (null (nth-value 1 (ccompile nil ',lambda-expression)))
             "~s compiled with errors" ',lambda-expression)))

(5am:test noignore
  (test-style (lambda (x)))
  (test-style (lambda (&optional x)))
  (test-style (lambda (&optional (x nil xp)) x))
  (test-style (lambda (&rest r)))
  (test-style (lambda (&key k)))
  (test-style (lambda (&key (k nil kp)) k))
  (test-style (lambda () (let ((y 7)))))
  (test-style (lambda () (let* ((y 7)))))
  (test-style (lambda () (flet ((foo ())))))
  (test-style (lambda () (labels ((foo ())))))
  (test-nostyle (lambda (x) x))
  (test-nostyle (lambda (&optional x) x))
  ;; this also tests 0 contexts, which is important as the compiler
  ;; tends to skip a lot of work there.
  (test-nostyle (lambda (&optional (x nil xp)) (progn x xp)))
  (test-nostyle (lambda (&rest r) r))
  (test-nostyle (lambda (&key k) k))
  (test-nostyle (lambda (&key (k nil kp)) (progn k kp)))
  (test-nostyle (lambda () (let ((y 7)) y)))
  (test-nostyle (lambda () (let* ((y 7)) y)))
  (test-nostyle (lambda () (flet ((foo ())) (foo))))
  (test-nostyle (lambda () (flet ((foo ())) #'foo)))
  (test-nostyle (lambda () (labels ((foo ())) (foo))))
  (test-nostyle (lambda () (labels ((foo ())) #'foo))))

(5am:test ignore
  (test-nostyle (lambda (x) (declare (ignore x))))
  (test-nostyle (lambda (&optional x) (declare (ignore x))))
  (test-nostyle (lambda (&optional (x nil xp)) (declare (ignore xp)) x))
  (test-nostyle (lambda (&rest r) (declare (ignore r))))
  (test-nostyle (lambda (&key k) (declare (ignore k))))
  (test-nostyle (lambda (&key (k nil kp)) (declare (ignore kp)) k))
  (test-nostyle (lambda () (let ((y 7)) (declare (ignore y)))))
  (test-nostyle (lambda () (let* ((y 7)) (declare (ignore y)))))
  (test-nostyle (lambda () (flet ((foo ())) (declare (ignore #'foo)))))
  (test-nostyle (lambda () (labels ((foo ())) (declare (ignore #'foo)))))
  (test-style (lambda (x) (declare (ignore x)) x))
  (test-style (lambda (&optional x) (declare (ignore x)) x))
  ;; this also tests 0 contexts, which is important as the compiler
  ;; tends to skip a lot of work there.
  (test-style (lambda (&optional (x nil xp)) (declare (ignore x)) (progn x xp)))
  (test-style (lambda (&rest r) (declare (ignore r)) r))
  (test-style (lambda (&key k) (declare (ignore k)) k))
  (test-style (lambda (&key (k nil kp)) (declare (ignore k)) (progn k kp)))
  (test-style (lambda () (let ((y 7)) (declare (ignore y)) y)))
  (test-style (lambda () (let* ((y 7)) (declare (ignore y)) y)))
  (test-style (lambda () (flet ((foo ())) (declare (ignore #'foo)) (foo))))
  (test-style (lambda () (flet ((foo ())) (declare (ignore #'foo)) #'foo)))
  (test-style (lambda () (labels ((foo ())) (declare (ignore #'foo)) (foo))))
  (test-style (lambda () (labels ((foo ())) (declare (ignore #'foo)) #'foo))))

(5am:test ignorable
  (test-nostyle (lambda (x) (declare (ignorable x))))
  (test-nostyle (lambda (&optional x) (declare (ignorable x))))
  (test-nostyle (lambda (&optional (x nil xp)) (declare (ignorable xp)) x))
  (test-nostyle (lambda (&rest r) (declare (ignorable r))))
  (test-nostyle (lambda (&key k) (declare (ignorable k))))
  (test-nostyle (lambda (&key (k nil kp)) (declare (ignorable kp)) k))
  (test-nostyle (lambda () (let ((y 7)) (declare (ignorable y)))))
  (test-nostyle (lambda () (let* ((y 7)) (declare (ignorable y)))))
  (test-nostyle (lambda () (flet ((foo ())) (declare (ignorable #'foo)))))
  (test-nostyle (lambda () (labels ((foo ())) (declare (ignorable #'foo)))))
  (test-nostyle (lambda (x) (declare (ignorable x)) x))
  (test-nostyle (lambda (&optional x) (declare (ignorable x)) x))
  (test-nostyle (lambda (&optional (x nil xp)) (declare (ignorable x)) (progn x xp)))
  (test-nostyle (lambda (&rest r) (declare (ignorable r)) r))
  (test-nostyle (lambda (&key k) (declare (ignorable k)) k))
  (test-nostyle (lambda (&key (k nil kp)) (declare (ignorable k)) (progn k kp)))
  (test-nostyle (lambda () (let ((y 7)) (declare (ignorable y)) y)))
  (test-nostyle (lambda () (let* ((y 7)) (declare (ignorable y)) y)))
  (test-nostyle (lambda () (flet ((foo ())) (declare (ignorable #'foo)) (foo))))
  (test-nostyle (lambda () (flet ((foo ())) (declare (ignorable #'foo)) #'foo)))
  (test-nostyle (lambda () (labels ((foo ())) (declare (ignorable #'foo)) (foo))))
  (test-nostyle (lambda () (labels ((foo ())) (declare (ignorable #'foo)) #'foo))))
