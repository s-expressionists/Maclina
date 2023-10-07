(in-package #:maclina.test)

(defvar *environment*)

(defun ceval (form)
  (maclina.compile:eval form *environment* m:*client*))

(defun ccompile (name definition)
  (declare (ignore name))
  (etypecase definition
    ((cons (eql lambda))
     (maclina.compile:compile definition *environment*))
    ;; this happens in lambda.55,56
    (function definition)))

(defmacro is-true-eval (form)
  `(5am:is-true (ceval ',form)))

(defmacro signals-eval (condition-type form)
  `(5am:signals ,condition-type (ceval ',form)))

(defmacro is-values-eval (form &rest expected)
  `(5am:is (equal '(,@expected)
                  (multiple-value-list (ceval ',form)))))

(defmacro deftest (name form &rest expected)
  `(5am:test ,name
     (is-values-eval ,form ,@expected)))

(defun run (*environment* m:*client*)
  (let ((*default-pathname-defaults*
	  (asdf:system-relative-pathname :maclina/test "test/sandbox/")))
    (5am:run 'maclina)))

(defun run! (*environment* m:*client*)
  (let ((*default-pathname-defaults*
	  (asdf:system-relative-pathname :maclina/test "test/sandbox/")))
    (5am:run! 'maclina)))
