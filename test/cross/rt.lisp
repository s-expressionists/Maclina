(in-package #:maclina.test.cross)

(defvar *client* (make-instance 'client))

(defun fill-environment (environment)
  (%fill-environment *client* environment))

(defun run (maclina.test::*environment*)
  (let (;; system construction tests work in the sandbox.
        (*default-pathname-defaults*
          (asdf:system-relative-pathname :maclina/test "test/sandbox/"))
        (maclina.machine:*client* *client*))
    (5am:run 'maclina.test::maclina-cross)))
(defun run! (maclina.test::*environment*)
  (let ((*default-pathname-defaults*
          (asdf:system-relative-pathname :maclina/test "test/sandbox/"))
        (maclina.machine:*client* *client*))
    (5am:run! 'maclina.test::maclina-cross)))
