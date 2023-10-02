(in-package #:cvm.test.cross)

(defvar *client* (make-instance 'client))

(defun fill-environment (environment)
  (%fill-environment *client* environment))

(defun run (environment)
  (let (;; system construction tests work in the sandbox.
        (*default-pathname-defaults*
          (asdf:system-relative-pathname :cvm/test "test/sandbox/"))
        (cvm.test::*environment* environment)
        (cvm.test::*client* *client*)
        (cvm.machine:*client* *client*))
    (5am:run 'cvm-cross)))
(defun run! (environment)
  (let ((*default-pathname-defaults*
          (asdf:system-relative-pathname :cvm/test "test/sandbox/"))
        (cvm.test::*environment* environment)
        (cvm.test::*client* *client*)
        (cvm.machine:*client* *client*))
    (5am:run! 'cvm.test::cvm-cross)))
