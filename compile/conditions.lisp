(in-package #:maclina.compile)

(define-condition compiler-condition (condition)
  ((%source :initarg :source :initform nil :reader source)))

(defmethod source ((condition condition)) nil)

(define-condition program-condition (condition) ())

(define-condition compiler-program-error (program-condition program-error
                                          compiler-condition)
  ())

(define-condition compiler-program-warning (program-condition warning
                                            compiler-condition)
  ())

(define-condition compiler-program-style-warning
    (program-condition style-warning compiler-condition)
  ())

(defmethod print-object :after ((obj compiler-condition) stream)
  (unless *print-escape*
    (let ((source (source obj)))
      (when source
        (format stream "~&    at ~a~%" source)))))
