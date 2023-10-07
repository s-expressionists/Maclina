(in-package #:maclina.test)

(defun run-native! (native-client)
  (maclina.test:run! nil native-client))

(defun run-cross! ()
  (let* ((rte (make-instance 'clostrum-basic:run-time-environment))
         (ce (make-instance 'clostrum-basic:compilation-environment
               :parent rte)))
    (maclina.test.cross:fill-environment rte)
    (maclina.test.cross:run! ce)))
