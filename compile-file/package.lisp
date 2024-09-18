(defpackage #:maclina.compile-file
  (:use #:cl)
  (:local-nicknames (#:cmp #:maclina.compile)
		    (#:m #:maclina.machine))
  (:shadow #:compile-file #:compile-file-pathname
           #:make-load-form
           #:find-package #:package-name)
  (:export #:compile-stream #:compile-file #:compile-file-pathname)
  (:export #:make-load-form #:find-package #:package-name)
  (:export #:link-fasls))
