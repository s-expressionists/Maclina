(defpackage #:maclina.compile-file
  (:use #:cl)
  (:local-nicknames (#:cmp #:maclina.compile)
		    (#:m #:maclina.machine))
  (:shadow #:compile-file #:compile-file-pathname)
  (:export #:compile-stream #:compile-file #:compile-file-pathname))
