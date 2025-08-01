(defpackage #:maclina.compile-file
  (:use #:cl)
  (:local-nicknames (#:cmp #:maclina.compile)
		    (#:m #:maclina.machine))
  (:shadow #:compile-file #:compile-file-pathname
           #:make-load-form
           #:find-package #:package-name)
  (:export #:compile-stream #:compile-file #:compile-files #:compile-file-pathname
           #:compile-file-to-unit #:compile-stream-to-unit
           #:write-units #:write-units-to-file)
  (:export #:make-load-form #:find-package #:package-name)
  (:export #:source-location #:source-location-pathname
           #:source-location-position)
  (:export #:link-fasls))
