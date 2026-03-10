(in-package #:maclina.compile-file)

(defconstant +header-length+ (+ 4 2 2 8))
(defconstant +buffer-size+ 4096) ; arbitrary

(defun link-fasls (dest &rest files)
  (when (null files) (error "No empty link"))
  (let ((ninstructions 0) major minor
        (header (make-array +header-length+ :element-type '(unsigned-byte 8))))
    ;; Loop over the input files reading their headers and counting
    ;; up instructions and checking versions.
    (flet ((read-header (stream)
             (unless (= (read-sequence header stream) +header-length+)
               (error "FASL ~s header too short" stream))))
      (flet ((header-version (header)
               (values (logior (ash (aref header 4) 8) (aref header 5))
                       (logior (ash (aref header 6) 8) (aref header 7))))
             (header-ninstructions (header)
               (logior (ash (aref header  8) 56)
                       (ash (aref header  9) 48)
                       (ash (aref header 10) 40)
                       (ash (aref header 11) 32)
                       (ash (aref header 12) 24)
                       (ash (aref header 13) 16)
                       (ash (aref header 14)  8)
                       (aref header 15))))
        (with-open-file (s (first files) :element-type '(unsigned-byte 8))
          (read-header s)
          (setf (values major minor) (header-version header))
          (incf ninstructions (header-ninstructions header)))
        (dolist (f (rest files))
          (with-open-file (s f :element-type '(unsigned-byte 8))
            (read-header s)
            (multiple-value-bind (fmaj fmin) (header-version header)
              (unless (and (= major fmaj) (= minor fmin))
                (error "Version mismatch: ~d.~d versus ~d.~d"
                       major minor fmaj fmin)))
            (incf ninstructions (header-ninstructions header)))))
      ;; We've read in all the headers and have a final instruction count.
      ;; Make sure it's legit.
      (unless (typep ninstructions '(unsigned-byte 64))
        (error "Linking too many instructions: ~d" ninstructions))
      ;; We're good. Write out our new header, then copy all the old files
      ;; into the new file.
      ;; NOTE: We used to read everything into memory and then write it out,
      ;; so that we only needed to open each input file once, but then you
      ;; can run out of memory when linking large FASLs.
      (with-open-file (o dest :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (write-magic o)
        (write-b16 major o) (write-b16 minor o)
        (write-b64 ninstructions o)
        (let ((buffer
                (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
          (dolist (f files)
            (with-open-file (s f :element-type '(unsigned-byte 8))
              ;; skip header
              (or (file-position s +header-length+)
                (read-header s))
              ;; Copy one buffer at a time.
              (loop for pos = (read-sequence buffer s)
                    do (write-sequence buffer o :end pos)
                    until (< pos +buffer-size+)))))))))
