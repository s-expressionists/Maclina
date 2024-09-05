(in-package #:maclina.compile-file)

(defconstant +header-length+ (+ 4 2 2 8))
(defconstant +initial-buffer-size+ 4096) ; arbitrary

(defun link-fasls (dest &rest files)
  (when (null files) (error "No empty link"))
  (let ((ninstructions 0) major minor
        (instructions (make-array +initial-buffer-size+
                                  :element-type '(unsigned-byte 8)))
        (instlen 0)
        (header (make-array +header-length+
                            :element-type '(unsigned-byte 8))))
    (flet ((read-header (stream)
             (unless (= (read-sequence header stream) +header-length+)
               (error "FASL ~s header too short" stream)))
           (header-version (header)
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
                          (aref header 15)))
           (read-instructions (stream)
             (loop (setf instlen (read-sequence instructions stream :start instlen))
                   (unless (listen stream) (return)) 
                   (let ((new-instructions
                           (make-array (* 2 (length instructions))
                                       :element-type '(unsigned-byte 8))))
                     (replace new-instructions instructions)
                     (setf instructions new-instructions)))))
      (with-open-file (s (first files) :element-type '(unsigned-byte 8))
        (read-header s) (read-instructions s)
        (setf (values major minor) (header-version header))
        (incf ninstructions (header-ninstructions header)))
      (dolist (f (rest files))
        (with-open-file (s f :element-type '(unsigned-byte 8))
          (read-header s) (read-instructions s)
          (multiple-value-bind (fmaj fmin) (header-version header)
            (unless (and (= major fmaj) (= minor fmin))
              (error "Version mismatch: ~d.~d versus ~d.~d"
                     major minor fmaj fmin)))
          (incf ninstructions (header-ninstructions header)))))
    ;; Everything's read - time to dump
    (unless (typep ninstructions '(unsigned-byte 64))
      (error "Linking too many instructions: ~d" ninstructions))
    (with-open-file (o dest :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-magic o)
      (write-b16 major o) (write-b16 minor o)
      (write-b64 ninstructions o)
      (write-sequence instructions o)))
  dest)
