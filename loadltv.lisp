(defpackage #:maclina.load
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:float #:ieee-floats))
  (:export #:load-bytecode #:load-bytecode-stream))

(in-package #:maclina.load)

(defparameter +ops+
  '((nil 65 sind)
    (t 66 sind)
    (ratio 67)
    (complex 68)
    (cons 69 sind)
    (rplaca 70 ind1 ind2) ; (setf (car [ind1]) [ind2])
    (rplacd 71 ind1 ind2)
    (make-array 74 sind rank . dims)
    (setf-row-major-aref 75 arrayind rmindex valueind)
    (make-hash-table 76 sind test count)
    (setf-gethash 77 htind keyind valueind)
    (make-sb64 78 sind sb64)
    (find-package 79 sind nameind)
    (make-bignum 80 sind size . words) ; size is signed
    (make-symbol 81)
    (intern 82 sind packageind nameind)
    (make-character 83 sind ub32)
    (make-pathname 85)
    (make-bytecode-function 87)
    (make-bytecode-module 88)
    (setf-literals 89 modind litsind)
    (make-single-float 90 sind ub32)
    (make-double-float 91 sind ub64)
    (funcall-create 93 sind fnind)
    (funcall-initialize 94 fnind)
    (fdefinition 95 find nameind)
    (fcell 96 find nameind)
    (vcell 97 vind nameind)
    (find-class 98 sind cnind)
    (init-object-array 99 ub64)
    (environment 100)
    (attribute 255 name nbytes . data)))

;;; Read an unsigned n-byte integer from a ub8 stream, big-endian.
(defun read-ub (n stream)
  ;; read-sequence might be better but bla bla consing
  (loop with int = 0
        repeat n
        do (setf int (logior (ash int 8) (read-byte stream)))
        finally (return int)))

(defun read-ub64 (stream) (read-ub 8 stream))
(defun read-ub32 (stream) (read-ub 4 stream))
(defun read-ub16 (stream) (read-ub 2 stream))

;;; Read a signed n-byte integer from a ub8 stream, big-endian.
(defun read-sb (n stream)
  (let ((word (read-ub n stream))
        (nbits (* n 8)))
    (declare (type (integer 1 64) nbits))
    ;; Read sign bit and make this negative if it's set.
    ;; FIXME: Do something more efficient probably.
    (- word (ash (ldb (byte 1 (1- nbits)) word) nbits))))

(defun read-sb64 (stream) (read-sb 8 stream))
(defun read-sb32 (stream) (read-sb 4 stream))
(defun read-sb16 (stream) (read-sb 2 stream))
(defun read-sb8  (stream) (read-sb 1 stream))

(defconstant +magic+ #x8d7498b1) ; randomly chosen bytes.

(defmacro verboseprint (message &rest args)
  `(when *load-verbose*
     (format t ,(concatenate 'string "~&; " message "~%") ,@args)))
(defmacro printprint (message &rest args)
  `(when *load-print*
     (format t ,(concatenate 'string "~&; " message "~%") ,@args)))

(defvar *debug-loader* nil)

(defmacro dbgprint (message &rest args)
  `(when *debug-loader*
     (format *error-output* ,(concatenate 'string "~&; " message "~%") ,@args)))

(defun load-magic (stream)
  (let ((magic (read-ub32 stream)))
    (unless (= magic +magic+)
      (error "~s is not a valid bytecode FASL: invalid magic identifier ~d"
             stream magic))
    (dbgprint "Magic number matches: ~x" magic)))

;; Bounds for major and minor version understood by this loader.
(defparameter *min-version* '(0 15))
(defparameter *max-version* '(0 15))

(defun loadable-version-p (major minor)
  (and
   ;; minimum
   (if (= major (first *min-version*))
       (>= minor (second *min-version*))
       (> major (first *min-version*)))
   ;; maximum
   (if (= major (first *max-version*))
       (<= minor (second *max-version*))
       (< major (first *max-version*)))))

(defun load-version (stream)
  (let ((major (read-ub16 stream)) (minor (read-ub16 stream)))
    (unless (loadable-version-p major minor)
      (error "Don't know how to load bytecode FASL format version ~d.~d
(This loader only understands ~d.~d to ~d.~d)"
             major minor (first *min-version*) (second *min-version*)
             (first *max-version*) (second *max-version*)))
    (dbgprint "File version ~d.~d (loader accepts ~d.~d-~d.~d)"
              major minor (first *min-version*) (second *min-version*)
              (first *max-version*) (second *max-version*))
    (values major minor)))

;; Major and minor version of the file being read.
(defvar *major*)
(defvar *minor*)

;; how many bytes are needed to represent an index?
(defvar *index-bytes*)

(defun read-index (stream)
  (ecase *index-bytes*
    ((1) (read-byte stream))
    ((2) (read-ub16 stream))
    ((4) (read-ub32 stream))
    ((8) (read-ub64 stream))))

(defun read-mnemonic (stream)
  (let* ((opcode (read-byte stream))
         (info (find opcode +ops+ :key #'second)))
    (if info
        (first info)
        (error "BUG: Unknown opcode #x~x" opcode))))

;; Constants vector we're producing.
(defvar *constants*)
(declaim (type simple-vector *constants*))

;; The environment we're loading into.
(defvar *environment*)

;;; The next free index, where the next creator's value will be stored.
(defvar *next-index* 0)

(define-condition loader-error (file-error)
  ()
  (:default-initargs :pathname *load-pathname*))

(define-condition invalid-fasl (loader-error) ())

(define-condition uninitialized-constant (invalid-fasl)
  ((%index :initarg :index :reader offending-index))
  (:report (lambda (condition stream)
             (format stream "FASL ~s is invalid:
Tried to read constant #~d before initializing it"
                     (file-error-pathname condition)
                     (offending-index condition)))))

(define-condition index-out-of-range (invalid-fasl)
  ((%index :initarg :index :reader offending-index)
   (%nobjs :initarg :nobjs :reader nobjs))
  (:report (lambda (condition stream)
             (format stream "FASL ~s is invalid:
Tried to access constant #~d, but there are only ~d constants in the FASL."
                     (file-error-pathname condition)
                     (offending-index condition) (nobjs condition)))))

(define-condition not-all-initialized (invalid-fasl)
  ((%indices :initarg :indices :reader offending-indices))
  (:report (lambda (condition stream)
             (format stream "FASL ~s is invalid:
Did not initialize constants~{ #~d~}"
                     (file-error-pathname condition)
                     (offending-indices condition)))))

(defun check-initialization ()
  (unless (= *next-index* (length *constants*))
    (error 'not-all-initialized
           :indices (loop for i from *next-index*
			    below (length *constants*)
			  collect i)))
  (values))

(defun constant (index)
  (cond ((not (array-in-bounds-p *constants* index))
         (error 'index-out-of-range :index index
                                    :nobjs (length *constants*)))
	((>= index *next-index*)
	 (error 'uninitialized-constant :index index))
        (t (aref *constants* index))))

(defun (setf next-constant) (value)
  (cond ((not (array-in-bounds-p *constants* *next-index*))
         (error 'index-out-of-range :index *next-index*
                                    :nobjs (length *constants*)))
        (t
         (setf (aref *constants* *next-index*) value)))
  (incf *next-index*)
  value)

;; Versions 0.0-0.2: Return how many bytes were read.
;; Versions 0.3-: Return value irrelevant.
(defgeneric %load-instruction (mnemonic stream))

(defmethod %load-instruction ((mnemonic (eql 'nil)) stream)
  (dbgprint " (nil)")
  (setf (next-constant) nil))

(defmethod %load-instruction ((mnemonic (eql 't)) stream)
  (dbgprint " (t)")
  (setf (next-constant) t))

(defmethod %load-instruction ((mnemonic (eql 'cons)) stream)
  (dbgprint " (cons)")
  (setf (next-constant) (cons nil nil)))

(defmethod %load-instruction ((mnemonic (eql 'rplaca)) stream)
  (let ((cons (read-index stream)) (value (read-index stream)))
    (dbgprint " (rplaca ~d ~d)" cons value)
    (setf (car (constant cons)) (constant value))))

(defmethod %load-instruction ((mnemonic (eql 'rplacd)) stream)
  (let ((cons (read-index stream)) (value (read-index stream)))
    (dbgprint " (rplacd ~d ~d)" cons value)
    (setf (cdr (constant cons)) (constant value))))

(defmacro read-sub-byte (array stream nbits)
  (let ((perbyte (floor 8 nbits))
        (a (gensym "ARRAY")) (s (gensym "STREAM")))
    `(let* ((,a ,array) (,s ,stream)
            (total-size (array-total-size ,a)))
       (multiple-value-bind (full-bytes remainder) (floor total-size 8)
         (loop for byteindex below full-bytes
               for index = (* ,perbyte byteindex)
               for byte = (read-byte ,s)
               do ,@(loop for j below perbyte
                          for bit-index
                            = (* nbits (- perbyte j 1))
                          for bits = `(ldb (byte ,nbits ,bit-index)
                                           byte)
                          for arrindex = `(+ index ,j)
                          collect `(setf (row-major-aref array ,arrindex) ,bits)))
         ;; write remainder
         (let* ((index (* ,perbyte full-bytes))
                (byte (read-byte ,s)))
           (loop for j below remainder
                 for bit-index = (* ,nbits (- ,perbyte j 1))
                 for bits = (ldb (byte ,nbits bit-index) byte)
                 do (setf (row-major-aref ,a (+ index j)) bits)))))))

(define-condition illegal-utf8 (invalid-fasl) ())

(define-condition illegal-utf8-continuation (illegal-utf8)
  ((%bytes :initarg :bytes :reader utf8-bytes))
  (:report (lambda (condition stream)
	     (format stream "Illegal UTF-8 sequence: expected continuation bytes while reading~{ #x~x~}"
		     (utf8-bytes condition)))))

(defun illegal-utf8-continuation (&rest bytes)
  (error 'illegal-utf8-continuation :bytes bytes))

(define-condition illegal-utf8-head (illegal-utf8)
  ((%byte :initarg :byte :reader utf8-byte))
  (:report (lambda (condition stream)
	     (format stream "Illegal UTF-8 sequence: encountered invalid byte #x~x as the start of a codepoint"
		     (utf8-byte condition)))))

(defun illegal-utf8-head (byte)
  (error 'illegal-utf8-head :byte byte))

(declaim (inline continuation-byte-p))
(defun continuation-byte-p (byte)
  (declare (optimize speed) (type (unsigned-byte 8) byte))
  (= #b10000000 (mask-field (byte 2 6) byte)))

(defun read-utf8-codepoint (stream)
  (declare (optimize speed))
  (let ((byte0 (read-byte stream)))
    (declare (type (unsigned-byte 8) byte0))
    (cond
      ((= #b00000000 (mask-field (byte 1 7) byte0)) ; one byte
       byte0)
      ((= #b11000000 (mask-field (byte 3 5) byte0)) ; two bytes
       (let ((byte1 (read-byte stream)))
	 (declare (type (unsigned-byte 8) byte1))
	 (unless (continuation-byte-p byte1)
	   (illegal-utf8-continuation byte0 byte1))
	 (logior (ash (ldb (byte 5 0) byte0) 6)
  		      (ldb (byte 6 0) byte1))))
      ((= #b11100000 (mask-field (byte 4 4) byte0)) ; three
       (let ((byte1 (read-byte stream))
	     (byte2 (read-byte stream)))
	 (declare (type (unsigned-byte 8) byte1 byte2))
	 (unless (and (continuation-byte-p byte1)
		      (continuation-byte-p byte2))
	   (illegal-utf8-continuation byte0 byte1 byte2))
	 (logior (ash (ldb (byte 4 0) byte0) 12)
		 (ash (ldb (byte 6 0) byte1)  6)
		      (ldb (byte 6 0) byte2))))
      ((= #b11110000 (mask-field (byte 5 3) byte0)) ; four
       (let ((byte1 (read-byte stream))
	     (byte2 (read-byte stream))
	     (byte3 (read-byte stream)))
	 (declare (type (unsigned-byte 8) byte1 byte2 byte3))
	 (unless (and (continuation-byte-p byte1)
		      (continuation-byte-p byte2)
		      (continuation-byte-p byte3))
	   (illegal-utf8-continuation byte0 byte1 byte2 byte3))
	 (logior (ash (ldb (byte 3 0) byte0) 18)
		 (ash (ldb (byte 6 0) byte1) 12)
		 (ash (ldb (byte 6 0) byte2)  6)
		      (ldb (byte 6 0) byte3))))
      (t (illegal-utf8-head byte0)))))

(defun read-utf8 (array stream)
  (declare (optimize speed) (type (simple-array character) array))
  ;; Since we only make simple arrays right now, we declare that type to
  ;; maybe make this slightly faster. With complex arrays that would change.
  ;; WARNING: Like compile-file, we assume that code-char operates on Unicode
  ;; codepoints. Which is true unless your Lisp is a scrub.
  (loop for i below (array-total-size array)
	for cpoint = (read-utf8-codepoint stream)
	for char = (code-char cpoint)
	do (setf (row-major-aref array i) char)))

(defmethod %load-instruction ((mnemonic (eql 'make-array)) stream)
  (let* ((etcode (read-byte stream))
         (element-type (decode-element-type etcode stream))
         (packing-code (read-byte stream))
         (packing-type (decode-packing packing-code))
         (rank (read-byte stream))
         (dimensions (loop repeat rank collect (read-ub16 stream)))
         (array (make-array dimensions :element-type element-type)))
    (dbgprint " (make-array ~d ~x ~d)" element-typei packing-code rank)
    (dbgprint "  dimensions ~a" dimensions)
    (setf (next-constant) array)
    (macrolet ((undump (form)
                 `(loop for i below (array-total-size array)
                        for elem = ,form
                        do (setf (row-major-aref array i) elem))))
      (cond ((eql etcode +other-uaet+)) ; handled via setf-aref
            ((equal packing-type 'nil))
            ((equal packing-type 'base-char)
             (undump (code-char (read-byte stream))))
            ((equal packing-type 'character)
	     (read-utf8 array stream))
            ((equal packing-type 'single-float)
             (undump (float:decode-float32 (read-ub32 stream))))
            ((equal packing-type 'double-float)
             (undump (float:decode-float64 (read-ub64 stream))))
            ((equal packing-type '(complex single-float))
             (undump
              (complex (float:decode-float32 (read-ub32 stream))
                       (float:decode-float32 (read-ub32 stream)))))
            ((equal packing-type '(complex double-float))
             (undump
              (complex (float:decode-float64 (read-ub64 stream))
                       (float:decode-float64 (read-ub64 stream)))))
            ((equal packing-type 'bit) (read-sub-byte array stream 1))
            ((equal packing-type '(unsigned-byte 2))
             (read-sub-byte array stream 2))
            ((equal packing-type '(unsigned-byte 4))
             (read-sub-byte array stream 4))
            ((equal packing-type '(unsigned-byte 8))
             (read-sequence array stream))
            ((equal packing-type '(unsigned-byte 16))
             (undump (read-ub16 stream)))
            ((equal packing-type '(unsigned-byte 32))
             (undump (read-ub32 stream)))
            ((equal packing-type '(unsigned-byte 64))
             (undump (read-ub64 stream)))
            ((equal packing-type '(signed-byte 8))
             (undump (read-sb8  stream)))
            ((equal packing-type '(signed-byte 16))
             (undump (read-sb16 stream)))
            ((equal packing-type '(signed-byte 32))
             (undump (read-sb32 stream)))
            ((equal packing-type '(signed-byte 64))
             (undump (read-sb64 stream)))
            ((equal packing-type 't)) ; setf-aref takes care of it
            (t (error "BUG: Unknown packing-type ~s" packing-type))))))

(defmethod %load-instruction ((mnemonic (eql 'setf-row-major-aref)) stream)
  (let ((index (read-index stream)) (aindex (read-ub16 stream))
        (value (read-index stream)))
    (dbgprint " ((setf row-major-aref) ~d ~d ~d" index aindex value)
    (setf (row-major-aref (constant index) aindex)
          (constant value))))

(defmethod %load-instruction ((mnemonic (eql 'make-hash-table)) stream)
  (dbgprint " (make-hash-table)")
  (let* ((testcode (read-byte stream))
         (test (ecase testcode
                 ((#b00) 'eq)
                 ((#b01) 'eql)
                 ((#b10) 'equal)
                 ((#b11) 'equalp)))
         (count (read-ub16 stream)))
    (dbgprint "  test = ~a, count = ~d" test count)
    (setf (next-constant) (make-hash-table :test test :size count))))

(defmethod %load-instruction ((mnemonic (eql 'setf-gethash)) stream)
  (let ((htind (read-index stream))
        (keyind (read-index stream)) (valind (read-index stream)))
    (dbgprint " ((setf gethash) ~d ~d ~d)" htind keyind valind)
    (setf (gethash (constant keyind) (constant htind))
          (constant valind))))

(defmethod %load-instruction ((mnemonic (eql 'make-sb64)) stream)
  (let ((sb64 (read-sb64 stream)))
    (dbgprint " (make-sb64 ~d)" sb64)
    (setf (next-constant) sb64)))

(defmethod %load-instruction ((mnemonic (eql 'find-package)) stream)
  (let ((name (read-index stream)))
    (dbgprint " (find-package ~d)" name)
    (setf (next-constant) (find-package (constant name)))))

(defmethod %load-instruction ((mnemonic (eql 'make-bignum)) stream)
  (let ((ssize (read-sb64 stream)))
    (dbgprint " (make-bignum ~d)" ssize)
    (setf (next-constant)
          (let ((result 0) (size (abs ssize)) (negp (minusp ssize)))
            (loop repeat size
                  do (let ((word (read-ub64 stream)))
                       (dbgprint  "#x~8,'0x" word)
                       (setf result (logior (ash result 64) word)))
                  finally (return (if negp (- result) result)))))))

(defmethod %load-instruction ((mnemonic (eql 'make-single-float)) stream)
  (let ((bits (read-ub32 stream)))
    (dbgprint " (make-single-float #x~4,'0x)" bits)
    (setf (next-constant) (float:decode-float32 bits))))

(defmethod %load-instruction ((mnemonic (eql 'make-double-float)) stream)
  (let ((bits (read-ub64 stream)))
    (dbgprint " (make-double-float #x~8,'0x)" bits)
    (setf (next-constant) (float:decode-float64 bits))))

(defmethod %load-instruction ((mnemonic (eql 'ratio)) stream)
  (let ((numi (read-index stream)) (deni (read-index stream)))
    (dbgprint " (ratio ~d ~d)" numi deni)
    (setf (next-constant)
          ;; a little inefficient.
          (/ (constant numi) (constant deni)))))

(defmethod %load-instruction ((mnemonic (eql 'complex)) stream)
  (let ((reali (read-index stream)) (imagi (read-index stream)))
    (dbgprint " (complex ~d ~d)" reali imagi)
    (setf (next-constant) (complex (constant reali) (constant imagi)))))

(defmethod %load-instruction ((mnemonic (eql 'make-symbol)) stream)
  (let ((namei (read-index stream)))
    (dbgprint " (make-symbol ~d)" namei)
    (setf (next-constant) (make-symbol (constant namei)))))

(defmethod %load-instruction ((mnemonic (eql 'intern)) stream)
  (let ((package (read-index stream)) (name (read-index stream)))
    (dbgprint " (intern ~d ~d)" package name)
    (setf (next-constant) (intern (constant name) (constant package)))))

(defmethod %load-instruction ((mnemonic (eql 'make-character)) stream)
  (let* ((code (read-ub32 stream))
         (char (code-char code)))
    (dbgprint " (make-character #x~x) ; ~c" code char)
    (setf (next-constant) char)))

(defmethod %load-instruction ((mnemonic (eql 'make-pathname)) stream)
  (let ((hosti (read-index stream)) (devicei (read-index stream))
        (directoryi (read-index stream)) (namei (read-index stream))
        (typei (read-index stream)) (versioni (read-index stream)))
    (dbgprint " (make-pathname ~d ~d ~d ~d ~d ~d)"
              hosti devicei directoryi namei typei versioni)
    (setf (next-constant)
          (make-pathname :host (constant hosti)
                         :device (constant devicei)
                         :directory (constant directoryi)
                         :name (constant namei)
                         :type (constant typei)
                         :version (constant versioni)))))

(defconstant +other-uaet+   #b11111110)

(defvar +array-packing-infos+
  '((nil                    #b00000000)
    (base-char              #b10000000)
    (character              #b11000000)
    ;;(short-float          #b10100000) ; i.e. binary16
    (single-float           #b00100000) ; binary32
    (double-float           #b01100000) ; binary64
    ;;(long-float           #b11100000) ; binary128?
    ;;((complex short...)   #b10110000)
    ((complex single-float) #b00110000)
    ((complex double-float) #b01110000)
    ;;((complex long...)    #b11110000)
    (bit                    #b00000001) ; (2^(code-1)) bits
    ((unsigned-byte 2)      #b00000010)
    ((unsigned-byte 4)      #b00000011)
    ((unsigned-byte 8)      #b00000100)
    ((unsigned-byte 16)     #b00000101)
    ((unsigned-byte 32)     #b00000110)
    ((unsigned-byte 64)     #b00000111)
    ;;((unsigned-byte 128) ??)
    ((signed-byte 8)        #b10000100)
    ((signed-byte 16)       #b10000101)
    ((signed-byte 32)       #b10000110)
    ((signed-byte 64)       #b10000111)
    ;; invalid:             #b11111110 ; see +other-uaet+
    (t                      #b11111111)))

(defun decode-packing (code)
  (or (first (find code +array-packing-infos+ :key #'second))
      (error "BUG: Unknown array packing code ~x" code)))

(defun decode-element-type (code stream)
  (cond ((eql code +other-uaet+) (constant (read-index stream)))
        ((first (find code +array-packing-infos+ :key #'second)))
        (error "BUG: Unknown array element type code ~x" code)))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-function)) stream)
  (let ((entry-point (read-ub32 stream))
        (size (read-ub32 stream))
        (nlocals (read-ub16 stream))
        (nclosed (read-ub16 stream))
        (modulei (read-index stream)))
    (dbgprint " (make-bytecode-function ~d ~d ~d ~d)"
              entry-point nlocals nclosed modulei)
    (let ((module (constant modulei)))
      (dbgprint "  entry-point = ~d, nlocals = ~d, nclosed = ~d"
                entry-point nlocals nclosed)
      (dbgprint "  module-index = ~d" modulei)
      (setf (next-constant)
            (m:make-bytecode-function
             m:*client* module nlocals nclosed entry-point size)))))

(defmethod %load-instruction ((mnemonic (eql 'make-bytecode-module)) stream)
  (let* ((len (read-ub32 stream))
         (bytecode (make-array len :element-type '(unsigned-byte 8)))
         ;; literals set by setf-literals
         (module (m:make-bytecode-module :bytecode bytecode)))
    (dbgprint " (make-bytecode-module ~d)" len)
    (read-sequence bytecode stream)
    (dbgprint "  bytecode:~{ ~2,'0x~}" (coerce bytecode 'list))
    (setf (next-constant) module)))

(defmethod %load-instruction ((mnemonic (eql 'setf-literals)) stream)
  (let* ((mod (constant (read-index stream)))
         (nlits (read-ub16 stream))
         (lits (make-array nlits)))
    (loop for i below nlits
          do (setf (aref lits i) (constant (read-index stream))))
    (dbgprint " (setf-literals ~s ~s)" mod lits)
    (setf (m:bytecode-module-literals mod) lits)))

(defmethod %load-instruction ((mnemonic (eql 'fdefinition)) stream)
  (let ((namei (read-index stream)))
    (dbgprint " (fdefinition ~d)" namei)
    (setf (next-constant) (fdefinition (constant namei)))))

(defmethod %load-instruction ((mnemonic (eql 'fcell)) stream)
  (let ((fnamei (read-index stream)))
    (dbgprint " (fcell ~d)" fnamei)
    (setf (next-constant)
          (m:link-function m:*client* *environment*
                           (constant fnamei)))))

(defmethod %load-instruction ((mnemonic (eql 'vcell)) stream)
  (let ((vnamei (read-index stream)))
    (dbgprint " (vcell ~d)" vnamei)
    (setf (next-constant)
          (m:link-variable m:*client* *environment*
                           (constant vnamei)))))

(defmethod %load-instruction ((mnemonic (eql 'environment)) stream)
  (dbgprint " (environment)")
  (setf (next-constant) (m:link-environment m:*client* *environment*)))

(defmethod %load-instruction ((mnemonic (eql 'funcall-create)) stream)
  (let ((funi (read-index stream))
        (args (loop repeat (read-ub16 stream)
                    collect (read-index stream))))
    (dbgprint " (funcall-create ~d~{ ~d~})" funi args)
    (setf (next-constant)
          (apply (constant funi) (mapcar #'constant args)))))

(defmethod %load-instruction ((mnemonic (eql 'funcall-initialize)) stream)
  (let ((funi (read-index stream))
        (args (loop repeat (read-ub16 stream)
                    collect (read-index stream))))
    (dbgprint " (funcall-initialize ~d~{ ~d~})" funi args)
    (dbgprint "  calling ~s" (constant funi))
    (apply (constant funi) (mapcar #'constant args))))

(defmethod %load-instruction ((mnemonic (eql 'find-class)) stream)
  (let ((cni (read-index stream)))
    (dbgprint " (find-class ~d)" cni)
    (setf (next-constant) (find-class (constant cni)))))

(defmethod %load-instruction ((mnemonic (eql 'init-object-array)) stream)
  (check-initialization)
  (let ((nobjs (read-ub64 stream)))
    (dbgprint " (init-object-array ~d)" nobjs)
    (setf *index-bytes* (max 1 (ash 1 (1- (ceiling (integer-length nobjs) 8))))
          *constants* (make-array nobjs)
	  *next-index* 0)))

(defun load-instruction (stream)
  (%load-instruction (read-mnemonic stream) stream))

(defparameter *attributes*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) 'name)
    (setf (gethash "docstring" ht) 'docstring)
    (setf (gethash "lambda-list" ht) 'lambda-list)
    ht))

(defgeneric %load-attribute (mnemonic stream))

(defun skip (nbytes stream)
  ;; FIXME: would file-position be better? Is it guaranteed to work here?
  (loop repeat nbytes do (read-byte stream)))

(defmethod %load-attribute ((mnemonic string) stream)
  (let ((nbytes (read-ub32 stream)))
    (dbgprint " (unknown-attribute ~s ~d)" mnemonic nbytes)
    (skip nbytes stream)))

(define-condition attribute-problem (style-warning)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)))

(define-condition bad-attribute-size (attribute-problem)
  ((%expected :initarg :expected :reader expected)
   (%actual :initarg :actual :reader actual))
  (:report (lambda (condition stream)
             (format stream "Malformed ~a attribute: Expected length ~d, got ~d. Ignoring."
                     (mnemonic condition)
                     (expected condition) (actual condition)))))

(defun check-attribute-size (mnemonic stream expected)
  (let ((nbytes (read-ub32 stream)))
    (cond ((= nbytes expected) t) ; no problem
          (t
           (warn 'bad-attribute-size
                 :mnemonic mnemonic :expected expected :actual nbytes)
           (skip nbytes stream)
           nil))))

(defmethod %load-attribute ((mnemonic (eql 'name)) stream)
  (when (check-attribute-size mnemonic stream (* 2 *index-bytes*))
    (let ((fun (constant (read-index stream)))
          (name (constant (read-index stream))))
      ;; constant closure would be weird, but we may as well support it.
      ;; anything else we silently ignore.
      (when (typep fun '(or m:bytecode-function m:bytecode-closure))
        (setf (m:bytecode-function-name fun) name)))))

(defmethod %load-attribute ((mnemonic (eql 'docstring)) stream)
  (when (check-attribute-size mnemonic stream (* 2 *index-bytes*))
    (let ((object (constant (read-index stream)))
          (doc (constant (read-index stream))))
      (typecase object
        ;; Stuff we can definitely call (setf documentation) on
        ;; without an error.
        ;; Anything else we silently ignore.
        ((or function method-combination standard-method package
             standard-class structure-class)
         (setf (documentation object t) doc))))))

(defmethod %load-attribute ((mnemonic (eql 'lambda-list)) stream)
  (when (check-attribute-size mnemonic stream (* 2 *index-bytes*))
    (let ((fun (constant (read-index stream)))
          (lambda-list (constant (read-index stream))))
      ;; constant closure would be weird, but we may as well support it.
      ;; anything else we silently ignore.
      (when (typep fun '(or m:bytecode-function m:bytecode-closure))
        (setf (m:bytecode-function-lambda-list fun) lambda-list)))))

(defun load-attribute (stream)
  (let ((aname (constant (read-index stream))))
    (%load-attribute (or (gethash aname *attributes*) aname) stream)))

(defmethod %load-instruction ((mnemonic (eql 'attribute)) stream)
  (load-attribute stream))

(defun load-bytecode-stream (stream
                             &key ((:environment *environment*))
			       ((:verbose *load-verbose*)
                                *load-verbose*))
  "As CL:LOAD, but only operates on bytecode FASLs, and has a stream as input rather than a file. The stream must be an (unsigned-byte 8) stream.
If :ENVIRONMENT is provided, it must be a runtime environment. The FASL is loaded into this environment."
  (load-magic stream)
  (multiple-value-bind (*major* *minor*) (load-version stream)
    (let* ((ninsts (read-ub64 stream))
           ;; Bind these, and also set them to empty so that if there's
           ;; an instruction that tries to set a constant before doing
           ;; init-object-array, we get a nice error.
           (*index-bytes* 0)
	   (*next-index* 0)
           (*constants* #()))
      (dbgprint "Executing FASL bytecode")
      (dbgprint "File reports ~d instructions" ninsts)
      (loop repeat ninsts
            do (load-instruction stream))
      ;; CLHS is sort of written like LISTEN only works on character
      ;; streams, but that would be a pointless restriction.
      ;; Clasp and SBCL at least allow it on byte streams.
      (when (listen stream)
        (error "Bytecode continues beyond end of instructions"))
      (check-initialization)))
  t)

(defun load-bytecode (filespec
                      &key
			environment
                        ((:verbose *load-verbose*) *load-verbose*)
                        ((:print *load-print*) *load-print*)
                        ((:debug *debug-loader*) *debug-loader*)
                        (if-does-not-exist :error)
                        (external-format :default))
  "As CL:LOAD, but only operates on bytecode FASLs. Load FILESPEC as a bytecode FASL.
If :ENVIRONMENT is provided, it must be a runtime environment. If not provided, an environment of NIL (meaning the host's) is used. The FASL is loaded into this environment."
  (let ((*load-pathname* (pathname (merge-pathnames filespec))))
    (with-open-file (input filespec :element-type '(unsigned-byte 8)
                                    :if-does-not-exist if-does-not-exist
                                    :external-format external-format)
      ;; check for :if-does-not-exist nil failure
      (unless input (return-from load-bytecode nil))
      (verboseprint "Loading ~a as FASL" filespec)
      (load-bytecode-stream input :environment environment)
      t)))
