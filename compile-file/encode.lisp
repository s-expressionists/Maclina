(in-package #:maclina.compile-file)

;;; WRITE-BYTECODE will, given a list of instructions and a stream, write out
;;; the entire FASL.

;;; Write out a FASL instruction to a ub8 stream.
(defgeneric encode (instruction stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bytecode
;;;
;;; "bytecode" is actually a little strong. This "bytecode" consists of a
;;; sequence of "instructions" that must be executed sequentially.
;;; There's no other control flow. There is no data structure involved other
;;; than the array of constants being produced (so e.g. no operand stack).
;;; All multibyte values are big-endian. All indices are one byte, or two
;;; bytes, or etc. powers of two based on how many constants there are. E.g. if
;;; there are 200 constants indices will be one byte, but if there are 300
;;; indices will be two bytes.
;;; Instruction set is copied from Clasp for now. "sind" in the below means an
;;; index that the allocated object will be stored into. This may need some
;;; review later.
;;; Operations are as follows:

(defparameter +ops+
  '((nil 65 sind)
    (t 66 sind)
    (ratio 67)
    (complex 68)
    (cons 69 sind)
    (initialize-cons 70 consind carind cdrind)
    (base-string 72 size . data)
    (utf8-string 73 nbytes . data)
    (make-array 74 sind rank . dims)
    (initialize-array 75 arrayind . valueinds)
    (make-hash-table 76 sind test count)
    (initialize-hash-table 77 htind keyind valueind)
    (make-sb64 78 sind sb64)
    (find-package 79 sind nameind)
    (make-bignum 80 sind size . words)
    (make-symbol 81)
    (intern 82 sind packageind nameind)
    (make-character 83 sind ub32)
    (make-pathname 85)
    (make-bytecode-function 87)
    (make-bytecode-module 88)
    (setf-literals 89)
    (make-single-float 90 sind ub32)
    (make-double-float 91 sind ub64)
    (funcall-create 93 sind find nargs . args)
    (funcall-initialize 94 find nargs . args)
    (fdefinition 95 find nameind)
    (fcell 96 find nameind)
    (vcell 97 vind nameind)
    (find-class 98 sind cnind)
    (init-object-array 99 ub64)
    (environment 100)
    (fcell-set 101 nameind)
    (vcell-set 102 nameind)
    (ccell-set 103 nameind)
    (attribute 255 name nbytes . data)))

;; how many bytes are needed to represent an index?
(defvar *index-bytes*)

;;; Write an n-byte integer to a ub8 stream, big-endian.
(defun write-b (int n stream)
  ;; write-sequence is better for this, but I don't think we can really
  ;; use it without consing or touching memory generally.
  (loop for i from (* (1- n) 8) downto 0 by 8
        for byte = (ldb (byte 8 i) int)
        do (write-byte byte stream)))

(defun write-b64 (word stream) (write-b word 8 stream))
(defun write-b32 (word stream) (write-b word 4 stream))
(defun write-b16 (word stream) (write-b word 2 stream))

(defun write-magic (stream) (write-b32 +magic+ stream))

(defun write-version (stream)
  (write-b16 *major-version* stream)
  (write-b16 *minor-version* stream))

;; Used in disltv as well.
(defun write-bytecode (instructions stream)
  (let* ((nobjs (count-if (lambda (i) (typep i 'creator)) instructions))
         ;; Next highest power of two bytes, roughly
         (*index-bytes* (ash 1 (1- (ceiling (integer-length nobjs) 8))))
         ;; 1+ for the init-object-array.
         (ninsts (1+ (length instructions))))
    (assign-indices instructions)
    (dbgprint "Instructions:~{~&~a~}" instructions)
    (write-magic stream)
    (write-version stream)
    (write-b64 ninsts stream)
    (encode (make-instance 'init-object-array :count nobjs) stream)
    (map nil (lambda (inst) (encode inst stream)) instructions)))

(defun opcode (mnemonic)
  (let ((inst (assoc mnemonic +ops+ :test #'equal)))
    (if inst
        (second inst)
        (error "unknown mnemonic ~a" mnemonic))))

(defun write-mnemonic (mnemonic stream) (write-byte (opcode mnemonic) stream))

(defun write-index (creator stream)
  (let ((position (index creator)))
    (ecase *index-bytes*
      ((1) (write-byte position stream))
      ((2) (write-b16 position stream))
      ((4) (write-b32 position stream))
      ((8) (write-b64 position stream)))))

(defmethod encode ((inst cons-creator) stream)
  (write-mnemonic 'cons stream))

(defmethod encode ((inst initialize-cons) stream)
  (write-mnemonic 'initialize-cons stream)
  (write-index (rplac-cons inst) stream)
  (write-index (rplac-car inst) stream)
  (write-index (rplac-cdr inst) stream))

(defun write-dimensions (dimensions stream)
  (let ((rank (length dimensions)))
    (unless (< rank 256)
      (error "Can't dump an array of ~d dimensions" rank))
    (write-byte rank stream))
  ;; Only two bytes for now. Might want different opcodes for larger
  ;; (or smaller?) dimensions.
  (unless (< (reduce #'* dimensions) 65536)
    (error "Can't dump an array with ~d elements" (reduce #'* dimensions)))
  (dolist (dim dimensions)
    (write-b16 dim stream)))

(defmacro write-sub-byte (array stream nbits)
  (let ((perbyte (floor 8 nbits))
        (a (gensym "ARRAY")) (s (gensym "STREAM")))
    `(let* ((,a ,array) (,s ,stream) (total-size (array-total-size ,a)))
       (multiple-value-bind (full-bytes remainder) (floor total-size ,perbyte)
         (loop for byteindex below full-bytes
               for index = (* ,perbyte byteindex)
               for byte = (logior
                           ,@(loop for i below perbyte
                                   for shift = (- 8 (* i nbits) nbits)
                                   for rma = `(row-major-aref ,a (+ index ,i))
                                   collect `(ash ,rma ,shift)))
               do (write-byte byte ,s))
         ;; write remainder
         (unless (zerop remainder)
           (let* ((index (* ,perbyte full-bytes))
                  (byte 0))
             (loop for i below remainder
                   for shift = (- 8 (* i ,nbits) ,nbits)
                   for rma = (row-major-aref ,a (+ index i))
                   do (setf (ldb (byte ,nbits shift) byte) rma))
             (write-byte byte ,s)))))))

(defun write-utf8 (character-array stream)
  (declare (optimize speed) (type (array character) character-array))
  ;; WARNING: We assume the host lisp's CHAR-CODE returns the Unicode codepoint.
  ;; This is the case on anything sane, probably. Babel makes the same assumption
  ;; as far as I can tell.
  ;; TODO: There's probably cleverness to be done to make this write faster.
  (loop for i below (array-total-size character-array)
	for char = (row-major-aref character-array i)
	for cpoint = (char-code char)
	do (cond ((< cpoint #x80) ; one byte
		  (write-byte cpoint stream))
		 ((< cpoint #x800) ; two
		  (write-byte (logior #b11000000 (ldb (byte 5  6) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
		 ((< cpoint #x10000) ; three
		  (write-byte (logior #b11100000 (ldb (byte 4 12) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6  6) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
		 ((< cpoint #x110000) ; four
		  (write-byte (logior #b11110000 (ldb (byte 3 18) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6 12) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6  6) cpoint)) stream)
		  (write-byte (logior #b10000000 (ldb (byte 6  0) cpoint)) stream))
		 ;; The following is deleted as unreachable on e.g. SBCL because
		 ;; it knows that char-code doesn't go this high.
		 ;; Don't worry about it.
                 #-sbcl
		 (t ; not allowed by RFC3629
		  (error "Code point #x~x for character ~:c is out of range for UTF-8"
			 cpoint char)))))

(defmethod encode ((inst array-creator) stream)
  (write-mnemonic 'make-array stream)
  (let* ((et-info (element-type-info inst))
         (packing-info (packing-info inst))
         (dims (dimensions inst))
         (packing-type (first packing-info))
         (packing-code (second packing-info)))
    (cond ((consp et-info) ;; (element-type-creator +other-uaet+)
           (write-byte (second et-info) stream)
           (write-index (first et-info) stream))
          (t (write-byte et-info stream)))
    (write-byte packing-code stream)
    (write-dimensions dims stream)
    (macrolet ((dump (&rest forms)
                 `(loop with arr = (prototype inst)
                        for i below (array-total-size arr)
                        for elem = (row-major-aref arr i)
                        do ,@forms)))
      (cond ((equal packing-type :nil)) ; just need dims
            ((equal packing-type :base-char)
             (dump (write-byte (char-code elem) stream)))
            ((equal packing-type :character)
	     (write-utf8 (prototype inst) stream))
            ((equal packing-type :binary32)
             (dump (write-b32 (ieee-floats:encode-float32 elem) stream)))
            ((equal packing-type :binary64)
             (dump (write-b64 (ieee-floats:encode-float64 elem) stream)))
            ((equal packing-type :complex-binary32)
             (dump (write-b32 (ieee-floats:encode-float32 (realpart elem))
                              stream)
                   (write-b32 (ieee-floats:encode-float32 (imagpart elem))
                              stream)))
            ((equal packing-type :complex-binary64)
             (dump (write-b64 (ieee-floats:encode-float64 (realpart elem))
                              stream)
                   (write-b64 (ieee-floats:encode-float64 (imagpart elem))
                              stream)))
            ((equal packing-type :unsigned-byte1)
             (write-sub-byte (prototype inst) stream 1))
            ((equal packing-type :unsigned-byte2)
             (write-sub-byte (prototype inst) stream 2))
            ((equal packing-type :unsigned-byte4)
             (write-sub-byte (prototype inst) stream 4))
            ((equal packing-type :unsigned-byte8)
             (write-sequence (prototype inst) stream))
            ((equal packing-type :unsigned-byte16)
             (dump (write-b16 elem stream)))
            ((equal packing-type :unsigned-byte32)
             (dump (write-b32 elem stream)))
            ((equal packing-type :unsigned-byte64)
             (dump (write-b64 elem stream)))
            ((equal packing-type :signed-byte8)
             (dump (write-byte (ldb (byte 8 0) elem) stream)))
            ((equal packing-type :signed-byte16)
             (dump (write-b16 elem stream)))
            ((equal packing-type :signed-byte32)
             (dump (write-b32 elem stream)))
            ((equal packing-type :signed-byte64)
             (dump (write-b64 elem stream)))
            ;; TODO: Signed bytes
            ((equal packing-type :t)) ; handled by initialize-array instruction
            (t (error "BUG: Unknown packing-type ~s" packing-type))))))

(defmethod encode ((inst initialize-array) stream)
  (write-mnemonic 'initialize-array stream)
  (write-index (initialized-array inst) stream)
  ;; length is implicit from the array being initialized
  (loop for c in (array-values inst)
        do (write-index c stream)))

(defmethod encode ((inst base-string-creator) stream)
  (write-mnemonic 'base-string stream)
  (write-b16 (length (prototype inst)) stream)
  (loop for c across (prototype inst)
        for code = (char-code c)
        do (write-byte code stream)))

;;; Here we encode the number of bytes rather than the number of chars.
;;; This is smarter, since it means the I/O can be batched. We should
;;; do it for general arrays as well.
(defmethod encode ((inst utf8-string-creator) stream)
  (write-mnemonic 'utf8-string stream)
  (write-b16 (nbytes inst) stream)
  (write-utf8 (prototype inst) stream))

(defmethod encode ((inst hash-table-creator) stream)
  (let* ((ht (prototype inst))
         ;; TODO: Custom hash-table tests.
         ;; NOTE that for non-custom hash table tests, the standard
         ;; guarantees that hash-table-test returns a symbol.
         (testcode (ecase (hash-table-test ht)
                     ((eq) #b00)
                     ((eql) #b01)
                     ((equal) #b10)
                     ((equalp) #b11)))
         ;; For now, only allow counts up to #xffff.
         ;; Since the count is just a hint, bigger hash tables can still
         ;; be dumped okay.
         ;; efficiency NOTE: The size passed to make-hash-table really
         ;; specifies a capacity, so for example if we have an HT with 56
         ;; entries, make a 56-entry similar hash table, and start filling it
         ;; up, it might be rehashed and resized during initialization as it
         ;; reaches the rehash threshold. I am not sure how to deal with this
         ;; in a portable fashion. (we could just invert a provided rehash-size?)
         (count (max (hash-table-count ht) #xffff)))
    (write-mnemonic 'make-hash-table stream)
    (write-byte testcode stream)
    (write-b16 count stream)))

(defmethod encode ((inst initialize-hash-table) stream)
  (write-mnemonic 'initialize-hash-table stream)
  (write-index (initialized-table inst) stream)
  (write-b32 (initialized-table-count inst) stream)
  (loop for (k . v) in (alist inst)
        do (write-index k stream)
           (write-index v stream)))

(defmethod encode ((inst singleton-creator) stream)
  (ecase (prototype inst)
    ((nil) (write-mnemonic 'nil stream))
    ((t) (write-mnemonic 't stream))))

(defmethod encode ((inst symbol-creator) stream)
  (write-mnemonic 'make-symbol stream)
  (write-index (symbol-creator-name inst) stream))

(defmethod encode ((inst interned-symbol-creator) stream)
  (write-mnemonic 'intern stream)
  (write-index (symbol-creator-package inst) stream)
  (write-index (symbol-creator-name inst) stream))

(defmethod encode ((inst package-creator) stream)
  (write-mnemonic 'find-package stream)
  (write-index (package-creator-name inst) stream))

(defmethod encode ((inst character-creator) stream)
  (write-mnemonic 'make-character stream)
  (write-b32 (char-code (prototype inst)) stream))

(defmethod encode ((inst pathname-creator) stream)
  (write-mnemonic 'make-pathname stream)
  (write-index (pathname-creator-host inst) stream)
  (write-index (pathname-creator-device inst) stream)
  (write-index (pathname-creator-directory inst) stream)
  (write-index (pathname-creator-name inst) stream)
  (write-index (pathname-creator-type inst) stream)
  (write-index (pathname-creator-version inst) stream))

(defmethod encode ((inst sb64-creator) stream)
  (write-mnemonic 'make-sb64 stream)
  (write-b64 (prototype inst) stream))

(defmethod encode ((inst bignum-creator) stream)
  ;; uses sign-magnitude representation.
  (write-mnemonic 'make-bignum stream)
  (let* ((number (prototype inst))
         (anumber (abs number))
         (nwords (ceiling (integer-length anumber) 64))
         (negp (minusp number)))
    (write-b64 (if negp (- nwords) nwords) stream)
    (loop for i from nwords above 0
          for pos = (* (1- i) 64)
          for word = (ldb (byte 64 pos) anumber)
          do (write-b64 word stream))))

(defmethod encode ((inst single-float-creator) stream)
  (write-mnemonic 'make-single-float stream)
  (write-b32 (ieee-floats:encode-float32 (prototype inst)) stream))

(defmethod encode ((inst double-float-creator) stream)
  (write-mnemonic 'make-double-float stream)
  (write-b64 (ieee-floats:encode-float64 (prototype inst)) stream))

(defmethod encode ((inst ratio-creator) stream)
  (write-mnemonic 'ratio stream)
  (write-index (ratio-creator-numerator inst) stream)
  (write-index (ratio-creator-denominator inst) stream))

(defmethod encode ((inst complex-creator) stream)
  (write-mnemonic 'complex stream)
  (write-index (complex-creator-realpart inst) stream)
  (write-index (complex-creator-imagpart inst) stream))

(defmethod encode ((inst fdefinition-lookup) stream)
  (write-mnemonic 'fdefinition stream)
  (write-index (name inst) stream))

(defmethod encode ((inst fcell-lookup) stream)
  (write-mnemonic 'fcell stream)
  (write-index (name inst) stream))

(defmethod encode ((inst fcell-set) stream)
  (write-mnemonic 'fcell-set stream)
  (write-index (fcell inst) stream)
  (write-index (value inst) stream))

(defmethod encode ((inst vcell-lookup) stream)
  (write-mnemonic 'vcell stream)
  (write-index (name inst) stream))

(defmethod encode ((inst environment-lookup) stream)
  (write-mnemonic 'environment stream))

(defmethod encode ((inst general-creator) stream)
  (write-mnemonic 'funcall-create stream)
  (write-index (general-function inst) stream)
  (write-b16 (length (general-arguments inst)) stream)
  (loop for arg in (general-arguments inst)
        do (write-index arg stream)))

(defmethod encode ((inst general-initializer) stream)
  (write-mnemonic 'funcall-initialize stream)
  (write-index (general-function inst) stream)
  (write-b16 (length (general-arguments inst)) stream)
  (loop for arg in (general-arguments inst)
        do (write-index arg stream)))

(defmethod encode ((inst class-creator) stream)
  (write-mnemonic 'find-class stream)
  (write-index (class-creator-name inst) stream))

(defmethod encode ((inst load-time-value-creator) stream)
  (write-mnemonic 'funcall-create stream)
  (write-index (load-time-value-creator-function inst) stream)
  ;; no arguments
  (write-b16 0 stream))

(defmethod encode ((inst bytefunction-creator) stream)
  (write-mnemonic 'make-bytecode-function stream)
  (write-b32 (entry-point inst) stream)
  (write-b32 (size inst) stream)
  (write-b16 (nlocals inst) stream)
  (write-b16 (nclosed inst) stream)
  (write-index (module inst) stream))

(defmethod encode ((inst bytemodule-creator) stream)
  ;; Write instructions.
  (write-mnemonic 'make-bytecode-module stream)
  (let* ((lispcode (bytemodule-lispcode inst))
         (len (length lispcode)))
    (when (> len #.(ash 1 32))
      (error "Bytecode length is ~d, too long to dump" len))
    (write-b32 len stream)
    (write-sequence lispcode stream)))

(defmethod encode ((inst setf-literals) stream)
  (write-mnemonic 'setf-literals stream)
  (write-index (setf-literals-module inst) stream)
  (let ((literals (setf-literals-literals inst)))
    (write-b16 (length literals) stream)
    (loop for creator across literals
          do (write-index creator stream))))

;;;

(defmethod encode :before ((attr attribute) stream)
  (write-mnemonic 'attribute stream)
  (write-index (name attr) stream))  

(defmethod encode ((attr docstring-attr) stream)
  ;; Write the length.
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  ;; And the data.
  (write-index (object attr) stream)
  (write-index (docstring attr) stream))

(defmethod encode ((attr name-attr) stream)
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  (write-index (object attr) stream)
  (write-index (objname attr) stream))

(defmethod encode ((attr lambda-list-attr) stream)
  (write-b32 (+ *index-bytes* *index-bytes*) stream)
  (write-index (ll-function attr) stream)
  (write-index (lambda-list attr) stream))

;;;

(defmethod encode ((init init-object-array) stream)
  (write-mnemonic 'init-object-array stream)
  (write-b64 (init-object-array-count init) stream))
