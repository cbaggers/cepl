(in-package :cepl.c-arrays)

;;------------------------------------------------------------

(defun+ check-c-array-dimensions (dimensions total-size row-alignment)
  (labels ((valid-c-array-dimension-p (x)
             (typep x 'c-array-index)))
    (assert (member row-alignment '(1 2 4 8)) ()
            "c-arrays may their row alignment set to 1, 2, 4 or 8")
    (assert (and (> (length dimensions) 0) (<= (length dimensions) 4)) ()
            "c-arrays have a maximum of 4 dimensions: (attempted ~a)"
            (length dimensions))
    (assert (every #'valid-c-array-dimension-p dimensions) ()
            "Invalid dimensions for c-array ~a" dimensions)
    (assert (valid-c-array-dimension-p total-size) ()
            'c-array-total-size-type-error
            :size total-size :required-type 'c-array-index)))

(defun+ make-c-array-from-pointer (dimensions
                                   element-type
                                   pointer
                                   &key
                                   (free #'cffi:foreign-free)
                                   element-byte-size
                                   (row-alignment 1))
  (assert dimensions ()
          "dimensions are not optional when making an array from a pointer")
  (let* ((dimensions (listify dimensions))
         (total-size (reduce #'* dimensions)))
    (check-c-array-dimensions dimensions total-size row-alignment)
    (let* ((p-format (cepl.pixel-formats:pixel-format-p element-type))
           (element-type2 (if p-format
                              (pixel-format->lisp-type element-type)
                              element-type))
           (elem-size (or element-byte-size (gl-type-size element-type2)))
           (total-size-bytes
            (%gl-calc-byte-size elem-size dimensions row-alignment)))
      (%make-c-array
       :pointer pointer
       :dimensions dimensions
       :total-size total-size
       :byte-size total-size-bytes
       :element-type element-type2
       :sizes (gen-c-array-sizes dimensions
                                 elem-size
                                 row-alignment)
       :row-alignment row-alignment
       :struct-element-typep (symbol-names-cepl-structp element-type2)
       :element-pixel-format (when p-format element-type)
       :element-from-foreign (get-typed-from-foreign element-type2)
       :element-to-foreign (get-typed-to-foreign element-type2)
       :free free))))

;;------------------------------------------------------------

(defun gen-c-array-sizes (dimensions element-byte-size alignment)
  (destructuring-bind (row-len &optional (y 0) (z 0) &rest r) dimensions
    (declare (ignore r))
    (let* ((row-size (* row-len element-byte-size))
           (row+padding (* (ceiling row-size alignment) alignment))
           (square (* y row+padding))
           (cube (* z square)))
      (make-array 4 :element-type 'c-array-index
                  :initial-contents (list element-byte-size
                                          row+padding
                                          square
                                          cube)))))

;;------------------------------------------------------------

(defun forgiving-list-dimension-guess (list)
  (labels ((inner (x)
             (let ((1st (first x)))
               (when 1st
                 (cons (length x) (inner (when (listp 1st) 1st)))))))
    (inner list)))

;; [TODO] extract error messages
(defn make-c-array (initial-contents
                    &key
                    dimensions
                    element-type
                    (row-alignment t 1))
    c-array
  (make-c-array-internal initial-contents
                         dimensions
                         element-type
                         row-alignment
                         0))

(defn make-c-array-internal (initial-contents
                             dimensions
                             element-type
                             (row-alignment t 1)
                             (trailing-size c-array-index))
    c-array
  (let* ((p-format (cepl.pixel-formats:pixel-format-p element-type))
         (pixel-format (when p-format element-type))
         (element-type (if p-format
                           (pixel-format->lisp-type element-type)
                           element-type))
         (inferred-lisp-type (cond
                               (element-type nil)
                               (initial-contents
                                (scan-for-type initial-contents))
                               (t (error "If element-type is not specified the initial-contents must be provided"))))
         (element-type (if inferred-lisp-type
                           (second inferred-lisp-type)
                           element-type))
         (struct-type-p (symbol-names-cepl-structp element-type))
         (dimensions (listify dimensions))
         (dimensions
          (if dimensions
              (if initial-contents
                  (if (validate-dimensions initial-contents
                                           dimensions
                                           struct-type-p)
                      dimensions
                      (error "Dimensions are invalid for initial-contents~%~a~%~a"
                             dimensions initial-contents))
                  dimensions)
              (if initial-contents
                  (reverse
                   (etypecase initial-contents
                     (array
                      (if (array-has-fill-pointer-p initial-contents)
                          (list (fill-pointer initial-contents))
                          (array-dimensions initial-contents)))
                     (list
                      (let ((guess (forgiving-list-dimension-guess initial-contents)))
                        (if struct-type-p
                            (butlast guess)
                            guess)))))
                  (error "make-c-array must be given initial-elements or dimensions"))))
         (initial-contents (if inferred-lisp-type
                               (update-data initial-contents inferred-lisp-type)
                               initial-contents))
         (elem-size (gl-type-size element-type))
         (total-size (reduce #'* dimensions))
         (total-size-bytes
          (+ (%gl-calc-byte-size elem-size dimensions row-alignment)
             trailing-size)))
    (when (and initial-contents (not struct-type-p))
      (check-single-element-not-list initial-contents dimensions element-type))
    (check-c-array-dimensions dimensions total-size row-alignment)
    (let ((new-array (%make-c-array
                      :pointer (cffi::%foreign-alloc total-size-bytes)
                      :dimensions dimensions
                      :total-size total-size
                      :byte-size total-size-bytes
                      :sizes (gen-c-array-sizes dimensions
                                                elem-size
                                                row-alignment)
                      :row-alignment row-alignment
                      :element-type element-type
                      :struct-element-typep struct-type-p
                      :element-pixel-format pixel-format
                      :element-from-foreign (get-typed-from-foreign
                                             element-type)
                      :element-to-foreign (get-typed-to-foreign
                                           element-type))))
      (when initial-contents
        (copy-lisp-data-to-c-array new-array initial-contents nil))
      new-array)))

(defun check-single-element-not-list (initial-contents dimensions element-type)
  (labels ((check-arr ()
             (row-major-aref initial-contents 0))
           (check-list ()
             (loop
                :for i :below (length dimensions)
                :with curr := initial-contents
                :do (setf curr (first curr))
                :finally (return curr))))
    (let ((elem (etypecase initial-contents
                  (array (check-arr))
                  (list (check-list)))))
      (assert (not (listp elem)) () 'bad-c-array-element
              :incorrect-type :list
              :correct-type element-type
              :elem elem
              :initial-contents initial-contents
              :extra-info-string nil)
      (assert (not (symbolp elem)) () 'bad-c-array-element
              :incorrect-type :symbol
              :correct-type element-type
              :elem elem
              :initial-contents initial-contents
              :extra-info-string "Perhaps a misplaced QUOTE?")
      (not (symbolp elem)))))

;;------------------------------------------------------------

(defn clone-c-array ((c-array c-array))
    c-array
  (let* ((size (c-array-byte-size c-array))
         (new-pointer (cffi::%foreign-alloc size)))
    (cepl.types::%memcpy new-pointer (c-array-pointer c-array) size)
    (%make-c-array
     :pointer new-pointer
     :dimensions (c-array-dimensions c-array)
     :total-size (c-array-total-size c-array)
     :byte-size (c-array-byte-size c-array)
     :element-type (c-array-element-type c-array)
     :sizes (make-array 4 :element-type 'c-array-index
                        :initial-contents (c-array-sizes c-array))
     :row-alignment (c-array-row-alignment c-array)
     :struct-element-typep (c-array-struct-element-typep c-array)
     :element-from-foreign (c-array-element-from-foreign c-array)
     :element-to-foreign (c-array-element-to-foreign c-array))))

(defmacro with-c-array-freed ((var-name c-array) &body body)
  `(let* ((,var-name ,c-array))
     (release-unwind-protect (progn ,@body) (free-c-array ,var-name))))

(defmacro with-c-arrays-freed ((var-name c-arrays) &body body)
  `(let* ((,var-name ,c-arrays))
     (release-unwind-protect (progn ,@body)
       (loop :for a :in ,var-name :do (free-c-array a)))))

;;------------------------------------------------------------

(defun first-elem (x)
  (if x
      (if (listp x)
          (first-elem (first x))
          x)
      (error "Empty list found in data")))

;; ideally we would use a generic reduce in this case to handle the different
;; kinds of structures, but alas this is not available.
(defun+ scan-for-type (data)
  (typecase data
    ((or array vector) (scan-array-for-type data))
    (list (scan-list-for-type data))
    (t (error "Can not infer the type the c-array should be unless it is a vector, array or flat list"))))

(defun scan-array-for-type (data)
  (let ((initial-set (find-suitable-type (row-major-aref data 0))))
    (loop
       :for i :below (array-total-size data)
       :with x := initial-set
       :for elem := (row-major-aref data i)
       :do (setf x (find-compatible-c-array-type x elem))
       :finally (return (subseq x 0 2)))))

(defun scan-list-for-type (data)
  (let* ((initial-set (find-suitable-type (first-elem data)))
         (current-set initial-set))
    (labels ((scan (x)
               (if x
                   (if (listp x)
                       (map nil #'scan x)
                       (setf current-set
                             (find-compatible-c-array-type current-set x)))
                   (error "found null list in data"))))
      (scan data))
    (subseq current-set 0 2)))

(defun+ update-data (data type)
  (if (or (eq type 'single-float) (eq type 'double-float))
      (typecase data
        ((or array vector)
         (let ((new-data (make-array (array-dimensions data)
                                     :element-type (array-element-type data))))
           (loop :for i :below (array-total-size data) :do
              (setf (row-major-aref new-data i)
                    (coerce (row-major-aref data i) type))
              :finally (return (values new-data t)))))
        (list (values (mapcar (lambda (x) (coerce x type)) data) t))
        (t (error "Can not infer the type the c-array should be unless it is a vector, array or flat list")))
      (values data nil)))


(defun find-compatible-c-array-type (current-set data)
  (unless current-set
    (error "Cannot unambiguously determine the type of the data. Please use the :element-type option"))
  (if (typep data (first current-set))
      current-set
      (destructuring-bind (type-spec gl-type casts) current-set
        (declare (ignore type-spec gl-type))
        (or (find-if (lambda (x) (typep data x)) casts)
            (destructuring-bind (type-spec gl-type casts)
                (find-suitable-type data)
              (declare (ignore type-spec gl-type))
              (find current-set casts :test #'equal))
            (error "Types in source data are inconsistent")))))

(defun find-suitable-type (datum)
  (typecase datum
    (structure-object
     (let ((type (type-of datum)))
       (list type type nil)))
    ((integer 0 256)
     '((integer 0 256) :uint8 ((integer -127 128)
                               fixnum
                               single-float
                               double-float)))
    ((integer -127 128)
     '((integer -127 128) :int8 ((integer 0 256)
                                 fixnum
                                 single-float
                                 double-float)))
    (fixnum
     '(fixnum :int (single-float double-float)))
    (single-float
     '(single-float :float (double-float)))
    (double-float
     '(double-float :double nil))
    ((array (unsigned-byte 8) (2))
     '((array (unsigned-byte 8) (2)) :uint8-vec2 nil))
    ((array (unsigned-byte 8) (3))
     '((array (unsigned-byte 8) (3)) :uint8-vec3 nil))
    ((array (unsigned-byte 8) (4))
     '((array (unsigned-byte 8) (4)) :uint8-vec4 nil))
    ((array single-float (2))
     '((array single-float (2)) :vec2 nil))
    ((array single-float (3))
     '((array single-float (3)) :vec3 nil))
    ((array single-float (4))
     '((array single-float (4)) :vec4 nil))))
