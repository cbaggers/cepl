(in-package :cepl.c-arrays)

;;------------------------------------------------------------

(defun+ valid-c-array-dimension-p (x)
  (>= x 0))

(defun+ make-c-array-from-pointer (dimensions
                                   element-type
                                   pointer
                                   &key
                                   (free #'cffi:foreign-free)
                                   element-byte-size)
  (assert dimensions ()
          "dimensions are not optional when making an array from a pointer")
  (let ((dimensions (listify dimensions)))
    (assert (every #'valid-c-array-dimension-p dimensions) ()
            "Invalid dimensions for c-array ~a" dimensions)
    (let* ((p-format (cepl.pixel-formats:pixel-format-p element-type))
           (element-type2 (if p-format
                              (pixel-format->lisp-type element-type)
                              element-type))
           (elem-size (or element-byte-size (gl-type-size element-type2))))
      (multiple-value-bind (byte-size row-byte-size)
          (%gl-calc-byte-size elem-size dimensions)
        (declare (ignore byte-size))
        (%make-c-array
         :pointer pointer
         :dimensions dimensions
         :element-type element-type2
         :element-byte-size elem-size
         :struct-element-typep (symbol-names-cepl-structp element-type2)
         :row-byte-size row-byte-size
         :element-pixel-format (when p-format element-type)
         :element-from-foreign (get-typed-from-foreign element-type2)
         :element-to-foreign (get-typed-to-foreign element-type2)
         :free free)))))

;;------------------------------------------------------------

;; [TODO] extract error messages
(defun+ make-c-array (initial-contents &key dimensions element-type)
  (let* ((dimensions (listify dimensions))
         (dimensions
          (if dimensions
              (if initial-contents
                  (or (validate-dimensions initial-contents dimensions)
                      (error "Dimensions are invalid for initial-contents~%~a~%~a"
                             dimensions initial-contents))
                  dimensions)
              (if initial-contents
                  (typecase initial-contents
                    (sequence (list (length initial-contents)))
                    (array (array-dimensions initial-contents)))
                  (error "make-c-array must be given initial-elements or dimensions"))))
         (p-format (cepl.pixel-formats:pixel-format-p element-type))
         (pixel-format (when p-format element-type))
         (element-type (if p-format
                           (pixel-format->lisp-type element-type)
                           element-type))
         (inferred-lisp-type (cond (element-type nil)
                                   (initial-contents (scan-for-type
                                                      initial-contents))
                                   (t (error "If element-type is not specified the initial-contents must be provided"))))
         (element-type (if inferred-lisp-type
                           (lisp->gl-type inferred-lisp-type)
                           element-type))
         (initial-contents (if inferred-lisp-type
                               (update-data initial-contents inferred-lisp-type)
                               initial-contents))
         (elem-size (gl-type-size element-type)))
    (assert (every #'indexp dimensions) ()
            "Invalid dimensions ~a" dimensions)
    (when (> (length dimensions) 4)
      (error "c-arrays have a maximum of 4 dimensions: (attempted ~a)"
             (length dimensions)))
    (when (not (loop for i in dimensions always (> i 0)))
      (error "all dimensions must be >=1 in length"))
    (multiple-value-bind (byte-size row-byte-size)
        (%gl-calc-byte-size elem-size dimensions)
      (let ((new-array (%make-c-array
                        :pointer (cffi::%foreign-alloc byte-size)
                        :dimensions dimensions
                        :element-byte-size elem-size
                        :element-type element-type
                        :struct-element-typep (symbol-names-cepl-structp
                                               element-type)
                        :row-byte-size row-byte-size
                        :element-pixel-format pixel-format
                        :element-from-foreign (get-typed-from-foreign
                                               element-type)
                        :element-to-foreign (get-typed-to-foreign
                                             element-type))))
        (when initial-contents
          (c-populate new-array initial-contents nil))
        new-array))))

;;------------------------------------------------------------

(defun+ clone-c-array (c-array)
  (let* ((size (c-array-byte-size c-array))
         (new-pointer (cffi::%foreign-alloc size)))
    (cepl.types::%memcpy new-pointer (c-array-pointer c-array) size)
    (%make-c-array
     :pointer new-pointer
     :dimensions (c-array-dimensions c-array)
     :element-byte-size (c-array-element-byte-size c-array)
     :element-type (c-array-element-type c-array)
     :struct-element-typep (c-array-struct-element-typep c-array)
     :row-byte-size (c-array-row-byte-size c-array)
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

;; ideally we would use a generic reduce in this case to handle the different
;; kinds of structures, but alas this is not available.
(defun+ scan-for-type (data)
  (typecase data
    ((or array vector)
     (let ((initial-type (first (find-suitable-type (row-major-aref data 0)))))
       (loop for i below (array-total-size data)
          :with x = initial-type :do
            (setf x (find-compatible-c-array-type x (row-major-aref data i)))
          :finally (return (values x (equal x initial-type))))))
    (list (let* ((initial-type (first (find-suitable-type (first data))))
                 (tmp (reduce #'find-compatible-c-array-type data :initial-value initial-type)))
            (values tmp (equal tmp initial-type))))
    (t (error "Can not infer the type the c-array should be unless it is a vector, array or flat list"))))

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

(let ((states `(((integer 0 256) :uint8
                 ((integer -127 128) fixnum single-float double-float))
                ((integer -127 128) :int8
                 ((integer 0 256) fixnum single-float double-float))
                (fixnum :int (single-float double-float))
                (single-float :float (double-float))
                (double-float :double nil))))
  (defun find-compatible-c-array-type (current-type data)
    (unless current-type
      (error "Cannot unambiguously determine the type of the data. Please use the :element-type option"))
    (if (typep data current-type)
        current-type
        (let ((c (find current-type states :key #'first :test #'equal)))
          (if c
              (destructuring-bind (type-spec gl-type casts) c
                (declare (ignore type-spec gl-type))
                (or (find-if (lambda (x) (typep data x)) casts)
                    (let ((data-type (find-suitable-type data)))
                      (destructuring-bind (type-spec gl-type casts) data-type
                        (declare (ignore type-spec gl-type))
                        (find current-type casts :test #'equal)))
                    (error "Types in source data are inconsistent")))
              (error "Types in source data are inconsistent")))))
  (defun find-suitable-type (datum)
    (if (typep datum 'structure-object)
        (list (type-of datum) (type-of datum) nil)
        (find-if (lambda (x) (typep datum x)) states :key #'first)))
  (defun lisp->gl-type (x)
    (second (find x states :key #'first :test #'equal))))
