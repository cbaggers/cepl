(in-package :cepl.c-arrays)

;;------------------------------------------------------------

(defun+ check-c-array-dimensions (dimensions total-size)
  (labels ((valid-c-array-dimension-p (x)
             (typep x 'c-array-index)))
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
                                   element-byte-size)
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (assert dimensions ()
          "dimensions are not optional when making an array from a pointer")
  (let ((dimensions (listify dimensions))
        (total-size (reduce #'* dimensions)))
    (check-c-array-dimensions dimensions total-size)
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
         :total-size total-size
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
  (when initial-contents
    (check-type initial-contents array))
  (let* ((dimensions (listify dimensions))
         (dimensions
          (if dimensions
              (if initial-contents
                  (assert (validate-dimensions initial-contents dimensions) ()
                          "Dimensions are invalid for initial-contents~%~a~%~a"
                          dimensions initial-contents)
                  dimensions)
              (if initial-contents
                  (array-dimensions initial-contents)
                  (error "make-c-array must be given initial-elements or dimensions"))))
         (p-format (cepl.pixel-formats:pixel-format-p element-type))
         (pixel-format (when p-format element-type))
         (element-type (if p-format
                           (pixel-format->lisp-type element-type)
                           element-type))
         (inferred-element-type (cond (element-type nil)
                                      (initial-contents (scan-for-type
                                                         initial-contents))
                                      (t (error "If element-type is not specified the initial-contents must be provided"))))
         (element-type (if inferred-element-type
                           inferred-element-type
                           element-type))
         (elem-size (gl-type-size element-type))
         (total-size (reduce #'* dimensions)))
    (check-c-array-dimensions dimensions total-size)
    (multiple-value-bind (byte-size row-byte-size)
        (%gl-calc-byte-size elem-size dimensions)
      (let ((new-array (%make-c-array
                        :pointer (cffi::%foreign-alloc byte-size)
                        :dimensions dimensions
                        :total-size total-size
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
          (c-populate new-array initial-contents))
        new-array))))

;;------------------------------------------------------------

(defun+ clone-c-array (c-array)
  (let* ((size (c-array-byte-size c-array))
         (new-pointer (cffi::%foreign-alloc size)))
    (cepl.types::%memcpy new-pointer (c-array-pointer c-array) size)
    (%make-c-array
     :pointer new-pointer
     :dimensions (c-array-dimensions c-array)
     :total-size (c-array-total-size c-array)
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

(defun scan-for-type (data)
  (when (> (array-rank data) 0)
    (if (not (eq (array-element-type data) t))
        (cepl.types::gl-type-for-lisp-data (array-element-type data))
        (let* ((curr (row-major-aref data 0))
               (type (type-of curr))
               (gl-type (cepl.types::gl-type-for-lisp-data curr)))
          (loop :for i :below (array-total-size data)
             :for elem := (row-major-aref data i)
             :do (unless (typep elem type)
                   (let ((glwip (cepl.types::gl-type-for-lisp-data elem))
                         (twip (type-of elem)))
                     (if (typep curr twip)
                         (setf type twip
                               gl-type glwip)
                         (error "Types in source data are inconsistent")))))
          gl-type))))
