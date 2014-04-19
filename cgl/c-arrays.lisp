(in-package :cgl)

;; [TODO] Find anything that uses hidden symbols (::) and justify that usage or 
;;        fix it

;;------------------------------------------------------------
(defclass c-array ()
  ((pointer :initarg :pointer :reader pointer)
   (dimensions :initarg :dimensions :reader dimensions)
   (element-type :initarg :element-type :reader element-type)
   (element-byte-size :initarg :element-byte-size :reader element-byte-size)
   (row-byte-size :initarg :row-byte-size :reader row-byte-size)
   (row-alignment :initarg :row-alignment :reader row-alignment)
   (element-pixel-format :initform nil :initarg :element-pixel-format
                         :reader element-pixel-format)))

(defun blank-c-array-object (c-array)
  (with-slots (pointer dimensions element-type element-byte-size row-byte-size 
                       row-alignment element-pixel-format) c-array
    (setf pointer nil
          dimensions nil
          element-type nil
          element-byte-size nil
          row-byte-size nil
          row-alignment nil
          element-pixel-format nil)))

(defmethod gl-free ((object c-array))
  (free-c-array object))

(defun free-c-array (c-array)
  "Frees the specified c-array."
  (foreign-free (pointer c-array))
  (blank-c-array-object c-array))

(defmethod print-object ((object c-array) stream)
  (format stream "#<C-ARRAY :element-type ~s :dimensions ~a>"
          (slot-value object 'element-type)
          (slot-value object 'dimensions)))

;;------------------------------------------------------------

(defun c-array-byte-size (c-array)
  (%gl-calc-byte-size (element-byte-size c-array)
                      (dimensions c-array)
                      (row-alignment c-array)))

(defun %gl-calc-byte-size (elem-size dimensions &optional (alignment 1))
  (let* ((x-size (first dimensions)) (rest (rest dimensions)))
    (let* ((row-raw-size (* x-size elem-size))
           (row-mod (mod row-raw-size alignment))
           (row-byte-size (+ row-raw-size row-mod)))
      (values (* row-byte-size (max (reduce #'* rest) 1))
              row-byte-size))))

(defun gl-calc-byte-size (type dimensions &optional (alignment 1))
  (%gl-calc-byte-size (gl-type-size type)
                      dimensions alignment))

(defun make-c-array-from-pointer (dimensions element-type pointer 
                                   &optional (alignment 1))
  (unless dimensions 
    (error "dimensions are not optional when making an array from a pointer"))
  (let* ((p-format (pixel-format-p element-type))
         (element-type2 (if p-format
                            (pixel-format-element-type element-type)
                            (if (keywordp element-type)
                                element-type
                                (symbolicate-package (symbol-package element-type)
                                                     element-type '_))))
         (elem-size (gl-type-size element-type2)))
    (multiple-value-bind (byte-size row-byte-size)
        (%gl-calc-byte-size elem-size dimensions alignment)
      (declare (ignore byte-size))
      (make-instance 'c-array
                     :pointer pointer
                     :dimensions dimensions
                     :element-type element-type2
                     :element-byte-size elem-size
                     :row-byte-size row-byte-size
                     :row-alignment alignment
                     :element-pixel-format (when p-format element-type)))))

(defmacro with-c-array ((var-name c-array) &body body)
  `(let* ((,var-name ,c-array))
     (unwind-protect (progn ,@body) (free-c-array ,var-name))))

(defun clone-c-array (c-array)
  (let* ((size (c-array-byte-size c-array))
         (new-pointer (cffi::%foreign-alloc size)))
    (cffi::%memcpy new-pointer (pointer c-array) size)
    (make-instance 'c-array
                   :pointer new-pointer
                   :dimensions (dimensions c-array)
                   :element-byte-size (element-byte-size c-array)
                   :element-type (element-type c-array)
                   :row-byte-size (row-byte-size c-array)
                   :row-alignment (row-alignment c-array ))))

; [TODO] Bad error message
(defun make-c-array (dimensions element-type 
                      &key initial-contents displaced-by (alignment 1))
  (let* ((dimensions (listify dimensions))
         (p-format (pixel-format-p element-type))
         (element-type2 (if p-format
                            (pixel-format-element-type element-type)
                            (if (keywordp element-type)
                                element-type
                                (symbolicate-package 
                                 (symbol-package element-type)
                                 element-type '_))))
         (elem-size (gl-type-size element-type2)))
    (when (> (length dimensions) 4) 
      (error "c-arrays have a maximum of 4 dimensions: (attempted ~a)"
             (length dimensions)))
    (when (not (find alignment '(1 2 4 8)))
      (error "alignment can only be 1, 2, 4 or 8"))
    (when (not (loop for i in dimensions always (> i 0)))
      (error "all dimensions must be >=1 in length"))
    (when displaced-by
      (if initial-contents
          (error "Cannot displace and populate array at the same time")
          (when (or (not (eql (reduce #'* dimensions) 
                              (reduce #'* (dimensions displaced-by))))
                    (not (eq (element-type displaced-by) element-type2))
                    (> (row-alignment displaced-by) 1))
            (error "Byte size and type of arrays must match and alignment must be 1"))))
    (multiple-value-bind (byte-size row-byte-size)
        (%gl-calc-byte-size elem-size dimensions alignment)
      (let ((new-array (make-instance 
                        'c-array
                        :pointer (or displaced-by 
                                     (cffi::%foreign-alloc byte-size)) 
                        :dimensions dimensions
                        :element-byte-size elem-size
                        :element-type element-type2
                        :row-byte-size row-byte-size
                        :row-alignment alignment
                        :element-pixel-format (when p-format element-type))))
        (when initial-contents
          (if (listp initial-contents)
              (c-populate new-array initial-contents)
              (error "cannot populate with that... will fix error when I have drunk less")))
        new-array))))

;; [TODO] this is damn chunky.. can this be better
(defun calc-gl-index (c-array subscripts)
  (with-slots (dimensions row-byte-size element-type) c-array
    (if (and (eql (length dimensions) (length subscripts))
             (loop for d in dimensions for s in subscripts
                :always (and (< s d) (>= s 0))))
        (+ (* (first subscripts) (element-byte-size c-array))
           (* (or (second subscripts) 0) row-byte-size)
           (* (or (third subscripts) 0) (or (second dimensions) 0) 
              row-byte-size)
           (* (or (fourth subscripts) 0) (or (third dimensions) 0) 
              (or (second dimensions) 0) row-byte-size))
        (error "The subscripts ~a are outside of the c-array range ~a"
               subscripts dimensions))))

(defun calc-1d-gl-index (c-array subscript)
  ;; only really for use with c-populate
  ;; it does take alignment into account
  (calc-gl-index c-array (1d-to-2d-subscript c-array subscript)))

(defun 1d-to-2d-subscript (c-array subscript)
  ;; only really for use with c-populate
  ;; it does take alignment into account
  (with-slots (dimensions) c-array
    (let ((x-size (or (first dimensions) 1))
          (y-size (or (second dimensions) 1))
          (z-size (or (third dimensions) 1)))
      (let* ((y (floor (/ subscript x-size)))
             (z (floor (/ y y-size)))
             (w (floor (/ z z-size))))
        (subseq (list (mod subscript x-size) y z w) 0 (length dimensions))))))

(defun aref-c (c-array &rest subscripts)
  (let ((etype (element-type c-array)))
    (if (keywordp etype)        
        (mem-ref (pointer c-array) etype
                 (calc-gl-index c-array subscripts))        
        (autowrap:wrap-pointer 
         (let ((ptr (pointer c-array)))
           (inc-pointer ptr (calc-gl-index c-array subscripts)))         
         etype))))

(defun (setf aref-c) (value c-array &rest subscripts)  
  (let ((etype (element-type c-array)))
    (if (keywordp etype)
        (setf (mem-ref (pointer c-array) (element-type c-array)
                       (calc-gl-index c-array subscripts))
              value)
        (let ((obj (autowrap:wrap-pointer 
                    (let ((ptr (pointer c-array)))
                      (inc-pointer ptr (calc-gl-index c-array subscripts)))
                    etype)))
          (populate obj value)))))

(defun %aref-c (c-array subscripts)    
  (let ((etype (element-type c-array)))
    (if (keywordp etype)        
        (mem-ref (pointer c-array) etype
                 (calc-gl-index c-array subscripts))        
        (autowrap:wrap-pointer 
         (let ((ptr (pointer c-array)))
           (inc-pointer ptr (calc-gl-index c-array subscripts)))         
         etype))))

(defun (setf %aref-c) (value c-array subscripts)  
  (let ((etype (element-type c-array)))
    (if (keywordp etype)
        (setf (mem-ref (pointer c-array) (element-type c-array)
                       (calc-gl-index c-array subscripts))
              value)
        (let ((obj (autowrap:wrap-pointer 
                    (let ((ptr (pointer c-array)))
                      (inc-pointer ptr (calc-gl-index c-array subscripts)))
                    etype)))
          (populate obj value)))))

;; [TODO] can the common of the two subfuncs be spun off? (almost certainly)
(defun c-populate (c-array data &optional (check-sizes t))
  (labels ((walk-to-dpop (data dimensions &optional structp pos)
             (let ((still-to-walk (rest dimensions)))
               (loop for sublist in data for i from 0 do
                    (if still-to-walk
                        (walk-to-dpop sublist still-to-walk structp (cons i pos))
                        (if structp
                            (populate (%aref-c c-array (reverse (cons i pos)))
                                      sublist)
                            (setf (%aref-c c-array (reverse (cons i pos))) 
                                  sublist))))))
           (check-sizes (data dimensions)
             (if (null dimensions) t
                 (if (eql (first dimensions) (length data))
                     (loop for sublist in data always
                          (check-sizes sublist (rest dimensions)))
                     (error "Dimensions of data and c-array do not match")))))
    (when check-sizes (check-sizes data (dimensions c-array)))
    (walk-to-dpop data (dimensions c-array) (keywordp (element-type c-array)))
    c-array))

(defmethod gl-subseq ((array c-array) start &optional end)
  (let ((dimensions (dimensions array)))
    (if (> (length dimensions) 1)
        (error "Cannot take subseq of multidimensional array")
        (let* ((length (first dimensions))
               (type (element-type array))
               (end (or end length)))
          (if (and (< start end) (< start length) (<= end length))
              (make-c-array-from-pointer
               (list (- end start)) type
               (cffi:inc-pointer (pointer array) 
                                 (%gl-calc-byte-size (element-byte-size array)
                                                     (list start))))
              (error "Invalid subseq start or end for c-array"))))))

(defmethod gl-pull-1 ((object c-array))
  (let* ((dimensions (dimensions object))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth))))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (gl-pull-1 (%aref-c object indices))
                               (recurse (1+ n))))))
      (recurse 0))))

(defmethod gl-pull ((object c-array))
  (gl-pull-1 object))

(defmethod gl-push ((object list) (destination c-array))
  (c-populate destination object))

(defmethod pixel-format-of ((type c-array))
  (or (element-pixel-format type)
      (pixel-format-of (element-type type))))
