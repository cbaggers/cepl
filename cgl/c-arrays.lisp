(in-package :cgl)

;; [TODO] Find anything that uses hidden symbols (::) and justify that usage or 
;;        fix it

;;------------------------------------------------------------
(defclass c-array ()
  ((pointer :initarg :pointer :reader pointer)
   (dimensions :initarg :dimensions :reader dimensions)
   (element-type :initarg :element-type :reader element-type)
   (row-byte-size :initarg :row-byte-size :reader row-byte-size)
   (row-alignment :initarg :row-alignment :reader row-alignment)
   (element-pixel-format :initform nil :initarg :element-pixel-format
                         :reader element-pixel-format)))

(defclass indirect-c-array (c-array) ())

(defun blank-c-array-object (c-array)
  (with-slots (pointer dimensions element-type row-byte-size 
                       row-alignment element-pixel-format) c-array
    (setf pointer nil
          dimensions nil
          element-type nil
          row-byte-size nil
          row-alignment nil
          element-pixel-format nil)))

(defmethod gl-free ((object c-array))
  (free-c-array object))

(defun free-c-array (c-array)
  "Frees the specified c-array."
  (foreign-free (pointer c-array))
  (blank-c-array-object c-array))

;; [TODO] should be baseclass each glstruct will inherit from this
;; [TODO] payload is an uncommited value, so if you have no pointer and
;;        you set the object you will populate the payload.
;;        if you then (setf (aref-c a 0) gl-val) you would apply the 
;;        payload. If there is a pointer you set and get straight from
;;        the foreign data.
(defclass c-value ()
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (element-type :initarg :element-type :reader element-type)
   (lisp-payload :initform nil)))

(defmethod print-object ((object c-array) stream)
  (format stream "#<C-ARRAY :element-type ~s :dimensions ~a>"
          (slot-value object 'element-type)
          (slot-value object 'dimensions)))

(defmethod print-object ((object c-value) stream)
  (format stream "#<C-VALUE type: ~a>"
          (slot-value object 'element-type)))

;;------------------------------------------------------------

(defun c-array-byte-size (c-array)
  (gl-calc-byte-size (element-type c-array) 
                     (dimensions c-array)
                     (row-alignment c-array)))

(defun gl-calc-byte-size (type dimensions &optional (alignment 1))  
  (let* ((x-size (first dimensions)) (rest (rest dimensions)))
    (let* ((row-raw-size (* x-size (cffi:foreign-type-size type)))
           (row-mod (mod row-raw-size alignment))
           (row-byte-size (+ row-raw-size row-mod)))
      (values (* row-byte-size (max (reduce #'* rest) 1))
              row-byte-size))))

(defun make-c-array-from-pointer (dimensions element-type pointer 
                                   &optional (alignment 1) (indirect nil))
  (unless dimensions 
    (error "dimensions are not optional when making an array from a pointer"))
  (let* ((p-format (pixel-format-p element-type))
         (element-type2 (if p-format
                            (pixel-format-element-type element-type)
                            element-type)))
    (if indirect
        (make-instance 'indirect-c-array
                             :pointer pointer
                             :dimensions dimensions
                             :element-type element-type2
                             :row-byte-size nil
                             :row-alignment nil
                             :element-pixel-format nil)
        (multiple-value-bind (byte-size row-byte-size)
            (gl-calc-byte-size element-type dimensions alignment)
          (declare (ignore byte-size))
          (make-instance 'c-array
                         :pointer pointer
                         :dimensions dimensions
                         :element-type element-type2
                         :row-byte-size row-byte-size
                         :row-alignment alignment
                         :element-pixel-format (when p-format element-type))))))

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
                   :element-type (element-type c-array)
                   :row-byte-size (row-byte-size c-array)
                   :row-alignment (row-alignment c-array ))))

; [TODO] Bad error message
(defun make-c-array (dimensions element-type 
                      &key initial-contents displaced-by (alignment 1))
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
         (p-format (pixel-format-p element-type))
         (element-type2 (if p-format
                          (pixel-format-element-type element-type)
                          element-type)))
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
        (gl-calc-byte-size element-type2 dimensions alignment)
      (let ((new-array (make-instance 
                        'c-array
                        :pointer (or displaced-by 
                                     (cffi::%foreign-alloc byte-size))
                        :dimensions dimensions
                        :element-type element-type2
                        :row-byte-size row-byte-size
                        :row-alignment alignment
                        :element-pixel-format (when p-format element-type))))
        (when (not (null initial-contents))
          (cond ((listp initial-contents)
                 (c-populate new-array initial-contents))
                (t (error "cannot populate with that... will fix error when I have drunk less"))))
        new-array))))

;; [TODO] this is damn chunky.. can this be better
(defun calc-gl-index (gl-object subscripts)
  (with-slots (dimensions row-byte-size element-type) gl-object
    (if (and (eql (length dimensions) (length subscripts))
             (loop for d in dimensions for s in subscripts
                :always (and (< s d) (>= s 0))))
        (+ (* (first subscripts) (foreign-type-size element-type))
           (* (or (second subscripts) 0) row-byte-size)
           (* (or (third subscripts) 0) (or (second dimensions) 0) 
              row-byte-size)
           (* (or (fourth subscripts) 0) (or (third dimensions) 0) 
              (or (second dimensions) 0) row-byte-size))
        (error "The subscripts ~a are outside of the c-array range ~a"
               subscripts dimensions))))

(defun calc-1d-gl-index (gl-object subscript)
  ;; only really for use with c-populate
  ;; it does take alignment into account
  (calc-gl-index gl-object (1d-to-2d-subscript gl-object subscript)))

(defun 1d-to-2d-subscript (gl-object subscript)
  ;; only really for use with c-populate
  ;; it does take alignment into account
  (with-slots (dimensions) gl-object
    (let ((x-size (or (first dimensions) 1))
          (y-size (or (second dimensions) 1))
          (z-size (or (third dimensions) 1)))
      (let* ((y (floor (/ subscript x-size)))
             (z (floor (/ y y-size)))
             (w (floor (/ z z-size))))
        (subseq (list (mod subscript x-size) y z w) 0 (length dimensions))))))

(defmethod aref-c ((gl-object c-array) &rest subscripts)    
  (mem-ref (pointer gl-object) (element-type gl-object)
           (calc-gl-index gl-object subscripts)))

(defmethod (setf aref-c) (value (gl-object c-array) &rest subscripts)  
  (setf (mem-ref (pointer gl-object) (element-type gl-object)
                 (calc-gl-index gl-object subscripts))
        value))

(defmethod aref-c* ((gl-object c-array) subscripts)    
  (mem-ref (pointer gl-object) (element-type gl-object)
           (calc-gl-index gl-object subscripts)))

(defmethod (setf aref-c*) (value (gl-object c-array) subscripts)  
  (setf (mem-ref (pointer gl-object) (element-type gl-object)
                 (calc-gl-index gl-object subscripts))
        value))

(defun indirect-calc-gl-index (gl-object subscripts)
  (with-slots (dimensions) gl-object
    (if (and (eql (length dimensions) (length subscripts))
             (loop for d in dimensions for s in subscripts
                :always (and (< s d) (>= s 0))))
        (+ (first subscripts)
           (* (or (second subscripts) 0) (or (first dimensions) 0))
           (* (or (third subscripts) 0) (or (second dimensions) 0))
           (* (or (fourth subscripts) 0) (or (third dimensions) 0) 
              (or (second dimensions) 0)))
        (error "The subscripts ~a are outside of the c-array range ~a"
               subscripts dimensions))))

(defmethod aref-c ((gl-object indirect-c-array) &rest subscripts)
  (let ((ptr (mem-aref (pointer gl-object) :pointer 
                       (calc-gl-index gl-object subscripts))))
    (make-instance 'c-value :element-type (element-type gl-object) :pointer ptr)))

(defmethod (setf aref-c) (value (gl-object indirect-c-array) &rest subscripts)  
  (declare (ignore gl-object subscripts))
  (error "not yet implemented (setf (aref-c <indirect-c-array>))"))

(defmethod aref-c* ((gl-object indirect-c-array) subscripts)    
  (let ((ptr (mem-aref (pointer gl-object) :pointer 
                       (calc-gl-index gl-object subscripts))))
    (make-instance 'c-value :element-type (element-type gl-object) :pointer ptr)))

(defmethod (setf aref-c*) (value (gl-object indirect-c-array) subscripts)  
  (declare (ignore gl-object subscripts))
  (error "not yet implemented (setf (aref-c <indirect-c-array>))"))

;; [TODO] can the common of the two subfuncs be spun off? (almost certainly)
(defun c-populate (gl-object data &optional (check-sizes t))
  (labels ((walk-to-dpop (data dimensions &optional pos)
             (let ((still-to-walk (rest dimensions)))
               (loop for sublist in data for i from 0 do
                    (if still-to-walk
                        (walk-to-dpop sublist still-to-walk (cons i pos))
                        (setf (aref-c* gl-object (reverse (cons i pos))) 
                              sublist)))))
           (check-sizes (data dimensions)
             (if (null dimensions) t
                 (if (eql (first dimensions) (length data))
                     (loop for sublist in data always
                          (check-sizes sublist (rest dimensions)))
                     (error "Dimensions of data and gl-object do not match")))))
    (when check-sizes (check-sizes data (dimensions gl-object)))
    (walk-to-dpop data (dimensions gl-object))
    gl-object))

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
                                 (gl-calc-byte-size type (list start))))
              (error "Invalid subseq start or end for c-array"))))))

(defmethod gl-pull-1 ((object c-array))
  (let* ((dimensions (dimensions object))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth))))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (aref-c* object indices)
                               (recurse (1+ n))))))
      (recurse 0))))

(defmethod gl-pull ((object c-array))
  (gl-pull-1 object))

(defmethod gl-push ((object list) (destination c-array))
  (c-populate destination object))

(defmethod pixel-format-of ((type c-array))
  (or (element-pixel-format type)
      (pixel-format-of (element-type type))))
