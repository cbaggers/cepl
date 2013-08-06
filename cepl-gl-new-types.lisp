(in-package :cgl)

(defun slot-name (slot) (first slot))
(defun slot-type (slot) (second slot))
(defun slot-normalisedp (slot) (third slot))

(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(loop for slot in slots :collect 
            (let ((ptype (varjo:type-principle (slot-type slot))))
              (list (slot-name slot)
                    (if (varjo:type-aggregate-p ptype)
                        (varjo:type-principle (slot-type slot))
                        ptype)
                    :count (if (varjo:type-arrayp (slot-type slot))
                               (varjo:type-array-length (slot-type slot))
                               1))))))

(defmacro defglstruct (name &body slot-descriptions)
  (let ((slots (loop for slot in slot-descriptions collect
                    (destructuring-bind 
                          (slot-name slot-type &key (normalised nil) 
                                     (accessor nil) &allow-other-keys)
                        slot
                      (list slot-name (varjo:flesh-out-type slot-type) 
                            normalised accessor))))
        (struct-name (utils:symb name '-struct))
        (type-name (utils:symb name '-type)))
    `(progn
       (varjo:vdefstruct ,name
         ,@(loop for slot in slots
              collect (append (subseq slot 0 2) 
                              (list nil nil)
                              (last slot))))
       ,(make-cstruct-def struct-name slots)
       (define-foreign-type ,type-name () 
         ()
         (:actual-type :struct ,struct-name)
         (:simple-parser ,name))
       ',name)))

(defclass gl-array ()
  ((pointer :initarg :pointer :reader pointer)
   (dimensions :initarg :dimensions :reader dimensions)
   (element-type :initarg :element-type :reader element-type)
   (row-byte-size :initarg :row-byte-size :reader row-byte-size)
   (row-alignment :initarg :row-alignment :reader row-alignment)))

(defun gl-calc-byte-size (type dimensions &optional (alignment 1))  
  (let* ((x-size (first dimensions)) (rest (rest dimensions)))
    (let* ((row-raw-size (* x-size (cffi:foreign-type-size type)))
           (row-mod (mod row-raw-size alignment))
           (row-byte-size (+ row-raw-size row-mod)))
      (values (* row-byte-size (max (reduce #'* rest) 1))
              row-byte-size))))

(defun make-gl-array (dimensions element-type 
                      &key initial-contents displaced-by (alignment 1))
  (declare (ignore initial-contents))
  (let ((dimensions (if (listp dimensions) dimensions (list dimensions))))
    (when (> (length dimensions) 4) 
      (error "gl-arrays have a maximum of 4 dimensions: (attempted ~a)"
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
                    (not (eq (element-type displaced-by) element-type))
                    (> (row-alignment displaced-by) 1))
            (error "Byte size and type of arrays must match and alignment must be 1"))))
    (multiple-value-bind (byte-size row-byte-size)
        (gl-calc-byte-size element-type dimensions alignment)
      (make-instance 'gl-array
                     :pointer (or displaced-by (cffi::%foreign-alloc byte-size))
                     :dimensions dimensions
                     :element-type element-type
                     :row-byte-size row-byte-size
                     :row-alignment alignment))))

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
        (error "The subscripts ~a are outside of the gl-array range ~a"
               subscripts dimensions))))

(defun calc-1d-gl-index (gl-object subscript)
  ;; only really for use with destructuring populate
  ;; it does take alignment into account
  (calc-gl-index gl-object (1d-to-2d-subscript gl-object subscript)))


(defun 1d-to-2d-subscript (gl-object subscript)
  ;; only really for use with destructuring populate
  ;; it does take alignment into account
  (with-slots (dimensions) gl-object
    (let ((x-size (or (first dimensions) 1))
          (y-size (or (second dimensions) 1))
          (z-size (or (third dimensions) 1)))
      (let* ((y (floor (/ subscript x-size)))
             (z (floor (/ y y-size)))
             (w (floor (/ z z-size))))
        (subseq (list (mod subscript x-size) y z w) 0 (length dimensions))))))

(defun aref-gl (gl-object &rest subscripts)    
  (mem-ref (pointer gl-object) (element-type gl-object)
           (calc-gl-index gl-object subscripts)))

(defun (setf aref-gl) (value gl-object &rest subscripts)  
  (setf (mem-ref (pointer gl-object) (element-type gl-object)
                 (calc-gl-index gl-object subscripts))
        value))

;; haha this is totally wrong, need to take list of lists
(defun destructuring-populate (gl-object data)
  (if (every #'eql (array-dimensions data) (dimensions gl-object))
      (let ((data (make-array (reduce #'* (array-dimensions data)) 
                              :displaced-to data)))
        (loop :for datum :across data :for i :from 0 :do
           (setf (mem-ref (pointer gl-object) (element-type gl-object)
                          (calc-1d-gl-index gl-object i))
                 datum)))
      (error "Source data has different dimensions to gl-array: ~a ~a"
             (array-dimensions *) (dimensions gl-object))))

;; this had errors
;; (defmethod print-object ((object gl-array) stream)
;;   (format stream "#.<GL-ARRAY :element-type ~s :dimensions ~a>"
;;           (slot-value object 'type)
;;           (slot-value object 'dimensions)))
