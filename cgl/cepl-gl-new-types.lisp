(in-package :cgl)

;;------------------------------------------------------------
;; [TODO] We have gl-arrays...Is this a good name? They are gl 
;;        friendly bu have nothing to do with gl really.
;;        What would a better name be?
;;        c-array garray g-array f-array foreign-array
;;        carray ceplarray

(defclass gl-array ()
  ((pointer :initarg :pointer :reader pointer)
   (dimensions :initarg :dimensions :reader dimensions)
   (element-type :initarg :element-type :reader element-type)
   (row-byte-size :initarg :row-byte-size :reader row-byte-size)
   (row-alignment :initarg :row-alignment :reader row-alignment)))

;; [TODO] should be baseclass each glstruct will inherit from this
;; [TODO] payload is an uncommited value, so if you have no pointer and
;;        you set the object you will populate the payload.
;;        if you then (setf (aref-gl a 0) gl-val) you would apply the 
;;        payload. If there is a pointer you set and get straight from
;;        the foreign data.
;; [TODO] remove element-type...no wait... this provides a super easy check
;;        against gl-arrays for compatibility
(defclass gl-value ()
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (element-type :initarg :element-type :reader element-type)
   (lisp-payload :initform nil)))

(defmethod print-object ((object gl-array) stream)
  (format stream "#<GL-ARRAY :element-type ~s :dimensions ~a>"
          (slot-value object 'element-type)
          (slot-value object 'dimensions)))

(defmethod print-object ((object gl-value) stream)
  (format stream "#GLS<~a :SLOT NIL>"
          (slot-value object 'element-type)))

(defgeneric dpop1 (type gl-object pos data))
(defgeneric gl-assign-attrib-pointers (array-type &optional attrib-num
                                                    pointer-offset
                                                    stride-override
                                                    normalised))
;;------------------------------------------------------------

(defun gl-calc-byte-size (type dimensions &optional (alignment 1))  
  (let* ((x-size (first dimensions)) (rest (rest dimensions)))
    (let* ((row-raw-size (* x-size (cffi:foreign-type-size type)))
           (row-mod (mod row-raw-size alignment))
           (row-byte-size (+ row-raw-size row-mod)))
      (values (* row-byte-size (max (reduce #'* rest) 1))
              row-byte-size))))

(defun make-gl-array (dimensions element-type 
                      &key initial-contents displaced-by (alignment 1))
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
      (let ((new-array (make-instance 
                        'gl-array
                        :pointer (or displaced-by 
                                     (cffi::%foreign-alloc byte-size))
                        :dimensions dimensions
                        :element-type element-type
                        :row-byte-size row-byte-size
                        :row-alignment alignment)))
        (cond ((listp initial-contents) 
               (destructuring-populate new-array initial-contents)))
        new-array))))

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

(defun aref-gl* (gl-object subscripts)    
  (mem-ref (pointer gl-object) (element-type gl-object)
           (calc-gl-index gl-object subscripts)))

(defun (setf aref-gl*) (value gl-object subscripts)  
  (setf (mem-ref (pointer gl-object) (element-type gl-object)
                 (calc-gl-index gl-object subscripts))
        value))

(defmethod dpop1 ((type t) gl-object pos data)
  (setf (aref-gl* gl-object pos) data))

;; [TODO] can the common of the two subfuncs be spun off? (almost certainly)
(defun destructuring-populate (gl-object data &optional (check-sizes t))
  (labels ((walk-to-dpop (data dimensions &optional pos)
             (let ((still-to-walk (rest dimensions)))
               (loop for sublist in data for i from 0 do
                    (if still-to-walk
                        (walk-to-dpop sublist still-to-walk (cons i pos))
                        (dpop1 (element-type gl-object) gl-object
                               (reverse (cons i pos)) sublist)))))
           (check-sizes (data dimensions)
             (if (null dimensions) t
                 (if (eql (first dimensions) (length data))
                     (loop for sublist in data always
                          (check-sizes sublist (rest dimensions)))
                     (error "Dimensions of data and gl-object do not match")))))
    (when check-sizes (check-sizes data (dimensions gl-object)))
    (walk-to-dpop data (dimensions gl-object))
    gl-object))


(defmethod gl-assign-attrib-pointers ((array-type t) &optional (attrib-num 0)
                                                       (pointer-offset 0)
                                                       stride-override
                                                       normalised)
  (let ((type (varjo:flesh-out-type array-type)))
    (if (varjo:type-built-inp type)
        (let ((slot-layout (expand-slot-to-layout 
                            (list type normalised)))
              (stride 0))
          (loop :for attr :in slot-layout
             :for i :from 0
             :with offset = 0
             :do (progn 
                   (gl:enable-vertex-attrib-array (+ attrib-num i))
                   (%gl:vertex-attrib-pointer 
                    (+ attrib-num i) (first attr) (second attr)
                    (third attr) (or stride-override stride)
                    (cffi:make-pointer (+ offset pointer-offset))))
             :do (setf offset (+ offset (* (first attr) 
                                           (cffi:foreign-type-size
                                            (second attr))))))
          (length slot-layout))
        (error "Type ~a is not known to cepl" type))))
