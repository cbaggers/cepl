(in-package :cgl)

(defun slot-name (slot) (first slot))
(defun slot-type (slot) (second slot))
(defun slot-normalisedp (slot) (third slot))

(defun agg-type-helper-name (type)
  ;;(utils:symb-package 'cepl-gl 'cgl- (varjo:type-principle type))
  (varjo:type-principle type))

;; ((POS (:VEC3 NIL NIL "vec3") NIL NIL) (COLOR (:VEC4 NIL NIL "vec4") NIL NIL))
(defun make-cstruct-def (name slots)
  `(defcstruct ,name
     ,@(loop for slot in slots :collect 
            (let ((ptype (varjo:type-principle (slot-type slot))))
              (list (slot-name slot)
                    (if (varjo:type-aggregate-p ptype)
                        (agg-type-helper-name (slot-type slot))
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

;; (defglstruct test (a (:float 3)))

(defclass gl-array ()
  ((pointer :initarg :pointer :reader pointer)
   (dimensions :initarg :dimensions :reader dimensions)
   (element-type :initarg :element-type :reader element-type)
   (row-byte-size :initarg :row-byte-size :reader row-byte-size)
   (row-alignment :initarg :row-alignment :reader row-alignment)))

(defun make-gl-array (dimensions element-type 
                      &key initial-contents displaced-to (alignment 1))
  (declare (ignore initial-contents))
  (let ((dimensions (if (listp dimensions) dimensions (list dimensions))))
    (when (> (length dimensions) 4) 
      (error "gl-arrays have a maximum of 4 dimensions: (attempted ~a)"
             (length dimensions)))
    (when (not (find alignment '(1 2 4 8)))
      (error "alignment can only be 1, 2, 4 or 8"))
    (when (not (loop for i in dimensions always (> i 0)))
      (error "all dimensions must be >=1 in length"))
    (multiple-value-bind (byte-size row-byte-size)
        (gl-calc-byte-size element-type dimensions alignment)
      (make-instance 'gl-array
                     :pointer (or displaced-to (cffi::%foreign-alloc byte-size))
                     :dimensions dimensions
                     :element-type element-type
                     :row-byte-size row-byte-size
                     :row-alignment alignment))))

(defun aref-gl (gl-object &rest subscripts)  
  (with-slots (dimensions row-byte-size element-type) gl-object
    (if (and (eql (length dimensions) (length subscripts))
             (loop for d in dimensions for s in subscripts
                :always (and (< s d) (>= s 0))))
        (mem-ref (pointer gl-object) (element-type gl-object)
                 (+ (* (first subscripts) (foreign-type-size element-type))
                    (* (or (second subscripts) 0) row-byte-size)
                    (* (or (third subscripts) 0) (or (second dimensions) 0) 
                       row-byte-size)
                    (* (or (fourth subscripts) 0) (or (third dimensions) 0) 
                       (or (second dimensions) 0) row-byte-size)))
        (error "The subscripts ~a are outside of the gl-array range ~a"
               subscripts dimensions))))

(defun (setf aref-gl) (value gl-object &rest subscripts)  
  (with-slots (dimensions row-byte-size element-type) gl-object
    (if (and (eql (length dimensions) (length subscripts))
             (loop for d in dimensions for s in subscripts
                :always (and (< s d) (>= s 0))))
        (setf (mem-ref (pointer gl-object) (element-type gl-object)
                       (+ (* (first subscripts) (foreign-type-size element-type))
                          (* (or (second subscripts) 0) row-byte-size)
                          (* (or (third subscripts) 0) (or (second dimensions) 0) 
                             row-byte-size)
                          (* (or (fourth subscripts) 0) (or (third dimensions) 0) 
                             (or (second dimensions) 0) row-byte-size)))
              value)
        (error "The subscripts ~a are outside of the gl-array range ~a"
               subscripts dimensions))))

(defun gl-calc-byte-size (type dimensions &optional (alignment 1))  
  (let* ((x-size (first dimensions)) (rest (rest dimensions)))
    (let* ((row-raw-size (* x-size (cffi:foreign-type-size type)))
           (row-mod (mod row-raw-size alignment))
           (row-byte-size (+ row-raw-size row-mod)))
      (values (* row-byte-size (max (reduce #'* rest) 1))
              row-byte-size))))

;; this had errors
;; (defmethod print-object ((object gl-array) stream)
;;   (format stream "#.<GL-ARRAY :element-type ~s :dimensions ~a>"
;;           (slot-value object 'type)
;;           (slot-value object 'dimensions)))
