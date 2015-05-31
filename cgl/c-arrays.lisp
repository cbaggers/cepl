(in-package :cgl)

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

(defmethod print-mem ((thing c-array) &optional (size-in-bytes 64) (offset 0))
  (utils::%print-mem (cffi:inc-pointer (pointer thing) offset) size-in-bytes))

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
                            (pixel-format->lisp-type element-type)
                            element-type))
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

(defmacro with-c-arrays ((var-name c-arrays) &body body)
  `(let* ((,var-name ,c-arrays))
     (unwind-protect (progn ,@body)
       (loop :for a :in ,var-name :do (free-c-array a)))))

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
                   :row-alignment (row-alignment c-array))))

;; [TODO] extract error messages
(defun make-c-array (initial-contents
                      &key dimensions element-type displaced-by (alignment 1))
  (let* ((dimensions (listify dimensions))
         (dimensions
          (if dimensions
              (if initial-contents
                  (or (validate-dimensions initial-contents dimensions)
                      (error "Dimensions are invalid for initial-contents~%~a~%~a"
                             dimensions initial-contents))
                  (if (every #'integerp dimensions)
                      dimensions
                      (error "Invalid dimensions ~a" dimensions)))
              (if initial-contents
                  (typecase initial-contents
                    (sequence (list (length initial-contents)))
                    (array (array-dimensions initial-contents)))
                  (error "make-c-array must be given initial-elements or dimensions"))))
         (p-format (pixel-format-p element-type))
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
                    (not (eq (element-type displaced-by) element-type))
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
                        :element-type element-type
                        :row-byte-size row-byte-size
                        :row-alignment alignment
                        :element-pixel-format pixel-format)))
        (when initial-contents
          (c-populate new-array initial-contents nil))
        new-array))))

;; Old but here incase the crazy below it fails :)
;; (defun calc-gl-index (c-array subscripts)
;;   (with-slots (dimensions row-byte-size element-type) c-array
;;     (if (and (eql (length dimensions) (length subscripts))
;;              (loop for d in dimensions for s in subscripts
;;                 :always (and (< s d) (>= s 0))))
;;         (+ (* (first subscripts) (element-byte-size c-array))
;;            (* (or (second subscripts) 0) row-byte-size)
;;            (* (or (third subscripts) 0) (or (second dimensions) 0)
;;               row-byte-size)
;;            (* (or (fourth subscripts) 0) (or (third dimensions) 0)
;;               (or (second dimensions) 0) row-byte-size))
;;         (error "The subscripts ~a are outside of the c-array range ~a"
;;                subscripts dimensions))))

(defun row-index-correct
    (indices dimensions
     &optional (row-size (car (last dimensions)))
       (element-size 1))
  (let ((indices (reverse indices))
        (dimensions (cons row-size (reverse (rest dimensions)))))
    (+ (* (first indices) element-size)
       (let ((p (first dimensions)))
         (loop :for d :in (rest dimensions)
            :for i :in (rest indices)
            :sum (* i p)
            :do (setq p (* p d)))))))

(defun calc-gl-index-correct (c-array subscripts)
  (with-slots (dimensions row-byte-size element-byte-size) c-array
    (row-index-correct subscripts dimensions row-byte-size element-byte-size)))

(defun calc-gl-index (c-array subscripts)
  ;; This is allowed to be unsafe as the ptr is a fixed quantity
  ;; if you shove a stupid number in a pointer it'll fail no matter what
  (macrolet
      ((unsafe-ptr-index (dims &optional (c 0))
         `(the fixnum
               (+ (the fixnum (* (the fixnum (car sub))
                                 ,(if (= c 0)
                                      '(the fixnum element-byte-size)
                                      'p)))
                  ,(if (= (1+ c) dims)
                       0
                       `(progn
                          (setq p (the fixnum
                                       (* p ,(if (= c 0)
                                                 '(the fixnum row-byte-size)
                                                 '(the fixnum (car dim))))))
                          (setq sub (cdr sub))
                          (setq dim (cdr dim))
                          (if (car dim)
                              (unsafe-ptr-index ,dims ,(1+ c))
                              (the fixnum 0))))))))
    (locally (declare (optimize (speed 3) (safety 0)))
      (with-slots (dimensions row-byte-size element-byte-size) c-array
        (let ((sub (reverse subscripts))
              (dim (cons row-byte-size (reverse (rest dimensions))))
              (p 1))
          (declare (fixnum p))
          (unsafe-ptr-index 4))))))

(defmacro index-n-writer (dims dims-form subs-form &optional (c 0) sub dim p)
  "This essentially is a loop unroller for calculating the row-major index
given:
dims - the max number of dimensions this will support
dims-form - a form that will give a list of dimensions
subs-form - a form that will give a list of subscripts

For example:
;; (defun rm-index-4 (dimensions subscripts)
;;   (index-n-writer 4 dimensions subscripts))

is a function that would calculate the row major index
for any array of, up to and including, 4 dimensions."
  (let* ((sub (or sub (gensym "sub")))
         (dim (or dim (gensym "dim")))
         (p (or p (gensym "p")))
         (r `(+ (* (the integer (car (the list ,sub))) ,p)
                ,(if (= (1+ c) dims)
                     0
                     `(progn
                        (setq ,p (* ,p (the integer (car ,dim))))
                        (setq ,sub (cdr (the list ,sub)))
                        (setq ,dim (cdr (the list ,dim)))
                        (if (car (the list ,dim))
                            (index-n-writer ,dims nil nil ,(1+ c) ,sub ,dim ,p)
                            0))))))
    (if (= c 0)
        `(let ((,sub ,subs-form)
               (,dim ,dims-form)
               (,p 1))
           (declare (list ,sub ,dim)
                    (integer ,p))
           ,r)
        r)))

(defun row-major-index (dimensions subscripts
                        &optional (row-size (first dimensions)) (element-size 1))
  (let ((r (* (first subscripts) element-size))
        (p row-size))
    (loop :for s :in (cdr subscripts) :for d :in dimensions :do
       (incf r (* s p))
       (setf p (* p d)))
    r))

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
               (map nil (lambda (sublist i)
                          (if still-to-walk
                              (walk-to-dpop sublist still-to-walk structp (cons i pos))
                              (if structp
                                  (populate (%aref-c c-array (reverse (cons i pos)))
                                            sublist)
                                  (setf (%aref-c c-array (reverse (cons i pos)))
                                        sublist))))
                    data (range (length data)))))
           (dpop-with-array (data dimensions &optional structp)
             (loop :for i :below (array-total-size data)
                :for coords = (rm-index-to-coords i dimensions) :do
                (if structp
                    (populate (%aref-c c-array coords) (row-major-aref data i))
                    (setf (%aref-c c-array coords) (row-major-aref data i))))))
    (when check-sizes
      (unless (validate-dimensions data (dimensions c-array))
        (error "Dimensions of array differs from that of the data:~%~a~%~a"
               c-array data)))
    (typecase data
      (sequence (walk-to-dpop data (dimensions c-array) (not (keywordp (element-type c-array)))))
      (array (dpop-with-array data (dimensions c-array) (not (keywordp (element-type c-array))))))
    c-array))

(defun rm-index-to-coords (index subscripts)
  (let ((cur index))
    (nreverse
     (loop :for s :in subscripts :collect
        (multiple-value-bind (x rem) (floor cur (car subscripts))
          (setq cur x)
          rem)))))

(defun validate-dimensions (data dimensions)
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
         (r (typecase data
              (sequence (validate-seq-dimensions data dimensions))
              (array (validate-arr-dimensions data dimensions))
              (otherwise nil))))
    (when (and r (every #'identity r)) r)))

(defun validate-arr-dimensions (data dimensions)
  (let* ((actual-dims (array-dimensions data)))
    (if (= (length actual-dims) (length dimensions))
        (mapcar (lambda (d a) (if (eq d :?) a (when (= d a) d)))
                dimensions
                actual-dims)
        nil)))

(defun validate-seq-dimensions (data dimensions &optional (orig-dim dimensions) accum)
  (if (null dimensions)
      (reverse accum)
      (typecase data
        (sequence
         (let* ((f (first dimensions))
                (data-len (length data))
                (d (if (eq :? f) data-len (when (= f data-len) f))))
           (validate-seq-dimensions
            (when (> data-len 0)
              (elt data 0)) (rest dimensions) orig-dim
              (cons d accum))))
        (otherwise nil))))

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

(defmethod pull1-g ((object c-array))
  (let* ((dimensions (dimensions object))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth))))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (pull1-g (%aref-c object indices))
                               (recurse (1+ n))))))
      (recurse 0))))

(defmethod pull-g ((object c-array))
  (pull1-g object))

(defmethod push-g (object (destination c-array))
  (unless (or (listp object) (arrayp object))
    (error "Can only push arrays or lists to c-arrays"))
  (c-populate destination object))

(defmethod lisp-type->pixel-format ((type c-array))
  (or (element-pixel-format type)
      (lisp-type->pixel-format (element-type type))))

;;------------------------------------------------------------

(let ((states `(((integer 0 256) :ubyte
                 ((integer -127 128) fixnum single-float double-float))
                ((integer -127 128) :byte
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

;; ideally we would use a generic reduce in this case to handle the different
;; kinds of structures, but alas this is not available.
(defun scan-for-type (data)
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

(defun update-data (data type)
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
