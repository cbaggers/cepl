(in-package :cepl.c-arrays)

;;------------------------------------------------------------

(defstruct (c-array (:constructor %make-c-array))
  (pointer
   (error "cepl: c-array must be created with a pointer")
   :type cffi-sys:foreign-pointer)
  (dimensions
   (error "cepl: c-array must be created with dimensions")
   :type list)
  (element-type
   (error "cepl: c-array must be created with an element-type")
   :type symbol)
  (element-byte-size
   (error "cepl: c-array must be created with an element-byte-size")
   :type fixnum)
  (struct-element-typep nil :type boolean)
  (row-byte-size
   (error "cepl: c-array must be created with a pointer")
   :type fixnum)
  (element-pixel-format nil :type (or null cepl.pixel-formats:pixel-format)))

(defgeneric pointer (c-array))
(defmethod pointer ((array c-array))
  (c-array-pointer array))

(defmethod dimensions ((array c-array))
  (c-array-dimensions array))

(defmethod element-type ((array c-array))
  (c-array-element-type array))

(defmethod element-byte-size ((array c-array))
  (c-array-element-byte-size array))

;; (defmethod row-byte-size ((array c-array))
;;   (c-array-row-byte-size array))

;; (defmethod element-pixel-format ((array c-array))
;;   (c-array-element-pixel-format array))


(defun blank-c-array-object (c-array)
  (setf (c-array-pointer c-array) (cffi:null-pointer))
  (setf (c-array-dimensions c-array) nil)
  (setf (c-array-element-type c-array) nil)
  (setf (c-array-element-byte-size c-array) 0)
  (setf (c-array-row-byte-size c-array) 0)
  (setf (c-array-element-pixel-format c-array) nil))

(defmethod free ((object c-array))
  (free-c-array object))

(defun free-c-array (c-array)
  (foreign-free (c-array-pointer c-array))
  (blank-c-array-object c-array))

(defmethod print-object ((object c-array) stream)
  (format stream "#<C-ARRAY :element-type ~s :dimensions ~a>"
          (c-array-element-type object)
          (c-array-dimensions object)))

(defmethod print-mem ((thing c-array) &optional (size-in-bytes 64) (offset 0))
  (cepl-utils::%print-mem
   (cffi:inc-pointer (c-array-pointer thing) offset)
   size-in-bytes))

;;------------------------------------------------------------

(defun c-array-byte-size (c-array)
  (%gl-calc-byte-size (c-array-element-byte-size c-array)
                      (c-array-dimensions c-array)))

(defun %gl-calc-byte-size (elem-size dimensions)
  (let* ((x-size (first dimensions)) (rest (rest dimensions))
	 (row-byte-size (* x-size elem-size)))
    (values (* row-byte-size (max (reduce #'* rest) 1))
	    row-byte-size)))

(defun gl-calc-byte-size (type dimensions)
  (%gl-calc-byte-size (gl-type-size type) dimensions))

(defun make-c-array-from-pointer (dimensions element-type pointer)
  (unless dimensions
    (error "dimensions are not optional when making an array from a pointer"))
  (let* ((dimensions (listify dimensions))
         (p-format (cepl.pixel-formats:pixel-format-p element-type))
         (element-type2 (if p-format
                            (pixel-format->lisp-type element-type)
                            element-type))
         (elem-size (gl-type-size element-type2)))
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
       :element-pixel-format (when p-format element-type)))))
;; (symbol-names-cepl-structp element-type)

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
    (cffi::%memcpy new-pointer (c-array-pointer c-array) size)
    (%make-c-array
     :pointer new-pointer
     :dimensions (c-array-dimensions c-array)
     :element-byte-size (c-array-element-byte-size c-array)
     :element-type (c-array-element-type c-array)
     :struct-element-typep (c-array-struct-element-typep c-array)
     :row-byte-size (c-array-row-byte-size c-array))))

;; [TODO] extract error messages
(defun make-c-array (initial-contents &key dimensions element-type)
  (let* ((element-type element-type)
	 (dimensions (listify dimensions))
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
                        :element-pixel-format pixel-format)))
        (when initial-contents
          (c-populate new-array initial-contents nil))
        new-array))))

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
  (row-index-correct subscripts
		     (c-array-dimensions c-array)
		     (c-array-row-byte-size c-array)
		     (c-array-element-byte-size c-array)))

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
      (let* ((row-byte-size (c-array-row-byte-size c-array))
	     (element-byte-size (c-array-element-byte-size c-array))
	     (sub (reverse subscripts))
	     (dim (cons row-byte-size (reverse (rest (c-array-dimensions c-array)))))
	     (p 1))
	(declare (fixnum p))
	(unsafe-ptr-index 4)))))

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
  (calc-gl-index c-array (1d-to-2d-subscript c-array subscript)))

(defun 1d-to-2d-subscript (c-array subscript)
  (let ((dimensions (c-array-dimensions c-array)))
    (let ((x-size (or (first dimensions) 1))
          (y-size (or (second dimensions) 1))
          (z-size (or (third dimensions) 1)))
      (let* ((y (floor (/ subscript x-size)))
             (z (floor (/ y y-size)))
             (w (floor (/ z z-size))))
        (subseq (list (mod subscript x-size) y z w) 0 (length dimensions))))))

(defmacro %with-1-shot-aref-c ((aref-name array &key (prim-type :unknown)
                                          (get t) (set nil))
                               &body body)
  (let* ((arr (gensym "c-array"))
         (etype (gensym "element-type"))
         (primp (gensym "is-cepl-prim-type"))
         (get-code
          (when get
            (case prim-type
              ((t) `((,aref-name (subscripts)
                                 (mem-ref (c-array-pointer ,arr) ,etype
                                          (calc-gl-index ,arr subscripts)))))
              ((nil) `((,aref-name
			(subscripts)
			(autowrap:wrap-pointer
			 (inc-pointer (c-array-pointer ,arr)
				      (calc-gl-index ,arr subscripts))
			 ,etype))))
              (:unknown
               `((,aref-name
		  (subscripts)
		  (if ,primp
		      (mem-ref (c-array-pointer ,arr) ,etype
			       (calc-gl-index ,arr subscripts))
		      (autowrap:wrap-pointer
		       (inc-pointer (c-array-pointer ,arr)
				    (calc-gl-index ,arr subscripts))
		       ,etype))))))))
         (set-code
          (when set
            (case prim-type
              ((t) `(((setf ,aref-name) (value subscripts)
                      (setf (mem-ref (c-array-pointer c-array)
				     (c-array-element-type c-array)
                                     (calc-gl-index c-array subscripts))
                            value))))
              ((nil) `(((setf ,aref-name) (value subscripts)
                        (let ((obj (autowrap:wrap-pointer
                                    (let ((ptr (c-array-pointer c-array)))
                                      (inc-pointer ptr (calc-gl-index
							c-array subscripts)))
                                    ,etype)))
                          (populate obj value)))))
              (:unknown
               `(((setf ,aref-name) (value subscripts)
                  (if ,primp
                      (setf (mem-ref (c-array-pointer c-array)
				     (c-array-element-type c-array)
                                     (calc-gl-index c-array subscripts))
                            value)
                      (let ((obj (autowrap:wrap-pointer
                                  (let ((ptr (c-array-pointer c-array)))
                                    (inc-pointer ptr (calc-gl-index
						      c-array subscripts)))
                                  ,etype)))
                        (populate obj value))))))))))
    `(let* ((,arr ,array)
            (,etype (c-array-element-type ,arr))
            ,@(when (eq :unknown prim-type)
                    `((,primp ,(if (eq prim-type :unknown)
                                   `(not (c-array-struct-element-typep ,arr))
                                   prim-type)))))
       (labels (,@get-code
                ,@set-code)
         ,@body))))

(defun aref-c (c-array &rest subscripts)
  (aref-c* c-array subscripts))

(defun (setf aref-c) (value c-array &rest subscripts)
  (setf (aref-c* c-array subscripts) value))

;;
;; these two dont use &rest on subscripts
;;
(defun aref-c* (c-array subscripts)
  (%with-1-shot-aref-c (aref-c* c-array)
    (aref-c* subscripts)))

(defun (setf aref-c*) (value c-array subscripts)
  (%with-1-shot-aref-c (aref-c* c-array :get nil :set t)
    (setf (aref-c* subscripts) value)))

;;------------------------------------------------------------
;; map & across

;; (defun %map-into-c-1d (dest func source)
;;   (loop :for i :below (first (c-array-dimensions source)) :do
;;      ()))

;;------------------------------------------------------------

;; [TODO] can the common of the two subfuncs be spun off? (almost certainly)
(defun c-populate (c-array data &optional (check-sizes t))
  (labels ((walk-to-dpop (data dimensions &optional structp pos)
             (let ((still-to-walk (rest dimensions)))
               (map nil (lambda (sublist i)
                          (if still-to-walk
                              (walk-to-dpop sublist still-to-walk structp (cons i pos))
                              (if structp
                                  (populate (aref-c* c-array (reverse (cons i pos)))
                                            sublist)
                                  (setf (aref-c* c-array (reverse (cons i pos)))
                                        sublist))))
                    data (range (length data)))))
           (dpop-with-array (data dimensions &optional structp)
             (loop :for i :below (array-total-size data)
                :for coords = (rm-index-to-coords i dimensions) :do
                (if structp
                    (populate (aref-c* c-array coords) (row-major-aref data i))
                    (setf (aref-c* c-array coords) (row-major-aref data i))))))
    (when check-sizes
      (unless (validate-dimensions data (c-array-dimensions c-array))
        (error "Dimensions of array differs from that of the data:~%~a~%~a"
               c-array data)))
    (typecase data
      (sequence (walk-to-dpop data (c-array-dimensions c-array)
                              (c-array-struct-element-typep c-array)))
      (array (dpop-with-array data (c-array-dimensions c-array)
                              (c-array-struct-element-typep c-array))))
    c-array))

(defun rm-index-to-coords (index subscripts)
  (let ((cur index))
    (nreverse
     (loop :for s :in subscripts :collect
        (multiple-value-bind (x rem) (floor cur s)
          (setq cur x)
          rem)))))

(defun validate-dimensions (data dimensions)
  (let* ((dimensions (listify dimensions))
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

(defun subseq-c (array start &optional end)
  (let ((dimensions (dimensions array)))
    (if (> (length dimensions) 1)
        (error "Cannot take subseq of multidimensional array")
        (let* ((length (first dimensions))
               (type (c-array-element-type array))
               (end (or end length)))
          (if (and (< start end) (< start length) (<= end length))
              (make-c-array-from-pointer
               (list (- end start)) type
               (cffi:inc-pointer (c-array-pointer array)
                                 (%gl-calc-byte-size (c-array-element-byte-size array)
                                                     (list start))))
              (error "Invalid subseq start or end for c-array"))))))

(defmethod pull1-g ((object c-array))
  (let* ((dimensions (c-array-dimensions object))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth))))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (pull1-g (aref-c* object indices))
                               (recurse (1+ n))))))
      (recurse 0))))

(defmethod pull-g ((object c-array))
  (pull1-g object))

(defmethod push-g (object (destination c-array))
  (unless (or (listp object) (arrayp object))
    (error "Can only push arrays or lists to c-arrays"))
  (c-populate destination object))

(defmethod lisp-type->pixel-format ((type c-array))
  (or (c-array-element-pixel-format type)
      (lisp-type->pixel-format (c-array-element-type type))))

;;------------------------------------------------------------

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
