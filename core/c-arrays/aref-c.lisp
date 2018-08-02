(in-package :cepl.c-arrays)

;;----------------------------------------------------------------------
;; Q: Why not use mem-aptr?
;; A: That requires looking up the size of the type on every call. Instead
;;    we cache that in the c-array and do the math ourselves.

(defn-inline ptr-index-1d ((c-array c-array) (x c-array-index))
    cffi-sys:foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (profile t))
  (the cffi-sys:foreign-pointer
       (inc-pointer (c-array-pointer c-array)
                    (the c-array-index
                         (* (the c-array-index (c-array-element-byte-size
                                         c-array))
                            x)))))

(defn-inline ptr-index-2d ((c-array c-array)
                           (x c-array-index)
                           (y c-array-index))
    foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline inc-pointer c-array-pointer))
  (the cffi-sys:foreign-pointer
       (inc-pointer
        (c-array-pointer c-array)
        (vec-bind (elem-size row-size) (c-array-sizes c-array)
          (the c-array-index
               (+ (the c-array-index (* y row-size))
                  (the c-array-index (* x elem-size))))))))

(defn-inline ptr-index-3d ((c-array c-array)
                           (x c-array-index)
                           (y c-array-index)
                           (z c-array-index))
    foreign-pointer
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (vec-bind (elem-size row-size 2d-size) (c-array-sizes c-array)
    (let* ((byte-offset (the c-array-index
                             (+ (the c-array-index (* z 2d-size))
                                (the c-array-index (* y row-size))
                                (the c-array-index (* x elem-size))))))
      (the cffi-sys:foreign-pointer
           (inc-pointer (c-array-pointer c-array)
                        byte-offset)))))

(defn-inline ptr-index-4d ((c-array c-array)
                           (x c-array-index)
                           (y c-array-index)
                           (z c-array-index)
                           (w c-array-index))
    foreign-pointer
  (declare(optimize (speed 3) (safety 0) (debug 1)))
  (vec-bind (elem-size row-size 2d-size 3d-size) (c-array-sizes c-array)
    (let* ((byte-offset
            (the c-array-index
                 (the c-array-index
                      (+ (the c-array-index (* w 3d-size))
                         (the c-array-index (* z 2d-size))
                         (the c-array-index (* y row-size))
                         (the c-array-index (* x elem-size)))))))
      (the cffi-sys:foreign-pointer
           (inc-pointer (c-array-pointer c-array)
                        byte-offset)))))


(defn-inline ptr-index (c-array
                        (x c-array-index 0)
                        &optional
                        (y c-array-index 0)
                        (z c-array-index 0)
                        (w c-array-index 0))
    foreign-pointer
  (declare (c-array c-array)
           (c-array-index x y z w)
           (optimize (speed 3) (safety 0) (debug 1)))
  ;; this is safe as all c-arrays will have 0 as the size for missing
  ;; dimensions
  (ptr-index-4d c-array x y z w))

(define-compiler-macro ptr-index
    (c-array &optional x (y 0 y-set) (z 0 z-set) (w 0 w-set))
  (cond (w-set `(ptr-index-4d ,c-array ,x ,y ,z ,w))
        (z-set `(ptr-index-3d ,c-array ,x ,y ,z))
        (y-set `(ptr-index-2d ,c-array ,x ,y))
        (t `(ptr-index-1d ,c-array ,x))))

;;----------------------------------------------------------------------

(deferror c-array-4d-limit () (dimensions)
    "Sorry, currently cepl only supports up to rank 4 c-arrays.
Trying to make a c-array with the following dimensions: ~a

This is quite a daft limit so please feel free to raise a
github issue for this when it becomes a problem for you"
  dimensions)

(deferror c-array-4d-limit-aref () (c-arr indices)
    "Sorry, currently cepl only supports up to rank 4 c-arrays.
Trying to access a c-array: ~a
with the following indices: ~a

This is quite a daft limit so please feel free to raise a
github issue for this when it becomes a problem for you"
  c-arr
  indices)

;;----------------------------------------------------------------------

(defn-inline aref-c*-1d ((c-array c-array)
                         (x c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-1d))
  (let ((ptr (ptr-index-1d c-array x))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defn-inline aref-c*-2d ((c-array c-array)
                         (x c-array-index)
                         (y c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-2d))
  (let ((ptr (ptr-index-2d c-array x y))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defn-inline aref-c*-3d ((c-array c-array)
                         (x c-array-index)
                         (y c-array-index)
                         (z c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-3d))
  (let ((ptr (ptr-index-3d c-array x y z))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defn-inline aref-c*-4d ((c-array c-array)
                         (x c-array-index)
                         (y c-array-index)
                         (z c-array-index)
                         (w c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-4d))
  (let ((ptr (ptr-index-4d c-array x y z w))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

;;----------------------------------------------------------------------

(defn-inline (setf aref-c*-1d) ((value t)
                                (c-array c-array)
                                (x c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-1d))
  (let ((ptr (ptr-index-1d c-array x))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defn-inline (setf aref-c*-2d) ((value t)
                                (c-array c-array)
                                (x c-array-index)
                                (y c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-2d))
  (let ((ptr (ptr-index-2d c-array x y))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defn-inline (setf aref-c*-3d) ((value t)
                                (c-array c-array)
                                (x c-array-index)
                                (y c-array-index)
                                (z c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-3d))
  (let ((ptr (ptr-index-3d c-array x y z))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defn-inline (setf aref-c*-4d) ((value t)
                                (c-array c-array)
                                (x c-array-index)
                                (y c-array-index)
                                (z c-array-index)
                                (w c-array-index))
    t
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-4d))
  (let ((ptr (ptr-index-4d c-array x y z w))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

;;----------------------------------------------------------------------

(defun (setf aref-c) (value c-array &rest subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 (setf (aref-c*-1d c-array (first subscripts))
             value))
    (2 (setf (aref-c*-2d
              c-array (first subscripts) (second subscripts))
             value))
    (3 (setf (aref-c*-3d
              c-array (first subscripts) (second subscripts) (third subscripts))
             value))
    (4 (setf (aref-c*-4d
              c-array (first subscripts) (second subscripts)
              (third subscripts) (fourth subscripts))
             value))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))

(define-compiler-macro (setf aref-c) (value c-array &rest subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 `(setf (aref-c*-1d ,c-array ,@subscripts) ,value))
    (2 `(setf (aref-c*-2d ,c-array ,@subscripts) ,value))
    (3 `(setf (aref-c*-3d ,c-array ,@subscripts) ,value))
    (4 `(setf (aref-c*-4d ,c-array ,@subscripts) ,value))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))

(defun (setf aref-c*) (value c-array subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 (setf (aref-c*-1d c-array (first subscripts))
             value))
    (2 (setf (aref-c*-2d
              c-array (first subscripts) (second subscripts))
             value))
    (3 (setf (aref-c*-3d
              c-array (first subscripts) (second subscripts) (third subscripts))
             value))
    (4 (setf (aref-c*-4d
              c-array (first subscripts) (second subscripts)
              (third subscripts) (fourth subscripts))
             value))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))

;;----------------------------------------------------------------------

(defn-inline row-major-aref-c ((c-array c-array) (index c-array-index)) t
  (let ((row-len (first (c-array-dimensions c-array))))
    (multiple-value-bind (row i) (floor index row-len)
      (aref-c*-2d c-array i row))))

(defn-inline (setf row-major-aref-c) ((value t)
                                      (c-array c-array)
                                      (index c-array-index))
    t
  (let ((row-len (first (c-array-dimensions c-array))))
    (multiple-value-bind (row i) (floor index row-len)
      (setf (aref-c*-2d c-array i row) value))))

(defun aref-c (c-array &rest subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 (aref-c*-1d c-array (first subscripts)))
    (2 (aref-c*-2d
        c-array (first subscripts) (second subscripts)))
    (3 (aref-c*-3d
        c-array (first subscripts) (second subscripts) (third subscripts)))
    (4 (aref-c*-4d
        c-array (first subscripts) (second subscripts)
        (third subscripts) (fourth subscripts)))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))

(define-compiler-macro aref-c (c-array &rest subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 `(aref-c*-1d ,c-array ,@subscripts))
    (2 `(aref-c*-2d ,c-array ,@subscripts))
    (3 `(aref-c*-3d ,c-array ,@subscripts))
    (4 `(aref-c*-4d ,c-array ,@subscripts))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))

(defun aref-c* (c-array subscripts)
  (declare (type c-array c-array))
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 (aref-c*-1d c-array (first subscripts)))
    (2 (aref-c*-2d
        c-array (first subscripts) (second subscripts)))
    (3 (aref-c*-3d
        c-array (first subscripts) (second subscripts) (third subscripts)))
    (4 (aref-c*-4d
        c-array (first subscripts) (second subscripts)
        (third subscripts) (fourth subscripts)))
    (otherwise (error 'c-array-4d-limit-aref
                      :c-arr c-array
                      :indices subscripts))))
