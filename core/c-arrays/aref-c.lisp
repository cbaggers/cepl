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

(declaim (inline ptr-index-2d)
         (ftype (function (c-array c-array-index c-array-index) cffi-sys:foreign-pointer)
                ptr-index-2d))
(defun ptr-index-2d (c-array x y)
  (declare (c-array c-array)
           (c-array-index x) (c-array-index y)
           (optimize (speed 3) (safety 0) (debug 1))
           (inline inc-pointer c-array-pointer))
  (the cffi-sys:foreign-pointer
       (inc-pointer
        (c-array-pointer c-array)
        (the c-array-index
             (+ (the c-array-index
                     (* y (the c-array-index
                               (c-array-row-byte-size c-array))))
                (the c-array-index
                     (* x (the c-array-index
                               (c-array-element-byte-size c-array)))))))))

(declaim (inline ptr-index-3d)
         (ftype (function (c-array c-array-index c-array-index c-array-index)
                          cffi-sys:foreign-pointer)
                ptr-index-3d))
(defun ptr-index-3d (c-array x y z)
  (declare (c-array c-array)
           (c-array-index x) (c-array-index y) (c-array-index z)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let* ((row-size (the c-array-index (c-array-row-byte-size c-array)))
         (2d-size (the c-array-index
                       (* (the c-array-index (third (c-array-dimensions c-array)))
                          row-size)))
         (byte-offset (the c-array-index
                           (+ (the c-array-index (* z 2d-size))
                              (the c-array-index (* y row-size))
                              (the c-array-index (* (c-array-element-byte-size c-array)
                                             x))))))
    (the cffi-sys:foreign-pointer
         (inc-pointer (c-array-pointer c-array)
                      byte-offset))))

(declaim (inline ptr-index-4d)
         (ftype (function (c-array c-array-index c-array-index c-array-index c-array-index)
                          cffi-sys:foreign-pointer)
                ptr-index-4d))
(defun ptr-index-4d (c-array x y z w)
  (declare (c-array c-array)
           (c-array-index x) (c-array-index y) (c-array-index z) (c-array-index w)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let* ((row-size (the c-array-index (c-array-row-byte-size c-array)))
         (dimensions (c-array-dimensions c-array))
         (2d-size (the c-array-index
                       (* (the c-array-index (third dimensions))
                          row-size)))
         (3d-size (the c-array-index
                       (* (the c-array-index (fourth dimensions))
                          2d-size)))
         (byte-offset
          (the c-array-index
               (+ (the c-array-index (* w 3d-size))
                  (the c-array-index
                       (+ (the c-array-index (* z 2d-size))
                          (the c-array-index (* y row-size))
                          (the c-array-index (* (c-array-element-byte-size c-array)
                                         x))))))))
    (the cffi-sys:foreign-pointer
         (inc-pointer (c-array-pointer c-array)
                      byte-offset))))


(defun ptr-index (c-array &optional (x 0) (y 0 y-set) (z 0 z-set) (w 0 w-set))
  (declare (c-array c-array)
           (c-array-index x y z w)
           (optimize (speed 3) (safety 0) (debug 1)))
  (cond (w-set (ptr-index-4d c-array x y z w))
        (z-set (ptr-index-3d c-array x y z))
        (y-set (ptr-index-2d c-array x y))
        (t (ptr-index-1d c-array x))))

(define-compiler-macro ptr-index
    (c-array &optional x (y 0 y-set) (z 0 z-set) (w 0 w-set))
  (cond (w-set `(ptr-index-4d ,c-array ,x ,y ,z ,w))
        (z-set `(ptr-index-3d ,c-array ,x ,y ,z))
        (y-set `(ptr-index-2d ,c-array ,x ,y))
        (t `(ptr-index-1d ,c-array ,x))))

;;----------------------------------------------------------------------

(defn-inline row-major-aref-c ((c-array c-array) (index c-array-index)) t
  (aref-c*-1d c-array index))

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
    (1 `(aref-c*-1d ,c-array ,(first subscripts)))
    (2 `(aref-c*-2d
         ,c-array ,(first subscripts) ,(second subscripts)))
    (3 `(aref-c*-3d
         ,c-array ,(first subscripts) ,(second subscripts) ,(third subscripts)))
    (4 `(aref-c*-4d
         ,c-array ,(first subscripts) ,(second subscripts)
         ,(third subscripts) ,(fourth subscripts)))
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
    (1 `(setf (aref-c*-1d ,c-array ,(first subscripts))
              ,value))
    (2 `(setf (aref-c*-2d
               ,c-array ,(first subscripts) ,(second subscripts))
              ,value))
    (3 `(setf (aref-c*-3d
               ,c-array ,(first subscripts) ,(second subscripts)
               ,(third subscripts))
              ,value))
    (4 `(setf (aref-c*-4d
               ,c-array ,(first subscripts) ,(second subscripts)
               ,(third subscripts) ,(fourth subscripts))
              ,value))
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

(defun aref-c*-1d (c-array x)
  (declare (c-array c-array)
           (c-array-index x)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-1d c-array x))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defun aref-c*-2d (c-array x y)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-2d c-array x y))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defun aref-c*-3d (c-array x y z)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (c-array-index z)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-3d c-array x y z))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

(defun aref-c*-4d (c-array x y z w)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (c-array-index z)
           (c-array-index w)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-4d c-array x y z w))
        (ref (c-array-element-from-foreign c-array)))
    (funcall ref ptr)))

;;----------------------------------------------------------------------

(defun (setf aref-c*-1d) (value c-array x)
  (declare (c-array c-array)
           (c-array-index x)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-1d c-array x))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defun (setf aref-c*-2d) (value c-array x y)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (optimize (speed 3) (safety 0) (debug 1))
           (inline ptr-index-2d))
  (let ((ptr (ptr-index-2d c-array x y))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defun (setf aref-c*-3d) (value c-array x y z)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (c-array-index z)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-3d c-array x y z))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))

(defun (setf aref-c*-4d) (value c-array x y z w)
  (declare (c-array c-array)
           (c-array-index x)
           (c-array-index y)
           (c-array-index z)
           (c-array-index w)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-4d c-array x y z w))
        (ref (c-array-element-to-foreign c-array)))
    (funcall ref ptr value)))
