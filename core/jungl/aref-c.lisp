(in-package :jungl)

;;----------------------------------------------------------------------

(declaim (inline ptr-index-1d)
	 (ftype (function (c-array fixnum) cffi-sys:foreign-pointer)
		ptr-index-1d))
(defun ptr-index-1d (c-array x)
  (declare (c-array c-array)
	   (fixnum x)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (the cffi-sys:foreign-pointer
       (inc-pointer (c-array-pointer c-array)
		    (the fixnum
			 (* (the fixnum (c-array-element-byte-size
					 c-array))
			    x)))))

(declaim (inline ptr-index-2d)
	 (ftype (function (c-array fixnum fixnum) cffi-sys:foreign-pointer)
		ptr-index-2d))
(defun ptr-index-2d (c-array x y)
  (declare (c-array c-array)
	   (fixnum x) (fixnum y)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (the cffi-sys:foreign-pointer
       (inc-pointer
	(c-array-pointer c-array)
	(the fixnum
	     (+ (the fixnum
		     (* y (the fixnum
			       (c-array-row-byte-size c-array))))
		(the fixnum
		     (* x (the fixnum
			       (c-array-element-byte-size c-array)))))))))

(declaim (inline ptr-index-3d)
	 (ftype (function (c-array fixnum fixnum fixnum)
			  cffi-sys:foreign-pointer)
		ptr-index-3d))
(defun ptr-index-3d (c-array x y z)
  (declare (c-array c-array)
	   (fixnum x) (fixnum y) (fixnum z)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (let* ((row-size (the fixnum (c-array-row-byte-size c-array)))
	 (2d-size (the fixnum
		       (* (the fixnum (third (c-array-dimensions c-array)))
			  row-size)))
	 (byte-offset (the fixnum
			   (+ (the fixnum (* z 2d-size))
			      (the fixnum (* y row-size))
			      (the fixnum (* (c-array-element-byte-size c-array)
					     x))))))
    (the cffi-sys:foreign-pointer
	 (inc-pointer (c-array-pointer c-array)
		      byte-offset))))


;;----------------------------------------------------------------------

(defun aref-c (c-array &rest subscripts)
  (case= (length subscripts)
    (0 (error "aref-c: invalid number of subscripts: 0"))
    (1 (%aref-c-1d c-array (first subscripts)))
    (2 )
    (3 )))


(defun %aref-c-1d (c-array x)
  (declare (c-array c-array)
	   (fixnum x)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-1d c-array x))
	(type (c-array-element-type c-array)))
    (if (c-array-struct-element-typep c-array)
	(autowrap:wrap-pointer ptr type)
	(mem-ref ptr type 0))))

(defun %aref-c-2d (c-array x y)
  (declare (c-array c-array)
	   (fixnum x)
	   (fixnum y)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-2d c-array x y))
	(type (c-array-element-type c-array)))
    (if (c-array-struct-element-typep c-array)
	(autowrap:wrap-pointer ptr type)
	(mem-ref ptr type 0))))

(defun %aref-c-3d (c-array x y z)
  (declare (c-array c-array)
	   (fixnum x)
	   (fixnum y)
	   (fixnum z)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (let ((ptr (ptr-index-3d c-array x y z))
	(type (c-array-element-type c-array)))
    (if (c-array-struct-element-typep c-array)
	(autowrap:wrap-pointer ptr type)
	(mem-ref ptr type 0))))

(defun %aref-c-nd (c-array subscripts)
  (let* ((byte-offset (+ (* z 2d-size)
			 (* y row-size)
			 (* (c-array-element-byte-size c-array) x)))
	 (type (c-array-element-type c-array)))
    (if (c-array-struct-element-typep c-array)
	(autowrap:wrap-pointer (inc-pointer (c-array-pointer c-array)
					    byte-offset)
			       type)
	(mem-ref (c-array-pointer c-array) type byte-offset))))

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
