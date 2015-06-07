(in-package :matrices)

;;----------------------------------------------------------------

(defun zerop (matrix)
  (every #'base-maths:float-zero matrix))

;;----------------------------------------------------------------


(defun unitp (matrix)
  (let ((len (length matrix)))
    (cond
      ((= len 9)
       (null (mismatch matrix (matrix3:identity-matrix3))))
      ((= len 16)
       (null (mismatch matrix (matrix4:identity-matrix4)))))))


;;----------------------------------------------------------------


(defun eql (matrix-a matrix-b)
  (let ((len (length matrix-a)))
    (assert (= len (length matrix-b)))
    (cond
      ((= len 9)
       (matrix3:eql matrix-a matrix-b))
      ((= len 16)
       (matrix4:eql matrix-a matrix-b)))))


;;----------------------------------------------------------------

(defun = (&rest matrices)
  "Returns either t if the matrices are equal.
   Otherwise it returns nil."
  (let ((matrix-a (first matrices)))
    (loop for matrix in (cdr matrices)
       when (not (eql matrix-a matrix)) do (return nil)
       finally (return t))))

;;----------------------------------------------------------------

(defun /= (&rest matrices)
  "Returns either t if the matrices are equal.
   Otherwise it returns nil."
  (let ((matrix-a (first matrices)))
    (loop for matrix in (cdr matrices)
       when (eql matrix-a matrix) do (return nil)
       finally (return t))))

;;----------------------------------------------------------------


(defun 1+ (matrix-a matrix-b)
  (let ((len (length matrix-a)))
    (assert (= len (length matrix-b)))
    (cond
      ((= len 9)
       (matrix3:m+ matrix-a matrix-b))
      ((= len 16)
       (matrix4:m+ matrix-a matrix-b)))))


;;----------------------------------------------------------------

(defun + (&rest matrices)
  (let* ((len (length (first matrices)))
         (matrix-a (make-array len :element-type 'single-float
                               :initial-element 0.0)))
    (loop for matrix in matrices
       do (loop for i below len
             do (cl:setf (cl:aref matrix-a i)
                         (cl:+ (cl:aref matrix-a i)
                               (cl:aref matrix i)))))))

;;----------------------------------------------------------------


(defun 1- (matrix-a matrix-b)
  (let ((len (length matrix-a)))
    (assert (= len (length matrix-b)))
    (cond
      ((= len 9)
       (matrix3:m- matrix-a matrix-b ))
      ((= len 16)
       (matrix4:m- matrix-a matrix-b)))))


;;----------------------------------------------------------------

(defun - (&rest matrices)
  (let* ((len (length (first matrices)))
         (matrix-a (make-array len :element-type 'single-float
                               :initial-element 0.0)))
    (loop for matrix in matrices
       do (loop for i below len
             do (cl:setf (cl:aref matrix-a i)
                         (cl:- (cl:aref matrix-a i)
                               (cl:aref matrix i)))))))

;;----------------------------------------------------------------

(defun elt (matrix row col)
  (declare (simple-array matrix))
  (let ((len (if (cl:= (length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

(defun elm (matrix row col)
  (let ((len (if (cl:= (length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

;;----------------------------------------------------------------


(defun get-rows (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:get-rows matrix-a))
      ((= len 16)
       (matrix4:get-rows matrix-a)))))


;;----------------------------------------------------------------


(defun get-columns (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:get-columns matrix-a))
      ((= len 16)
       (matrix4:get-columns matrix-a)))))


;;----------------------------------------------------------------


(defun get-row (matrix-a row-num)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:get-row matrix-a row-num))
      ((= len 16)
       (matrix4:get-row matrix-a row-num)))))


;;----------------------------------------------------------------


(defun get-column (matrix-a col-num)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:get-column matrix-a col-num))
      ((= len 16)
       (matrix4:get-column matrix-a col-num)))))


;;----------------------------------------------------------------


(defun determinant (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:determinate-cramer matrix-a))
      ((= len 16)
       (matrix4:determinant matrix-a)))))


;;----------------------------------------------------------------


(defun inverse (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:inverse matrix-a))
      ((= len 16)
       (matrix4:affine-inverse matrix-a)))))


;;----------------------------------------------------------------


(defun transpose (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:transpose matrix-a))
      ((= len 16)
       (matrix4:transpose matrix-a)))))


;;----------------------------------------------------------------


(defun trace (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:mtrace matrix-a))
      ((= len 16)
       (matrix4:mtrace matrix-a)))))


;;----------------------------------------------------------------


(defun negate (matrix-a)
  (let ((len (length matrix-a)))
    (cond
      ((= len 9)
       (matrix3:negate matrix-a))
      ((= len 16)
       (matrix4:negate matrix-a)))))


;;----------------------------------------------------------------


(defun * (matrix-a mat-vec-or-scalar)
  (let ((len (length matrix-a)))
    (if (typep mat-vec-or-scalar 'number)
        (cond
          ((= len 9)
           (matrix3:m*scalar matrix-a mat-vec-or-scalar))
          ((= len 16)
           (matrix4:m*scalar matrix-a mat-vec-or-scalar)))

        (cond
          ((= len 9)
           (if (< (cl:length mat-vec-or-scalar) 5)
               (matrix3:mcol*vec3 matrix-a mat-vec-or-scalar)
               (progn
                 (assert (= len (length mat-vec-or-scalar)))
                 (matrix3:m* matrix-a mat-vec-or-scalar))))
          ((= len 16)
           (if (< (cl:length mat-vec-or-scalar) 5)
               (matrix4:mcol*vec4 matrix-a mat-vec-or-scalar)
               (progn
                 (assert (= len (length mat-vec-or-scalar)))
                 (matrix4:m* matrix-a mat-vec-or-scalar))))))))


;;----------------------------------------------------------------


(defun to-string (mat)
  (case (length mat)
    (9 (format nil "(m! ~a ~a ~a ~%     ~a ~a ~a ~%     ~a ~a ~a)~%"
               (m3:melm mat 0 0) (m3:melm mat 0 1) (m3:melm mat 0 2)
               (m3:melm mat 1 0) (m3:melm mat 1 1) (m3:melm mat 1 2)
               (m3:melm mat 2 0) (m3:melm mat 2 1) (m3:melm mat 2 2)))
    (16 (format nil "(m! ~a ~a ~a ~a ~%     ~a ~a ~a ~a ~%     ~a ~a ~a ~a ~%     ~a ~a ~a ~a)~%"
                (m4:melm mat 0 0) (m4:melm mat 0 1) (m4:melm mat 0 2) (m4:melm mat 0 3)
                (m4:melm mat 1 0) (m4:melm mat 1 1) (m4:melm mat 1 2) (m4:melm mat 1 3)
                (m4:melm mat 2 0) (m4:melm mat 2 1) (m4:melm mat 2 2) (m4:melm mat 2 3)
                (m4:melm mat 3 0) (m4:melm mat 3 1) (m4:melm mat 3 2) (m4:melm mat 3 3)))
    (otherwise "<unknown matrix format: print-matrix assumes a 3x3 or 4x4 matrix>")))
