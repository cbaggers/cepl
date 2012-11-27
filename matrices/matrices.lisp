(in-package :matrices)

;----------------------------------------------------------------

(defun zerop (matrix)
  (every #'base-maths:float-zero matrix))

;----------------------------------------------------------------

(defgeneric munitp (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod munitp ((size (cl:eql 9)) matrix)
  (null (mismatch matrix (matrix3:identity-matrix3))))

(defmethod munitp ((size (cl:eql 16)) matrix)
  (null (mismatch matrix (matrix4:identity-matrix4))))

(defmacro unitp (matrix)
  (base-macros:once-only (matrix)
    `(munitp (cl:length ,matrix) ,matrix)))

(defmacro identityp (matrix)
  (base-macros:once-only (matrix)
    `(munitp (cl:length ,matrix) ,matrix)))

;----------------------------------------------------------------

(defgeneric meq (size matrix-a matrix-b)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod meq ((size (cl:eql 9)) matrix-a matrix-b)
  (matrix3:meql matrix-a matrix-b ))

(defmethod meq ((size (cl:eql 16)) matrix-a matrix-b)
  (matrix4:meql matrix-a matrix-b))

(defmacro eq (matrix-a matrix-b)
  (base-macros:once-only (matrix-a matrix-b)
    `(meq (cl:length ,matrix-a) ,matrix-a ,matrix-b)))

;----------------------------------------------------------------

(defun = (&rest matrices)
  "Returns either t if the matrices are equal. 
   Otherwise it returns nil."
  (let ((matrix-a (first matrices)))
    (loop for matrix in (cdr matrices)
	  when (not (eq matrix-a matrix)) do (return nil)
	    finally (return t))))

;----------------------------------------------------------------

(defun /= (&rest matrices)
  "Returns either t if the matrices are equal. 
   Otherwise it returns nil."
  (let ((matrix-a (first matrices)))
    (loop for matrix in (cdr matrices)
	  when (eq matrix-a matrix) do (return nil)
	    finally (return t))))

;----------------------------------------------------------------

(defgeneric m1+ (size matrix-a matrix-b)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod m1+ ((size (cl:eql 9)) matrix-a matrix-b)
  (matrix3:m+ matrix-a matrix-b ))

(defmethod m1+ ((size (cl:eql 16)) matrix-a matrix-b)
  (matrix4:m+ matrix-a matrix-b))

(defmacro 1+ (matrix-a matrix-b)
  (base-macros:once-only (matrix-a matrix-b)
    `(m1+ (cl:length ,matrix-a) ,matrix-a ,matrix-b)))

;----------------------------------------------------------------

(defun + (&rest matrices)
  (let* ((len (length (first matrices)))
	 (matrix-a (make-array len :element-type 'single-float
				   :initial-element 0.0)))
    (loop for matrix in matrices
	  do (loop for i below len
		   do (cl:setf (cl:aref matrix-a i) 
			       (cl:+ (cl:aref matrix-a i)
				     (cl:aref matrix i)))))))

;----------------------------------------------------------------

(defgeneric m1- (size matrix-a matrix-b)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod m1- ((size (cl:eql 9)) matrix-a matrix-b)
  (matrix3:m- matrix-a matrix-b ))

(defmethod m1- ((size (cl:eql 16)) matrix-a matrix-b)
  (matrix4:m- matrix-a matrix-b))

(defmacro 1- (matrix-a matrix-b)
  (base-macros:once-only (matrix-a matrix-b)
    `(m1- (cl:length ,matrix-a) ,matrix-a ,matrix-b)))

;----------------------------------------------------------------

(defun - (&rest matrices)
  (let* ((len (length (first matrices)))
	 (matrix-a (make-array len :element-type 'single-float
				   :initial-element 0.0)))
    (loop for matrix in matrices
	  do (loop for i below len
		   do (cl:setf (cl:aref matrix-a i) 
			       (cl:- (cl:aref matrix-a i)
				     (cl:aref matrix i)))))))

;----------------------------------------------------------------

(defun elt (matrix row col)
  (declare (simple-array matrix))
  (let ((len (if (cl:eq (length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

(defun elm (matrix row col)
  (let ((len (if (cl:eq (length matrix) 16) 4 3)))
    (aref matrix (cl:+ row (cl:* col len)))))

;----------------------------------------------------------------

(defgeneric mget-rows (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mget-rows ((size (cl:eql 9)) matrix-a)
  (matrix3:get-rows matrix-a))

(defmethod mget-rows ((size (cl:eql 16)) matrix-a)
  (matrix4:get-rows matrix-a))

(defmacro get-rows (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mget-rows (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric mget-columns (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mget-columns ((size (cl:eql 9)) matrix-a)
  (matrix3:get-columns matrix-a))

(defmethod mget-columns ((size (cl:eql 16)) matrix-a)
  (matrix4:get-columns matrix-a))

(defmacro get-columns (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mget-columns (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric mget-row (size matrix row-num)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mget-row ((size (cl:eql 9)) matrix-a row-num)
  (matrix3:get-row matrix-a row-num))

(defmethod mget-row ((size (cl:eql 16)) matrix-a row-num)
  (matrix4:get-row matrix-a row-num))

(defmacro get-row (matrix-a row-num)
  (base-macros:once-only (matrix-a row-num)
    `(mget-row (cl:length ,matrix-a) ,matrix-a ,row-num)))

;----------------------------------------------------------------

(defgeneric mget-column (size matrix col-num)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mget-column ((size (cl:eql 9)) matrix-a col-num)
  (matrix3:get-column matrix-a col-num))

(defmethod mget-column ((size (cl:eql 16)) matrix-a col-num)
  (matrix4:get-column matrix-a col-num))

(defmacro get-column (matrix-a col-num)
  (base-macros:once-only (matrix-a col-num)
    `(mget-column (cl:length ,matrix-a) ,matrix-a ,col-num)))

;----------------------------------------------------------------

(defgeneric mdeterminant (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mdeterminant ((size (cl:eql 9)) matrix-a)
  (matrix3:determinate-cramer matrix-a))

(defmethod mdeterminant ((size (cl:eql 16)) matrix-a)
  (matrix4:determinant matrix-a))

(defmacro determinant (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mdeterminant (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric minverse (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod minverse ((size (cl:eql 9)) matrix-a)
  (matrix3:inverse matrix-a))

(defmethod minverse ((size (cl:eql 16)) matrix-a)
  (matrix4:affine-inverse matrix-a))

(defmacro inverse (matrix-a)
  (base-macros:once-only (matrix-a)
    `(minverse (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric mtranspose (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mtranspose ((size (cl:eql 9)) matrix-a)
  (matrix3:transpose matrix-a))

(defmethod mtranspose ((size (cl:eql 16)) matrix-a)
  (matrix4:transpose matrix-a))

(defmacro transpose (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mtranspose (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric mtrace (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mtrace ((size (cl:eql 9)) matrix-a)
  (matrix3:mtrace matrix-a))

(defmethod mtrace ((size (cl:eql 16)) matrix-a)
  (matrix4:mtrace matrix-a))

(defmacro trace (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mtrace (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

(defgeneric mnegate (size matrix)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod mnegate ((size (cl:eql 9)) matrix-a)
  (matrix3:negate matrix-a))

(defmethod mnegate ((size (cl:eql 16)) matrix-a)
  (matrix4:negate matrix-a))

(defmacro negate (matrix-a)
  (base-macros:once-only (matrix-a)
    `(mnegate (cl:length ,matrix-a) ,matrix-a)))

;----------------------------------------------------------------

;; [TODO] This could be nicer

(defgeneric m* (size-a matrix-a mat-vec-or-scalar)
  (:documentation "Returns t if the vector is the identity matrix"))

(defmethod m* ((size (cl:eql 9)) matrix-a 
	       (mat-vec-or-scalar simple-array))
  (if (< (length mat-vec-or-scalar) 5)
      (matrix3:m* matrix-a mat-vec-or-scalar)
      (matrix3:m*vec matrix-a mat-vec-or-scalar)))

(defmethod m* ((size (cl:eql 16)) matrix-a 
	       (mat-vec-or-scalar simple-array))
    (if (< (length mat-vec-or-scalar) 5)
      (matrix4:m* matrix-a mat-vec-or-scalar)
      (matrix4:mcol*vec4 matrix-a mat-vec-or-scalar)))

(defmethod m* ((size (cl:eql 9)) matrix-a 
	       (mat-vec-or-scalar number))
  (matrix3:m*scalar matrix-a mat-vec-or-scalar))

(defmethod m* ((size (cl:eql 16)) matrix-a 
	       (mat-vec-or-scalar number))
  (matrix4:m*scalar matrix-a mat-vec-or-scalar))

(defmacro * (matrix-a mat-vec-or-scalar)
  (base-macros:once-only (matrix-a mat-vec-or-scalar)
    `(m* (cl:length ,matrix-a) ,matrix-a ,mat-vec-or-scalar)))

;----------------------------------------------------------------

