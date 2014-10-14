;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This package provides a generic interface to all the vector
;; functions provided in vector2,3 & 4.
;; It is here to make programming more pleasent.
;; Also see the reader macro for creating vectors 

;; DO NOT ':USE' THIS PACKAGE IN YOUR PROGRAMS AS IT REDEFINES
;; VARIOUS MATHEMATICAL SYMBOLS

(in-package :vectors)

;;----------------------------------------------------------------

(define-compiler-macro swizzle (vec pattern)
  (let* ((name (cl:symbol-name pattern))
         (len (cl:length name))
         (arr (gensym "arr")))
    (if (or (> len 4) (< len 2))
        (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
        `(cl:let ((,arr ,vec))
           (cl:make-array
            ,len :element-type 'single-float :initial-contents
            (list ,@(loop :for char :across name
                       :collect `(aref ,arr ,(or (position char '(#\X #\Y #\Z #\W))
                                                 (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char))))))))))

(defun swizzle (vec pattern)
  (let* ((name (cl:symbol-name pattern))
         (len (cl:length name))
         (result (cl:make-array (cl:length name) :element-type 'single-float)))
    (if (or (> len 4) (< len 2))
        (error "Vectors: swizzle: Cepl vectors cannot have a length less that 2 or greater than 4")
        (loop :for char :across name :for i :from 0 :do
           (setf (aref result i)
                 (aref vec (or (position char '(#\X #\Y #\Z #\W))
                               (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char))))))
    result))


;;----------------------------------------------------------------

(defun make-vector (x y &optional z w)
  "This takes floats and give back a vector, this is just an
   array but it specifies the array type and populates it. "
  (cond (w (vector4:make-vector4 x y z w))
        (z (vector3:make-vector3 x y z))
        (t (vector2:make-vector2 x y))))

;; 
(defun merge-into-vector (&rest vectors)
  "Takes a list of vectors and combines them into a new vector"
  (labels ((seqify (x) 
             (if (or (listp x) (arrayp x))
                 x
                 (list x))))
    (let ((combined (mapcar #'(lambda (x) 
                                (coerce x 'single-float))
                            (apply #'concatenate 'list
                                   (mapcar #'seqify vectors)))))
      (apply #'make-vector combined))))

;;----------------------------------------------------------------

(defun zerop (vec-a)
  "Returns t if the vector is of zero length"
  (case (length vec-a)
    (2 (v2:vzerop vec-a))
    (3 (v3:vzerop vec-a))
    (4 (v4:vzerop vec-a))))

;;----------------------------------------------------------------

(defun unitp (vec-a)
  (case (length vec-a)
    (2 (v2:unitp vec-a))
    (3 (v3:unitp vec-a))
    (4 (v4:unitp vec-a))))

;;----------------------------------------------------------------

(defun eq (vec-a vec-b)
  (case (length vec-a)
    (2 (v2:v-eq vec-a vec-b))
    (3 (v3:v-eq vec-a vec-b))
    (4 (v4:v-eq vec-a vec-b))))

;;----------------------------------------------------------------

(defun = (&rest vecs)
  "Returns either t if the vectors are equal. 
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop for vec in (cdr vecs)
       when (not (eq vec-a vec)) do (return nil)
       finally (return t))))

;;----------------------------------------------------------------

(defun v/= (&rest vecs)
  "Returns either t if the two vectors are equal. 
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop for vec in (cdr vecs)
       when (eq vec-a vec) do (return nil)
       finally (return t))))

;;----------------------------------------------------------------

(defun 1+ (vec-a vec-b)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (case (length vec-a)
    (2 (v2:v+1 vec-a vec-b))
    (3 (v3:v+1 vec-a vec-b))
    (4 (v4:v+1 vec-a vec-b))))

;;----------------------------------------------------------------

(defun 1- (vec-a vec-b)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (case (length vec-a)
    (2 (v2:v-1 vec-a vec-b))
    (3 (v3:v-1 vec-a vec-b))
    (4 (v4:v-1 vec-a vec-b))))

;;----------------------------------------------------------------

(defun + (vec-a &rest vecs)
  (case (length vec-a)
    (2 (apply #'v2:v+ vecs))
    (3 (apply #'v3:v+ vecs))
    (4 (apply #'v4:v+ vecs))))

;;----------------------------------------------------------------

(defun - (vec-a &rest vecs)
  (case (length vec-a)
    (2 (apply #'v2:v- vecs))
    (3 (apply #'v3:v- vecs))
    (4 (apply #'v4:v- vecs))))

;;----------------------------------------------------------------

(defun * (vec-a scalar-or-vec)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (case (length vec-a)
    (2 (v2:v*vec vec-a multiple))
    (3 (v3:v*vec vec-a multiple))
    (4 (v4:v*vec vec-a multiple))))

;;----------------------------------------------------------------

(defun / (vec-a scalar-or-vec)
  (case (length vec-a)
    (2 (v2:v/vec vec-a multiple))
    (3 (v3:v/vec vec-a multiple))
    (4 (v4:v/vec vec-a multiple))))

;;----------------------------------------------------------------

(defun length (vec-a)
  (case (length vec-a)
    (2 (v2:vlength vec-a))
    (3 (v3:vlength vec-a))
    (4 (v4:vlength vec-a))))

;;----------------------------------------------------------------

(defun length-squared (vec-a)
  (case (length vec-a)
    (2 (v2:vlength-squared vec-a))
    (3 (v3:vlength-squared vec-a))
    (4 (v4:vlength-squared vec-a))))

;;----------------------------------------------------------------

(defun distance (vec-a vec-b)
  (case (length vec-a)
    (2 (v2:distance vec-a vec-b))
    (3 (v3:distance vec-a vec-b))
    (4 (v4:distance vec-a vec-b))))

;;----------------------------------------------------------------

(defun distance-squared (vec-a vec-b)
  (case (length vec-a)
    (2 (v2:distance-squared vec-a vec-b))
    (3 (v3:distance-squared vec-a vec-b))
    (4 (v4:distance-squared vec-a vec-b))))

;;----------------------------------------------------------------

(defun dot (vec-a vec-b)
  (case (length vec-a)
    (2 (v2:dot vec-a vec-b))
    (3 (v3:dot vec-a vec-b))
    (4 (v4:dot vec-a vec-b))))

;;----------------------------------------------------------------

(defun negate (vec)
  (case (cl:length vec)
    (2 (v2:negate vec))
    (3 (v3:negate vec))
    (4 (v4:negate vec))))

(defun face-foreward (vec-a vec-b)
  (case (cl:length vec-a)
    (2 (v2:face-foreward vec-a vec-b))
    (3 (v3:face-foreward vec-a vec-b))
    (4 (v4:face-foreward vec-a vec-b))))

;;----------------------------------------------------------------

(defun absolute-dot (vec-a vec-b)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (case (cl:length vec-a)
    (2 (v2:absolute-dot vec-a vec-b))
    (3 (v3:absolute-dot vec-a vec-b))
    (4 (v4:absolute-dot vec-a vec-b))))

;;----------------------------------------------------------------

(defun perp-dot (vec-a vec-b)
  (if (= 2 (length vec-a))
      (v2:perp-dot vec-a vec-b)
      (error "Incorrect vector size")))

;;----------------------------------------------------------------

(defun normalize (vec-a)
  (case (length vec-a)
    (2 (v2:normalize vec-a))
    (3 (v3:normalize vec-a))
    (4 (v4:normalize vec-a))))

;;----------------------------------------------------------------

(defun cross (vec-a vec-b)
  (case (length vec-a)
    (2 (v2:cross vec-a vec-b))
    (3 (v3:cross vec-a vec-b))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function ((or (simple-array single-float (2))
                               (simple-array single-float (3))
                               (simple-array single-float (4))) 
                           (or (simple-array single-float (2))
                               (simple-array single-float (3))
                               (simple-array single-float (4)))
                           (or (integer) (single-float))) 
                          (or (simple-array single-float (2))
                              (simple-array single-float (3))
                              (simple-array single-float (4)))) 
                lerp))
(defun lerp (vector-a vector-b ammount) 
  (declare ((or (simple-array single-float (2)) 
                (simple-array single-float (3))
                (simple-array single-float (4)))
            vector-a vector-b))
  (case (cl:length vector-a)
    (2 (v2:lerp vector-a vector-b (float ammount)))
    (3 (v3:lerp vector-a vector-b (float ammount)))
    (4 (v4:lerp vector-a vector-b (float ammount)))
    (otherwise (error "only vectors of size 2-4 are valid"))))

(declaim (inline mix)
         (ftype (function ((or (simple-array single-float (2))
                               (simple-array single-float (3))
                               (simple-array single-float (4))) 
                           (or (simple-array single-float (2))
                               (simple-array single-float (3))
                               (simple-array single-float (4)))
                           (or (integer) (single-float))) 
                          (or (simple-array single-float (2))
                              (simple-array single-float (3))
                              (simple-array single-float (4)))) 
                mix))
(defun mix (vector-a vector-b ammount)
  (declare ((or (simple-array single-float (2)) 
                (simple-array single-float (3))
                (simple-array single-float (4)))
            vector-a vector-b))
  (lerp vector-a vector-b ammount))

(defun bezier (a1 a2 b1 b2 ammount)
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))
