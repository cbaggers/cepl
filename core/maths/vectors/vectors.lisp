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

(define-compiler-macro swizzle (&whole form vec pattern)
  (if (keywordp pattern)
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
                                                     (error "Vectors: swizzle: Pattern component was not X, Y, Z or W: ~a" char)))))))))
      form))

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

(defun s~ (vec pattern) (swizzle vec pattern))
(define-compiler-macro s~ (vec pattern) `(swizzle ,vec ,pattern))

(defmacro dvec (var-list expression &body body)
  (when (or (not (listp var-list))
	    (not (every #'symbolp var-list))
	    (< (cl:length var-list) 1)
	    (> (cl:length var-list) 4))
    (error "dvec: invalid vector destructuring pattern ~s"
	   var-list))
  (let ((e (gensym "expression")))
    `(let ((,e ,expression))
       (let ,(loop :for v :in var-list :for i :from 0 :collect
		`(,v (aref ,e ,i)))
	 ,@body))))

(defmacro dvec* (var-list-expression-pairs &body body)
  (let ((pairs (append body
		       (utils:group var-list-expression-pairs 2))))
    (labels ((m (x y) `(dvec ,@y ,x)))
      (reduce #'m pairs))))

;;----------------------------------------------------------------

(defun make-vector (x y &optional z w)
  "This takes floats and give back a vector, this is just an
   array but it specifies the array type and populates it. "
  (cond (w (vector4:make-vector4 (float x) (float y) (float z) (float w)))
        (z (vector3:make-vector3 (float x) (float y) (float z)))
        (t (vector2:make-vector2 (float x) (float y)))))

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

(defun floatify (vec)
  (make-array (cl:length vec) :element-type 'single-float :initial-contents
              (loop :for i :across vec :collect (float i))))

;;----------------------------------------------------------------

(defun zerop (vec-a)
  "Returns t if the vector is of zero length"
  (let ((vec-a (floatify vec-a)))
    (case (cl:length vec-a)
      (2 (v2:zerop vec-a))
      (3 (v3:zerop vec-a))
      (4 (v4:zerop vec-a)))))

;;----------------------------------------------------------------

(defun unitp (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case (cl:length vec-a)
      (2 (v2:unitp vec-a))
      (3 (v3:unitp vec-a))
      (4 (v4:unitp vec-a)))))

;;----------------------------------------------------------------

(defun eql (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:eql vec-a vec-b))
      (3 (v3:eql vec-a vec-b))
      (4 (v4:eql vec-a vec-b)))))

;;----------------------------------------------------------------

(defun = (&rest vecs)
  "Returns either t if the vectors are equal.
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop :for vec :in (cdr vecs)
       :when (not (v:eql vec vec-a)) :do (return nil)
       :finally (return t))))

;;----------------------------------------------------------------

(defun /= (&rest vecs)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (let ((vec-a (first vecs)))
    (loop :for vec :in (cdr vecs)
       :when (eql vec-a vec) :do (return nil)
       :finally (return t))))

;;----------------------------------------------------------------

(defun + (&rest vecs)
  (let ((vecs (mapcar #'floatify vecs)))
    (case (cl:length (first vecs))
      (2 (apply #'v2:+ vecs))
      (3 (apply #'v3:+ vecs))
      (4 (apply #'v4:+ vecs)))))

;;----------------------------------------------------------------

(defun - (&rest vecs)
  (let ((vecs (mapcar #'floatify vecs)))
    (case (cl:length (first vecs))
      (2 (apply #'v2:- vecs))
      (3 (apply #'v3:- vecs))
      (4 (apply #'v4:- vecs)))))

;;----------------------------------------------------------------

(defun * (vec-a scalar-or-vec)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (let ((vec-a (floatify vec-a)))
    (typecase scalar-or-vec
      (number (let ((num (coerce scalar-or-vec 'single-float)))
                (case (cl:length vec-a)
                  (2 (v2:* vec-a num))
                  (3 (v3:* vec-a num))
                  (4 (v4:* vec-a num)))))
      (array (case (cl:length vec-a)
               (2 (v2:*vec vec-a scalar-or-vec))
               (3 (v3:*vec vec-a scalar-or-vec))
               (4 (v4:*vec vec-a scalar-or-vec)))))))

;;----------------------------------------------------------------

(defun / (vec-a scalar-or-vec)
  (let ((vec-a (floatify vec-a)))
    (if (typep scalar-or-vec 'number)
        (let ((scalar (float scalar-or-vec)))
          (case (cl:length vec-a)
            (2 (v2:/ vec-a scalar))
            (3 (v3:/ vec-a scalar))
            (4 (v4:/ vec-a scalar))))
        (let ((vec-b (floatify scalar-or-vec)))
          (case (cl:length vec-a)
            (2 (v2:/vec vec-a vec-b))
            (3 (v3:/vec vec-a vec-b))
            (4 (v4:/vec vec-a vec-b)))))))

;;----------------------------------------------------------------

(defun length (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case (cl:length vec-a)
      (2 (v2:length vec-a))
      (3 (v3:length vec-a))
      (4 (v4:length vec-a)))))

;;----------------------------------------------------------------

(defun length-squared (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case (cl:length vec-a)
     (2 (v2:length-squared vec-a))
     (3 (v3:length-squared vec-a))
     (4 (v4:length-squared vec-a)))))

;;----------------------------------------------------------------

(defun distance (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:distance vec-a vec-b))
      (3 (v3:distance vec-a vec-b))
      (4 (v4:distance vec-a vec-b)))))

;;----------------------------------------------------------------

(defun distance-squared (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:distance-squared vec-a vec-b))
      (3 (v3:distance-squared vec-a vec-b))
      (4 (v4:distance-squared vec-a vec-b)))))

;;----------------------------------------------------------------

(defun dot (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:dot vec-a vec-b))
      (3 (v3:dot vec-a vec-b))
      (4 (v4:dot vec-a vec-b)))))

;;----------------------------------------------------------------

(defun negate (vec)
  (let ((vec (floatify vec)))
    (case (cl:length vec)
      (2 (v2:negate vec))
      (3 (v3:negate vec))
      (4 (v4:negate vec)))))

(defun face-foreward (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:face-foreward vec-a vec-b))
      (3 (v3:face-foreward vec-a vec-b))
      (4 (v4:face-foreward vec-a vec-b)))))

;;----------------------------------------------------------------

(defun absolute-dot (vec-a vec-b)
  "Adds two vectors together and returns the result as a new vector of the same type"
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:absolute-dot vec-a vec-b))
      (3 (v3:absolute-dot vec-a vec-b))
      (4 (v4:absolute-dot vec-a vec-b)))))

;;----------------------------------------------------------------

(defun perp-dot (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (if (= 2 (length vec-a))
        (v2:perp-dot vec-a vec-b)
        (error "Incorrect vector size"))))

;;----------------------------------------------------------------

(defun normalize (vec-a)
  (let ((vec-a (floatify vec-a)))
    (case (cl:length vec-a)
      (2 (v2:normalize vec-a))
      (3 (v3:normalize vec-a))
      (4 (v4:normalize vec-a)))))

;;----------------------------------------------------------------

(defun cross (vec-a vec-b)
  (let ((vec-a (floatify vec-a))
        (vec-b (floatify vec-b)))
    (case (cl:length vec-a)
      (2 (v2:cross vec-a vec-b))
      (3 (v3:cross vec-a vec-b)))))

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

;----------------------------------------------------------------

;; {TODO} compiler macro these
(defun x (vec)
  "Returns the x component of the vector"
  (aref vec 0))
(defun y (vec)
  "Returns the y component of the vector"
  (aref vec 1))
(defun z (vec)
  "Returns the z component of the vector"
  (aref vec 2))
(defun w (vec)
  "Returns the w component of the vector"
  (aref vec 3))

(defun (setf x) (value vec)
  "Sets the x component of the vector"
  (setf (aref vec 0) (float value)))
(defun (setf y) (value vec)
  "Sets the y component of the vector"
  (setf (aref vec 1) (float value)))
(defun (setf z) (value vec)
  "Sets the z component of the vector"
  (setf (aref vec 2) (float value)))
(defun (setf w) (value vec)
  "Sets the w component of the vector"
  (setf (aref vec 3) (float value)))

;;----------------------------------------------------------------

(varjo:v-defun vectors:x (a) "~a.x" (v-vector) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:y (a) "~a.y" (v-vector) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-vec3)  (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-bvec3) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-ivec3) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-uvec3) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-dvec3) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-vec4)  (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-bvec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-ivec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-uvec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:z (a) "~a.z" (v-dvec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:w (a) "~a.w" (v-vec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:w (a) "~a.w" (v-bvec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:w (a) "~a.w" (v-ivec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:w (a) "~a.w" (v-uvec4) (:element 0) :glsl-spec-matching t)
(varjo:v-defun vectors:w (a) "~a.w" (v-dvec4) (:element 0) :glsl-spec-matching t)
