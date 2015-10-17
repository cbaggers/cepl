;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This package is for all the vector4 math functions
;; There will be a generic function-set to make this as easy
;; as possible for people writing the games but this will
;; be in a seperate package (prehaps the base-maths one)

(in-package #:vector4)


;;; [TODO] make destructive versions aswell
;;; looking at some existing code the desctructive versions
;;; end up being comparitively fast to C.
;;; see http://stackoverflow.com/questions/8356494/efficient-vector-operations-of-linear-algebra-in-common-lisp-especially-sbcl for more details

;;; Also see http://nklein.com/2009/06/speedy-matrix-multiplication-in-lisp-again/ for a nice guide to declaim and its effects.

;;; Annoyingly as everything else is reliant on the speed of the
;;; vector math I have to make this bit fast, this seems to mean
;;; hardcoding for set structures sizes. I hope I'm wrong and I
;;; can use loops and have lisp be clever enough to optomize it
;;; but for now I'm not counting on it
;;; Also we will be making use of declaim for inlining and also
;;; for forcing float type on everything.

;;; vector4 operations

;;----------------------------------------------------------------

(declaim (inline make-vector4)
         (ftype (function ((single-float)
                           (single-float)
                           (single-float)
                           (single-float))
                          (simple-array single-float (4)))
                make-vector4))
(defun make-vector4 (x y z w)
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (single-float x y z w))
  (let ((vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z
          (aref vec 3) w)
    vec))

;;----------------------------------------------------------------

;; Not sure what I'm going to do with these. I don't belive this
;; is the best way to do this as it doesnt give a new vector
;; not that lispy
(defvar *unit-x* (vector 1.0 0.0 0.0 0.0))
(defvar *unit-y* (vector 0.0 1.0 0.0 0.0))
(defvar *unit-z* (vector 0.0 0.0 1.0 0.0))
(defvar *unit-w* (vector 0.0 0.0 0.0 1.0))
(defvar *unit-scale* (vector 1.0 1.0 1.0 1.0))
(defvar *origin* (vector 0.0 0.0 0.0 0.0))

;;----------------------------------------------------------------

;;[TODO] What is faster (cl:* x x) or (expt x 2) ?
(declaim (inline zerop)
         (ftype (function ((simple-array single-float (4)))
                          (boolean)) zerop))
(defun zerop (vector-a)
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare ((simple-array single-float (4)) vector-a))
  (float-zero (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
                    (EXPT (AREF VECTOR-A 2) 2) (EXPT (AREF VECTOR-A 3) 2))))

;;----------------------------------------------------------------

(declaim (inline unitp)
         (ftype (function ((simple-array single-float (4)))
                          (boolean)) unitp))
(defun unitp (vector-a)
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare ((simple-array single-float (4)) vector-a))
  (float-zero (cl:- 1.0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
                              (EXPT (AREF VECTOR-A 2) 2) (EXPT (AREF VECTOR-A 3) 2)))))
;;----------------------------------------------------------------

;; Would be interesting to see if checking that the arrays
;; are not 'eql' first would speed this up
(declaim (inline eql)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (boolean)) eql))
(defun eql (vector-a vector-b)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (AND (cl:= (AREF VECTOR-A 0) (AREF VECTOR-B 0))
       (cl:= (AREF VECTOR-A 1) (AREF VECTOR-B 1))
       (cl:= (AREF VECTOR-A 2) (AREF VECTOR-B 2))
       (cl:= (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(defun + (&rest vec4s)
  "takes any number of vectors and add them all together
   returning a new vector"
  (reduce #'%+ vec4s))

;;----------------------------------------------------------------

(declaim (inline %+)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (simple-array single-float (4))) %+))
(defun %+ (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (MAKE-VECTOR4 (cl:+ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
                (cl:+ (AREF VECTOR-A 1) (AREF VECTOR-B 1))
                (cl:+ (AREF VECTOR-A 2) (AREF VECTOR-B 2))
                (cl:+ (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(defun - (&rest vec4s)
  "takes any number of vectors and subtract them and return
   a new vector4"
  (reduce #'%- vec4s))

;;----------------------------------------------------------------

(declaim (inline %-)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (simple-array single-float (4))) %-))
(defun %- (vector-a vector-b)
  "Subtract two vectors and return a new vector containing
   the result"
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (MAKE-VECTOR4 (cl:- (AREF VECTOR-A 0) (AREF VECTOR-B 0))
                (cl:- (AREF VECTOR-A 1) (AREF VECTOR-B 1))
                (cl:- (AREF VECTOR-A 2) (AREF VECTOR-B 2))
                (cl:- (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline *)
         (ftype (function ((simple-array single-float (4))
                           (single-float))
                          (simple-array single-float (4))) *))
(defun * (vector-a a)
  "Multiply vector by scalar"
  (declare ((simple-array single-float (4)) vector-a)
           ((single-float) a))
  (MAKE-VECTOR4 (cl:* (AREF VECTOR-A 0) A) (cl:* (AREF VECTOR-A 1) A)
                (cl:* (AREF VECTOR-A 2) A) (cl:* (AREF VECTOR-A 3) A)))

;;----------------------------------------------------------------

(declaim (inline v3*)
         (ftype (function ((simple-array single-float (4))
                           (single-float))
                          (simple-array single-float (4))) v3*))
(defun v3* (vector-a a)
  "Multiply vector by scalar"
  (declare ((simple-array single-float (4)) vector-a)
           ((single-float) a))
  (make-vector4 (cl:* (aref vector-a 0) a) (cl:* (aref vector-a 1) a)
                (cl:* (aref vector-a 2) a) (aref vector-a 3)))

;;----------------------------------------------------------------

(declaim (inline *vec)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (simple-array single-float (4)))
                *vec))
(defun *vec (vector-a vector-b)
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (MAKE-VECTOR4 (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
                (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))
                (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2))
                (cl:* (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline /)
         (ftype (function ((simple-array single-float (4))
                           (single-float))
                          (simple-array single-float (4))) /))
(defun / (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare ((simple-array single-float (4)) vector-a)
           ((single-float) a))
  (let ((b (cl:/ 1 a)))
    (MAKE-VECTOR4 (cl:* (AREF VECTOR-A 0) B) (cl:* (AREF VECTOR-A 1) B)
                  (cl:* (AREF VECTOR-A 2) B) (cl:* (AREF VECTOR-A 3) B))))

;;----------------------------------------------------------------

(declaim (inline /vec)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (simple-array single-float (4)))
                /vec))
(defun /vec (vector-a vector-b)
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (MAKE-VECTOR4 (cl:/ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
                (cl:/ (AREF VECTOR-A 1) (AREF VECTOR-B 1))
                (cl:/ (AREF VECTOR-A 2) (AREF VECTOR-B 2))
                (cl:/ (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline negate)
         (ftype (function ((simple-array single-float (4)))
                          (simple-array single-float (4)))
                negate))
(defun negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare ((simple-array single-float (4)) vector-a))
  (MAKE-VECTOR4 (cl:- (AREF VECTOR-A 0)) (cl:- (AREF VECTOR-A 1)) (cl:- (AREF VECTOR-A 2))
                (cl:- (AREF VECTOR-A 3))))

;;----------------------------------------------------------------

(declaim (inline face-foreward)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (simple-array single-float (4)))
                face-foreward))
(defun face-foreward (vector-a vector-b)
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (if (> (print (dot vector-a vector-b)) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(declaim (inline length-squared)
         (ftype (function ((simple-array single-float (4)))
                          (single-float)) length-squared))
(defun length-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare ((simple-array single-float (4)) vector-a))
  (let ((x (v-x vector-a))
        (y (v-y vector-a))
        (z (v-z vector-a))
        (w (v-w vector-a)))
    (cl:+ (cl:* x x) (cl:* y y) (cl:* z z) (cl:* w w))))

;;----------------------------------------------------------------

(declaim (inline length)
         (ftype (function ((simple-array single-float (4)))
                          (single-float)) length))
(defun length (vector-a)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare ((simple-array single-float (4)) vector-a))
  (c-sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance-squared)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (single-float))
                distance-squared))
(defun distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (single-float))
                distance))
(defun distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a c-sqrt and thus is
   faster."
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (c-sqrt (distance-squared vector-a vector-b)))

;;----------------------------------------------------------------

(declaim (inline dot)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (single-float))
                dot))
(defun dot (vector-a vector-b)
  "Return the dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (cl:+ (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
        (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))
        (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2))
        (cl:* (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline absolute-dot)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4)))
                          (single-float))
                absolute-dot))
(defun absolute-dot (vector-a vector-b)
  "Return the absolute dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (cl:+ (ABS (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0)))
        (ABS (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1)))
        (ABS (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2)))
        (ABS (cl:* (AREF VECTOR-A 3) (AREF VECTOR-B 3)))))

;;----------------------------------------------------------------

;; [TODO] shouldnt this return a zero vector in event of zero
;; length? does it matter?
(declaim (inline normalize)
         (ftype (function ((simple-array single-float (4)))
                          (simple-array single-float (4)))
                normalize))
(defun normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare ((simple-array single-float (4)) vector-a))
  (let ((len (length-squared vector-a)))
    (if (float-zero len)
        vector-a
        (* vector-a (c-inv-sqrt len)))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4))
                           (single-float))
                          (simple-array single-float (4)))
                lerp))
(defun lerp (vector-a vector-b ammount)
  (declare ((simple-array single-float (4)) vector-a vector-b))
  (%+ vector-a (* (%- vector-b vector-a) ammount)))

;;----------------------------------------------------------------

(declaim (inline bezier)
         (ftype (function ((simple-array single-float (4))
                           (simple-array single-float (4))
                           (simple-array single-float (4))
                           (simple-array single-float (4))
                           (single-float))
                          (simple-array single-float (4)))
                bezier))
(defun bezier (a1 a2 b1 b2 ammount)
  (declare ((simple-array single-float (4)) a1 a2 b1 b2)
           ((single-float) ammount))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make-vector4 (maths:spline x (mapcar #'v-x knots))
                (maths:spline x (mapcar #'v-y knots))
                (maths:spline x (mapcar #'v-z knots))
                (maths:spline x (mapcar #'v-w knots))))
