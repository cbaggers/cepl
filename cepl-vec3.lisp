;;; This package is for all the vector3 math functions

(in-package #:cepl-vec3)

;;; Maths bits, not in the right place but I just need
;;; to start writing.

;;; Need to inline
;;; we will make destructive and no destructive versions,
;;; looking at some existing code the desctructive versions
;;; end up being comparitively fast to C. I will use -d as local
;;; parlance for destructive.
;;; see http://stackoverflow.com/questions/8356494/efficient-vector-operations-of-linear-algebra-in-common-lisp-especially-sbcl for more details
;;; Also see http://nklein.com/2009/06/speedy-matrix-multiplication-in-lisp-again/ for a nice guide to declaim and its effects.

;;; Annoyingly as everything else is reliant on the speed of the 
;;; vector math I have to make this bit fast, this seems to mean
;;; hardcoding for set structures sizes. I hope I'm wrong and I
;;; can use loops and have lisp be clever enough to optomize it
;;; but for now I'm not counting on it
;;; Also we will be making use of declaim for inlining and also 
;;; for forcing float type on everything.

;;; vector3 operations

;;; need a zero, unit-x, unit-y, unit-z, negative of all units
;;; & unit scale (all 1's)

;----------------------------------------------------------------

(declaim (inline make-vector3)
	 (ftype (function ((single-float) 
			   (single-float) 
			   (single-float)) 
			  (simple-array single-float (3))) 
		make-vector3))
(defun make-vector3 (x y z)
  "This takes 3 floats and give back a vector3, this is just an
   array but it specifies the array type and populates it. 
   For speed reasons it will not accept integers so make sure 
   you hand it floats."
  (declare (single-float x y z))
  (let (( vec (make-array 3 :element-type `single-float)))
    (setf (aref vec 0) x
	  (aref vec 1) y
	  (aref vec 2) z)
    vec))

;----------------------------------------------------------------

;; Not sure what I'm going to do with these. I don't belive this
;; is the best way to do this as it doesnt give a new vector
;; not that lispy
(defparameter *unit-x* (vector 1.0 0.0 0.0))
(defparameter *unit-y* (vector 0.0 1.0 0.0))
(defparameter *unit-z* (vector 0.0 0.0 1.0))
(defparameter *unit-scale* (vector 1.0 1.0 1.0))

;----------------------------------------------------------------

;; These have been defined as macros as it want to guarantee they
;; are 'inlined' as it were and also to avoid any possible cost 
;; of the implied let. THe 'implied let' thing is an assumption 
;; on my part from something I read in "ansi common lisp" 
;; (declaim (inline single?))
;; (defun single? (lst)
;;   (and (consp lst) (null (cdr lst))))
;; (defun foo (x)
;;   (single? (bar x)))
;; is equivilent to 
;; (defun foo (x)
;;   (let ((lst (bar x)))
;;     (and (consp lst) (bull (cdr lst)))))
;; Final justification is that it's purely for syntatic clarity
;; and not for any computational reason.
(defmacro c-x (vec)
  `(aref ,vec 0))

(defmacro c-y (vec)
  `(aref ,vec 1))

(defmacro c-z (vec)
  `(aref ,vec 2))

;----------------------------------------------------------------

;; Would be interesting to see if checking that the arrays
;; are not 'eq' first would speed this up 
(declaim (inline c-=)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (boolean)) c-=))
(defun c-= (vector-a vector-b)
  "Returns either t if the two vectors are equal. 
   Otherwise it returns nil."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (and (= (c-x vector-a) (c-x vector-b))
       (= (c-y vector-a) (c-y vector-b))
       (= (c-z vector-a) (c-z vector-b))))

;----------------------------------------------------------------

(declaim (inline c-/=)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (boolean)) c-/=))
(defun c-/= (vector-a vector-b)
  (declare ((simple-array single-float (3)) vector-a vector-b))
  "Returns either t if the two vectors are not equal. 
   Otherwise it returns nil."
  (or (/= (c-x vector-a) (c-x vector-b))
      (/= (c-y vector-a) (c-y vector-b))
      (/= (c-z vector-a) (c-z vector-b)))) 

;----------------------------------------------------------------

;; Not sure how to optomise this
(defun c-+ (&rest vec3s)
  (reduce #'c-+1 vec3s))

;----------------------------------------------------------------

(declaim (inline c-+1)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) c-+1))
(defun c-+1 (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (+ (c-x vector-a) (c-x vector-b))
		(+ (c-y vector-a) (c-y vector-b))
		(+ (c-z vector-a) (c-z vector-b))))

;----------------------------------------------------------------

;; Not sure how to optomise this
(defun c-- (&rest vec3s)
  "minus list of vector3s"
  (reduce #'c--1 vec3s))

;----------------------------------------------------------------

(declaim (inline c--1)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) c--1))
(defun c--1 (vector-a vector-b)
  "Subtract two vectors and return a new vector containing 
   the result"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (- (c-x vector-a) (c-x vector-b))
		(- (c-y vector-a) (c-y vector-b))
		(- (c-z vector-a) (c-z vector-b))))

;----------------------------------------------------------------

(declaim (inline c-*)
	 (ftype (function ((simple-array single-float (3)) 
			   (single-float)) 
			  (simple-array single-float (3))) c-*))
(defun c-* (vector-a a)
  "Multiply vector by scalar"
  (declare ((simple-array single-float (3)) vector-a)
	   ((single-float) a))
  (make-vector3 (* (c-x vector-a) a)
		(* (c-y vector-a) a)
		(* (c-z vector-a) a)))

;----------------------------------------------------------------

(declaim (inline c-*vec)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		c-*vec))
(defun c-*vec (vector-a vector-b) 
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (* (c-x vector-a) (c-x vector-b))
		(* (c-y vector-a) (c-y vector-b))
		(* (c-z vector-a) (c-z vector-b))))

;----------------------------------------------------------------

;;; may just be an evil side effect of ripping this off from
;;; from ogre but some of the optomisations will be coming over
;;; as well, this may not be relevent in lisp, I will see later
;;; on.
(declaim (inline c-/)
	 (ftype (function ((simple-array single-float (3)) 
			   (single-float)) 
			  (simple-array single-float (3))) c-/))
(defun c-/ (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare ((simple-array single-float (3)) vector-a)
	   ((single-float) a))
  (let ((b (/ 1 a)))
    (make-vector3 (* (c-x vector-a) b)
		  (* (c-y vector-a) b)
		  (* (c-z vector-a) b))))

;----------------------------------------------------------------

(declaim (inline c-/vec)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		c-/vec))
(defun c-/vec (vector-a vector-b) 
  "Divides components, not sure what, i'll need this for 
   yet but hey!"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (/ (c-x vector-a) (c-x vector-b))
		(/ (c-y vector-a) (c-y vector-b))
		(/ (c-z vector-a) (c-z vector-b))))

;----------------------------------------------------------------

(declaim (inline c-negate)
	 (ftype (function ((simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		c-negate))
(defun c-negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare ((simple-array single-float (3)) vector-a))
  (make-vector3 (- (c-x vector-a))
		(- (c-y vector-a))
		(- (c-z vector-a))))

;----------------------------------------------------------------

(declaim (inline c-length-squared)
	 (ftype (function ((simple-array single-float (3))) 
			  (single-float)) c-length-squared))
(defun c-length-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare ((simple-array single-float (3)) vector-a))
  (let ((x (c-x vector-a))
	(y (c-y vector-a))
	(z (c-z vector-a)))
    (+ (* x x) (* y y) (* z z))))

;----------------------------------------------------------------

(declaim (inline c-length)
	 (ftype (function ((simple-array single-float (3))) 
			  (single-float)) c-length))
(defun c-length (vector-a)
  "If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare ((simple-array single-float (3)) vector-a))
  (sqrt (c-length-squared vector-a)))

;----------------------------------------------------------------

(declaim (inline c-distance-squared)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		c-distance-squared))
(defun c-distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (c-length-squared (c-- vector-b vector-a)))

;----------------------------------------------------------------

(declaim (inline c-distance)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		c-distance))
(defun c-distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors 
   vector-a & vector-b. If comparing distances, use 
   c-distance-squared as it desnt require a sqrt and thus is 
   faster."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (sqrt (c-distance-squared vector-a vector-b)))

;----------------------------------------------------------------

(declaim (inline c-dot)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		c-dot))
(defun c-dot (vector-a vector-b)
  "Return the dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (let ((ux (c-x vector-a))
	(uy (c-y vector-a))
	(uz (c-z vector-a))
	(vx (c-x vector-b))
	(vy (c-y vector-b))
	(vz (c-z vector-b)))
    (+ (* ux vx) (* uy vy) (* uz vz))))

;----------------------------------------------------------------

(declaim (inline c-absolute-dot)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		c-absolute-dot))
(defun c-absolute-dot (vector-a vector-b) 
  "Return the absolute dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (+ (abs (* (c-x vector-a) (c-x vector-b))) 
     (abs (* (c-y vector-a) (c-y vector-b))) 
     (abs (* (c-z vector-a) (c-z vector-b)))))

;----------------------------------------------------------------

(declaim (inline c-normalize)
	 (ftype (function ((simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		c-normalize))
(defun c-normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error and assumes any length less
   than 1e-08 is 0. THis makes sense with floating point
   inaccuracies."
  (declare ((simple-array single-float (3)) vector-a))
  (let ((a (c-length vector-a))) 
    (if (> a 1e-08) 
	(c-/ vector-a a)
	vector-a)))

;----------------------------------------------------------------

(declaim (inline c-cross)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3)))
		c-cross))
(defun c-cross (vector-a vector-b)
  "Calculates the cross-product of 2 vectors, i.e. the vector 
   that lies perpendicular to them both. The resultign vector
   will <b>NOT</b> be normalised, to maximise efficiency
   The returned vector will be on the side from which the arc 
   from u to v is anticlockwise.
   This is because CEPL uses a right-handed coordinate system."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (let ((ux (c-x vector-a))
	(uy (c-y vector-a))
	(uz (c-z vector-a))
	(vx (c-x vector-b))
	(vy (c-y vector-b))
	(vz (c-z vector-b)))
    (make-vector3 (- (* uy vz) (* uz vy))
		  (- (* uz vx) (* ux vz))
		  (- (* ux vy) (* uy vx)))))


