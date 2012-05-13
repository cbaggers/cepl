;;; This package is for all the vector3 math functions

(in-package #:vector3)

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
(defmacro v-x (vec)
  `(aref ,vec 0))

(defmacro v-y (vec)
  `(aref ,vec 1))

(defmacro v-z (vec)
  `(aref ,vec 2))

;----------------------------------------------------------------

;; Would be interesting to see if checking that the arrays
;; are not 'eq' first would speed this up 
(declaim (inline v=)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (boolean)) v=))
(defun v= (vector-a vector-b)
  "Returns either t if the two vectors are equal. 
   Otherwise it returns nil."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (and (= (v-x vector-a) (v-x vector-b))
       (= (v-y vector-a) (v-y vector-b))
       (= (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

(declaim (inline v/=)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (boolean)) v/=))
(defun v/= (vector-a vector-b)
  (declare ((simple-array single-float (3)) vector-a vector-b))
  "Returns either t if the two vectors are not equal. 
   Otherwise it returns nil."
  (or (/= (v-x vector-a) (v-x vector-b))
      (/= (v-y vector-a) (v-y vector-b))
      (/= (v-z vector-a) (v-z vector-b)))) 

;----------------------------------------------------------------

;; Not sure how to optomise this
(defun v+ (&rest vec3s)
  (reduce #'v+1 vec3s))

;----------------------------------------------------------------

(declaim (inline v+1)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) v+1))
(defun v+1 (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (+ (v-x vector-a) (v-x vector-b))
		(+ (v-y vector-a) (v-y vector-b))
		(+ (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

;; Not sure how to optomise this
(defun v- (&rest vec3s)
  "minus list of vector3s"
  (reduce #'v-1 vec3s))

;----------------------------------------------------------------

(declaim (inline v-1)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) v-1))
(defun v-1 (vector-a vector-b)
  "Subtract two vectors and return a new vector containing 
   the result"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (- (v-x vector-a) (v-x vector-b))
		(- (v-y vector-a) (v-y vector-b))
		(- (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

(declaim (inline v*)
	 (ftype (function ((simple-array single-float (3)) 
			   (single-float)) 
			  (simple-array single-float (3))) v*))
(defun v* (vector-a a)
  "Multiply vector by scalar"
  (declare ((simple-array single-float (3)) vector-a)
	   ((single-float) a))
  (make-vector3 (* (v-x vector-a) a)
		(* (v-y vector-a) a)
		(* (v-z vector-a) a)))

;----------------------------------------------------------------

(declaim (inline v*vec)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		v*vec))
(defun v*vec (vector-a vector-b) 
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (* (v-x vector-a) (v-x vector-b))
		(* (v-y vector-a) (v-y vector-b))
		(* (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

;;; may just be an evil side effect of ripping this off from
;;; from ogre but some of the optomisations will be coming over
;;; as well, this may not be relevent in lisp, I will see later
;;; on.
(declaim (inline v/)
	 (ftype (function ((simple-array single-float (3)) 
			   (single-float)) 
			  (simple-array single-float (3))) v/))
(defun v/ (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare ((simple-array single-float (3)) vector-a)
	   ((single-float) a))
  (let ((b (/ 1 a)))
    (make-vector3 (* (v-x vector-a) b)
		  (* (v-y vector-a) b)
		  (* (v-z vector-a) b))))

;----------------------------------------------------------------

(declaim (inline v/vec)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		v/vec))
(defun v/vec (vector-a vector-b) 
  "Divides components, not sure what, i'll need this for 
   yet but hey!"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (make-vector3 (/ (v-x vector-a) (v-x vector-b))
		(/ (v-y vector-a) (v-y vector-b))
		(/ (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

(declaim (inline negate)
	 (ftype (function ((simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		negate))
(defun negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare ((simple-array single-float (3)) vector-a))
  (make-vector3 (- (v-x vector-a))
		(- (v-y vector-a))
		(- (v-z vector-a))))

;----------------------------------------------------------------

(declaim (inline vlength-squared)
	 (ftype (function ((simple-array single-float (3))) 
			  (single-float)) vlength-squared))
(defun vlength-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare ((simple-array single-float (3)) vector-a))
  (let ((x (v-x vector-a))
	(y (v-y vector-a))
	(z (v-z vector-a)))
    (+ (* x x) (* y y) (* z z))))

;----------------------------------------------------------------

(declaim (inline vlength)
	 (ftype (function ((simple-array single-float (3))) 
			  (single-float)) vlength))
(defun vlength (vector-a)
  "If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare ((simple-array single-float (3)) vector-a))
  (sqrt (vlength-squared vector-a)))

;----------------------------------------------------------------

(declaim (inline distance-squared)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		distance-squared))
(defun distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (vlength-squared (v- vector-b vector-a)))

;----------------------------------------------------------------

(declaim (inline distance)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		distance))
(defun distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors 
   vector-a & vector-b. If comparing distances, use 
   c-distance-squared as it desnt require a sqrt and thus is 
   faster."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (sqrt (distance-squared vector-a vector-b)))

;----------------------------------------------------------------

(declaim (inline dot)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		dot))
(defun dot (vector-a vector-b)
  "Return the dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (+ (* (v-x vector-a) (v-x vector-b)) 
     (* (v-y vector-a) (v-y vector-b)) 
     (* (v-z vector-a) (v-z vector-b))))

;----------------------------------------------------------------

(declaim (inline absolute-dot)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (single-float)) 
		absolute-dot))
(defun absolute-dot (vector-a vector-b) 
  "Return the absolute dot product of the vector-a and vector-b."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (+ (abs (* (v-x vector-a) (v-x vector-b))) 
     (abs (* (v-y vector-a) (v-y vector-b))) 
     (abs (* (v-z vector-a) (v-z vector-b)))))

;----------------------------------------------------------------

(declaim (inline normalize)
	 (ftype (function ((simple-array single-float (3))) 
			  (simple-array single-float (3))) 
		normalize))
(defun normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error and assumes any length less
   than 1e-08 is 0. THis makes sense with floating point
   inaccuracies."
  (declare ((simple-array single-float (3)) vector-a))
  (let ((a (vlength vector-a))) 
    (if (> a 1e-08) 
	(v/ vector-a a)
	vector-a)))

;----------------------------------------------------------------

(declaim (inline cross)
	 (ftype (function ((simple-array single-float (3)) 
			   (simple-array single-float (3))) 
			  (simple-array single-float (3)))
		cross))
(defun cross (vector-a vector-b)
  "Calculates the cross-product of 2 vectors, i.e. the vector 
   that lies perpendicular to them both. The resultign vector
   will <b>NOT</b> be normalised, to maximise efficiency
   The returned vector will be on the side from which the arc 
   from u to v is anticlockwise.
   This is because CEPL uses a right-handed coordinate system.
   Another note on the cross product is that if vector-a and 
   vector-b are normalized the length of the resulting vector
   will be sin(a) where a is the angle between the two vectors.
   The fact that we don't normalize may be useful in our 
   quaternion functions later on."
  (declare ((simple-array single-float (3)) vector-a vector-b))
  (let ((ux (v-x vector-a))
	(uy (v-y vector-a))
	(uz (v-z vector-a))
	(vx (v-x vector-b))
	(vy (v-y vector-b))
	(vz (v-z vector-b)))
    (make-vector3 (- (* uy vz) (* uz vy))
		  (- (* uz vx) (* ux vz))
		  (- (* ux vy) (* uy vx)))))


