;;; This package is for all the vector3 math functions

(in-package #:cepl-vec3)

;;; Maths bits, not in the right place but I just need
;;; to start writing.

;;; need a zero, unit-x, unit-y, unit-z, negative of all units
;;; & unit scale (all 1's)

;;; need to inline
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

(defun vec3 (x y z)
  (vector x y z))

(defun c-= (u v)
  (and (= (svref u 0) (svref v 0))
       (= (svref u 1) (svref v 1))
       (= (svref u 1) (svref v 2))))

(defun c-/= (u v)
  (or (/= (svref u 0) (svref v 0))
      (/= (svref u 1) (svref v 1))
      (/= (svref u 1) (svref v 2)))) 

(defun c-+ (&rest vec3s)
  (reduce #'c-+1 vec3s))

(defun c-+1 (u v)
  (vector (+ (svref u 0) (svref v 0))
	  (+ (svref u 1) (svref v 1))
	  (+ (svref u 2) (svref v 2))))

(defun c-- (&rest vec3s)
  (reduce #'c--1 vec3s))

(defun c--1 (u v)
  (vector (- (svref u 0) (svref v 0))
	  (- (svref u 1) (svref v 1))
	  (- (svref u 2) (svref v 2))))

(defun c-* (u a)
  (vector (* (svref u 0) a)
	  (* (svref u 1) a)
	  (* (svref u 2) a)))

(defun c-vec* (u v) 
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (vector (* (svref u 0) (svref v 0))
	  (* (svref u 1) (svref v 1))
	  (* (svref u 2) (svref v 2))))

;;; may just be an evil side effect of ripping this off from
;;; from ogre but some of the optomisations will be coming over
;;; as well, this may not be relevent in lisp, I will see later
;;; on.
(defun c-/ (u a)
  (let ((b (/ 1 a)))
    (vector (* (svref u 0) b)
	    (* (svref u 1) b)
	    (* (svref u 2) b))))

(defun c-vec/ (u v) 
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (vector (/ (svref u 0) (svref v 0))
	  (/ (svref u 1) (svref v 1))
	  (/ (svref u 2) (svref v 2))))

(defun c-negate (u)
  (vector (- (svref u 0))
	  (- (svref u 1))
	  (- (svref u 2))))

(defun c-length-squared (u)
  (let ((x (svref u 0))
	(y (svref u 1))
	(z (svref u 2)))
    (+ (* x x) (* y y) (* z z))))

(defun c-length (u)
  "If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation.
   Also c-length-squared should be inlined inside here
   eventually. "
  (sqrt (c-length-squared u)))

(defun c-distance-squared (u v)
  "finds the squared distance between 2 points defined by vectors
   u & v"
  (c-length-squared (c-- v u)))

(defun c-distance (u v)
  (sqrt (c-distance-squared u v)))

(defun c-dot (u v)
  (let ((ux (svref u 0))
	(uy (svref u 1))
	(uz (svref u 2))
	(vx (svref v 0))
	(vy (svref v 1))
	(vz (svref v 2)))
    (+ (* ux vx) (* uy vy) (* uz vz))))

(defun c-absolute-dot (u v) 
  (let ((ux (svref u 0))
	(uy (svref u 1))
	(uz (svref u 2))
	(vx (svref v 0))
	(vy (svref v 1))
	(vz (svref v 2)))
    (+ (abs (* ux vx)) (abs (* uy vy)) (abs (* uz vz)))))

(defun c-normalize (u)
  (let ((a (c-length u))) 
    (if (> a 1e-08) 
	(c-/ u a)
	u)))

(defun c-cross (u v)
  "Calculates the cross-product of 2 vectors, i.e. the vector 
that lies perpendicular to them both. The resultign vector
 will <b>NOT</b> be normalised, to maximise efficiency
The returned vector will be on the side from which the arc 
from u to v is anticlockwise.
This is because CEPL uses a right-handed coordinate system."
  (let ((ux (svref u 0))
	(uy (svref u 1))
	(uz (svref u 2))
	(vx (svref v 0))
	(vy (svref v 1))
	(vz (svref v 2)))
    (vector (- (* uy vz) (* uz vy))
	    (- (* uz vx) (* ux vz))
	    (- (* ux vy) (* uy vx)))))


