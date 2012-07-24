;;;; cepl.lisp

(in-package :cepl)

;; right now this is just functions we use across demos.
;; later this will be formalized when I draw up a decent 
;; design for the api/whatever-this-is

;;;--------------------------------------------------------------

(defun draw-elements-base-vertex (mode array indices base-vertex
			    &key (count (gl::gl-array-size array)))
  (%gl:draw-elements-base-vertex mode count
                     (gl::cffi-type-to-gl (gl::gl-array-type array))
                     (gl::gl-array-pointer-offset array indices)
		     base-vertex))


(defun calculate-frustrum-scale (field-of-view-degrees)
  (/ 1.0 (tan (/ (* field-of-view-degrees base-maths:+one-degree-in-radians+) 2.0))))


(defun make-cam-clip-matrix (frustrum-scale)
  (let* ((f-near 1.0)
	 (f-far 45.0)
	 (f-scale frustrum-scale))
    (matrix4:make-matrix4 f-scale 0.0 0.0 0.0
			  0.0 f-scale 0.0 0.0
			  0.0 0.0 (/ (+ f-far f-near)
				     (- f-near f-far)) -1.0
			  0.0 0.0 (/ (* 2 f-far f-near)
				     (- f-near f-far)) 0.0)))

;;;------------------------------------------------------------------

(defmacro with-init-cepl (&body body)
  `(progn 
     ;;(setf *cepl-* nil)
     (sdl:with-init ()
       (,@body))))
