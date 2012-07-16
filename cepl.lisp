;;;; cepl.lisp

(in-package :cepl)

;; right now this is just functions we use across demos.
;; later this will be formalized when I draw up a decent 
;; design for the api/whatever-this-is

;;;--------------------------------------------------------------

(defmacro continuable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case 
       (progn ,@body)
     (continue () :report "Continue")))

;;;--------------------------------------------------------------

(defun draw-elements-base-vertex (mode array indices base-vertex
			    &key (count (gl::gl-array-size array)))
  (%gl:draw-elements-base-vertex mode count
                     (gl::cffi-type-to-gl (gl::gl-array-type array))
                     (gl::gl-array-pointer-offset array indices)
		     base-vertex))


(defun calculate-frustrum-scale (field-of-view-degrees)
  (/ 1.0 (tan (/ (* field-of-view-degrees base-maths:+one-degree-in-radians+) 2.0))))

;;;------------------------------------------------------------------

(defmacro with-init-cepl (&body body)
  `(progn 
     ;;(setf *cepl-* nil)
     (sdl:with-init ()
       (,@body))))
