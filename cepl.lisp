;;;; cepl.lisp

(in-package :cepl)

;; right now this si just functions we use across demos.
;; later this will be formalized when I draw up a decent 
;; design for the api/whatever-this-is

;;;--------------------------------------------------------------

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

;;;--------------------------------------------------------------

(defun make-gl-array-from-array (data-type data)
  (let* ((data-length (length data))
	 (arr (gl:alloc-gl-array data-type data-length)))
    (dotimes (i data-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defun sub-buffer (buffer-type buffer new-gl-array 
		   &optional (offset 0) 
		     (size (gl:gl-array-byte-size new-gl-array)))
  (with-bind-buffer buffer-type buffer
    (gl:buffer-sub-data buffer-type new-gl-array
			:offset offset :size size)))


(defun draw-elements-base-vertex (mode array indices base-vertex
			    &key (count (gl::gl-array-size array)))
  (%gl:draw-elements-base-vertex mode count
                     (gl::cffi-type-to-gl (gl::gl-array-type array))
                     (gl::gl-array-pointer-offset array indices)
		     base-vertex))


(defun calculate-frustrum-scale (field-of-view-degrees)
  (/ 1.0 (tan (/ (* field-of-view-degrees base:+one-degree-in-radians+) 2.0))))

