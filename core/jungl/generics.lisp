(in-package :jungl)

(defgeneric free (object))
(defgeneric free-gpu-array (gpu-array))
(defgeneric push-g (object destination))
(defgeneric pull-g (object))
(defgeneric pull1-g (object))
(defgeneric dimensions (object))
(defgeneric backed-by (object))
(defgeneric lisp-type->pixel-format (type))

(defmethod pull-g ((object t)) object)
(defmethod pull1-g ((object t)) object)

(defun 1d-p (object)
  (= 1 (length (dimensions object))))

(defgeneric populate (object data))

(defmethod dimensions ((object t))
  (error "Jungl: Cannot extract dimensions from ~s object:~%~s"
	 (type-of object) object))

(defgeneric gl-assign-attrib-pointers (array-type &optional attrib-num
                                              pointer-offset
                                              stride-override
                                              normalised))



(defgeneric make-gpu-array (initial-contents &key)
  (:documentation "This function creates a gpu-array which is very similar
   to a c-array except that the data is located in the memory
   of the graphics card and so is accessible to shaders.
   You can either provide and type and length or you can
   provide a c-array and the data from that will be used to
   populate the gpu-array with.

   Access style is optional but if you are comfortable with
   opengl, and know what type of usage pattern thsi array will
   have, you can set this to any of the following:
   (:stream-draw :stream-read :stream-copy :static-draw
    :static-read :static-copy :dynamic-draw :dynamic-read
    :dynamic-copy)"))


(defgeneric clear-gl-context-cache (object))
(defgeneric s-arrayp (object))
(defgeneric s-prim-p (spec))
(defgeneric s-extra-prim-p (spec))
(defgeneric s-def (spec))

(defgeneric tangent (object))
(defgeneric bi-tangent (object))
(defgeneric (setf tangent) (val object))
(defgeneric (setf bi-tangent) (val object))

(defgeneric make-vao-from-id (gl-object gpu-arrays &optional index-array))
(defgeneric %collate-args (spec))
(defgeneric %get-pipeline-uniforms (pipeline-spec call-form))
(defgeneric free-texture (texture))
