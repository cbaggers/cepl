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

(defmethod gl-assign-attrib-pointers ((array-type t) &optional (attrib-num 0)
                                                       (pointer-offset 0)
                                                       stride-override
                                                       normalised)
  (let ((type (varjo:type-spec->type array-type)))
    (if (and (varjo:core-typep type) (not (varjo:v-typep type 'v-sampler)))
        (let ((slot-layout (expand-slot-to-layout nil type normalised))
              (stride 0))
          (loop :for attr :in slot-layout
             :for i :from 0
             :with offset = 0
             :do (progn
                   (gl:enable-vertex-attrib-array (+ attrib-num i))
                   (%gl:vertex-attrib-pointer
                    (+ attrib-num i) (first attr) (second attr)
                    (third attr) (or stride-override stride)
                    (cffi:make-pointer (+ offset pointer-offset))))
             :do (setf offset (+ offset (* (first attr)
                                           (gl-type-size (second attr))))))
          (length slot-layout))
        (error "Type ~a is not known to cepl" type))))

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


(defun gl-type-size (type)
  (if (keywordp type)
      (cffi:foreign-type-size type)
      (autowrap:foreign-type-size type)))
