(in-package :cepl.internals)

(defgeneric populate (object data))

(defmethod dimensions ((object t))
  (error "Jungl: Cannot extract dimensions from ~s object:~%~s"
	 (type-of object) object))

(defun 1d-p (object)
  (= 1 (length (dimensions object))))

(defgeneric gl-assign-attrib-pointers (array-type &optional attrib-num
                                              pointer-offset
                                              stride-override
                                              normalised))

(defgeneric clear-gl-context-cache (object))
(defgeneric s-arrayp (object))
(defgeneric s-prim-p (spec))
(defgeneric s-extra-prim-p (spec))
(defgeneric s-def (spec))
(defgeneric make-vao-from-id (gl-object gpu-arrays &optional index-array))
(defgeneric %collate-args (spec))
(defgeneric %get-pipeline-uniforms (pipeline-spec call-form))


(defgeneric symbol-names-cepl-structp (sym))
(defmethod symbol-names-cepl-structp ((sym t))
  nil)
