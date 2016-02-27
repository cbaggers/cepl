(in-package :jungl)

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

(defun gl-type-size (type)
  (if (keywordp type)
      (cffi:foreign-type-size type)
      (autowrap:foreign-type-size type)))
