(in-package :jungl)

(defun get-uniform-block-index (program name)
  (with-foreign-string (s name)
    (%gl:get-uniform-block-index program s)))

(defun gen-buffer ()
  (car (gl:gen-buffers 1)))

(defun tex-storage-1d (target levels internal-format width)
  (%gl:tex-storage-1d target levels (gl::internal-format->int internal-format)
                      width))

(defun tex-storage-2d (target levels internal-format width height)
  (%gl:tex-storage-2d target levels (gl::internal-format->int internal-format)
                      width height))

(defun tex-storage-3d (target levels internal-format width height depth)
  (%gl:tex-storage-3d target levels (gl::internal-format->int internal-format)
                      width height depth))

(defun active-texture-num (num)
  (gl:active-texture (+ #x84C0 num)))

(defun color-attachment-enum (attachment-num)
  (+ attachment-num #.(cffi:foreign-enum-value '%gl:enum :color-attachment0)))

(let ((vals #(#.(cffi:foreign-enum-value '%gl:enum :back-left)
              #.(cffi:foreign-enum-value '%gl:enum :front-left)
              #.(cffi:foreign-enum-value '%gl:enum :back-right)
              #.(cffi:foreign-enum-value '%gl:enum :front-right))))
  (defun default-fbo-attachment-enum (attachment-num)
    (aref vals attachment-num)))

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
