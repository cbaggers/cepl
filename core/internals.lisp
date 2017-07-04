(in-package :cepl.internals)

(defgeneric populate (object data))

(defun+ 1d-p (object)
  (= 1 (length (dimensions object))))

(defgeneric gl-assign-attrib-pointers (array-type &optional attrib-num
                                                    pointer-offset
                                                    stride-override
                                                    normalized))

(defmethod gl-assign-attrib-pointers ((array-type t) &optional (attrib-num 0)
                                                       (pointer-offset 0)
                                                       stride-override
                                                       normalized)
  (let ((type (varjo:type-spec->type array-type)))
    (if (and (varjo:core-typep type) (not (varjo:v-typep type 'v-sampler)))
        (let ((slot-layout (cepl.types::expand-slot-to-layout
                            nil type normalized))
              (stride 0))
          (loop :for attr :in slot-layout
             :for i :from 0
             :with offset = 0
             :do (progn
                   (%gl:enable-vertex-attrib-array (+ attrib-num i))
                   (%gl:vertex-attrib-pointer
                    (+ attrib-num i) (first attr) (second attr)
                    (third attr) (or stride-override stride)
                    (cffi:make-pointer (+ offset pointer-offset))))
             :do (setf offset (+ offset (* (first attr)
                                           (gl-type-size (second attr))))))
          (length slot-layout))
        (error "Type ~a is not known to cepl" type))))

(defgeneric clear-gl-context-cache (object))
(defgeneric s-arrayp (object))
(defgeneric s-prim-p (spec))
(defgeneric s-extra-prim-p (spec))
(defgeneric s-def (spec))

(defgeneric symbol-names-cepl-structp (sym))
(defmethod symbol-names-cepl-structp ((sym t))
  nil)


(defun+ color-attachment-enum (attachment-num)
  (+ attachment-num #.(cffi:foreign-enum-value '%gl:enum :color-attachment0)))

(defun+ draw-buffer-enum (buffer-num)
  (+ buffer-num #.(cffi:foreign-enum-value '%gl:enum :draw-buffer0)))

(defun+ surface-dimensions (surface)
  (cepl.host:window-size surface))

(defun+ surface-resolution (surface)
  (dbind (x y) (window-dimensions surface)
    (v! x y)))

(defun+ window-dimensions (window)
  (warn "CEPL: window-dimensions is deprecated, please use surface-dimensions instead")
  (surface-dimensions window))

(defun+ window-resolution (window)
  (warn "CEPL: window-resolution is deprecated, please use surface-resolution instead")
  (surface-resolution window))

(defun+ gl-type-size (type)
  (if (keywordp type)
      (cffi:foreign-type-size type)
      (autowrap:foreign-type-size type)))

(defun+ cffi-type->gl-type (type)
  (case type
    ((:char :signed-char) :byte)
    ((:uchar :unsigned-char) :unsigned-byte)
    ((:short :signed-short) :short)
    ((:ushort :unsigned-short) :unsigned-short)
    ((:int :signed-int) :int)
    ((:uint :unsigned-int) :unsigned-int)
    (:int8 :signed-byte)
    (:uint8 :unsigned-byte)
    (:float :float)
    (:double :double)
    (otherwise type)))

(deftype uploadable-lisp-seq () '(or list vector array))
