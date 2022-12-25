(in-package :cepl.internals)

(defgeneric populate (object data))

(defun+ 1d-p (object)
  (= 1 (length (dimensions object))))

(defgeneric gl-assign-attrib-pointers (array-type &optional attrib-num
                                                    pointer-offset
                                                    stride-override
                                                    normalized
                                                    instance-divisor))

(defmethod gl-assign-attrib-pointers ((array-type t) &optional (attrib-num 0)
                                                       (pointer-offset 0)
                                                       stride-override
                                                       normalized
                                                       instance-divisor)
  (let ((type (or (varjo.internals::try-type-spec->type array-type nil)
                  (error 'bad-type-for-buffer-stream-data :type array-type))))
    (if (and (varjo:core-typep type) (not (varjo:v-typep type 'v-sampler)))
        (let* ((slot-layout (cepl.types::expand-slot-to-layout
                             nil type normalized))
               (stride (reduce (lambda (accum attr)
                                 (incf accum (* (first attr)
                                                (gl-type-size (second attr)))))
                               slot-layout
                               :initial-value 0)))
          (loop :for attr :in slot-layout
             :for i :from 0
             :with offset = 0
             :do (progn
                   (%gl:enable-vertex-attrib-array (+ attrib-num i))
                   (when instance-divisor
                     (%gl:vertex-attrib-divisor
                      (+ attrib-num i) instance-divisor))
                   (%gl:vertex-attrib-pointer
                    (+ attrib-num i) (first attr) (second attr)
                    (third attr) (or stride-override stride)
                    (cffi:make-pointer (+ offset pointer-offset))))
             :do (setf offset (+ offset (* (first attr)
                                           (gl-type-size (second attr))))))
          (length slot-layout))
        (error "Type ~a is not known to cepl" type))))

(defgeneric clear-gl-context-cache (object))

(defgeneric symbol-names-cepl-structp (sym))
(defmethod symbol-names-cepl-structp ((sym t))
  nil)

(defun+ surface-dimensions (surface)
  (cepl.host:window-size surface))

(defun+ surface-resolution (surface)
  (dbind (x y) (window-dimensions surface)
    (vec2 (coerce x 'single-float)
          (coerce y 'single-float))))

(defun+ window-dimensions (window)
  (warn "CEPL: window-dimensions is deprecated, please use surface-dimensions instead")
  (surface-dimensions window))

(defun+ window-resolution (window)
  (warn "CEPL: window-resolution is deprecated, please use surface-resolution instead")
  (surface-resolution window))

(defun+ gl-type-size (type)
  (cffi:foreign-type-size type))

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
