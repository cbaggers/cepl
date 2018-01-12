(in-package :cepl.render-buffers)

;;----------------------------------------------------------------------

(defun+ make-render-buffer (element-type dimensions &key multisample)
  (assert (and (listp dimensions)
               (= (length dimensions) 2)
               (typep (first dimensions) '(unsigned-byte 16))
               (typep (second dimensions) '(unsigned-byte 16)))
          () "Dimensions passed to #'make-render-buffer must be a list of 2 positive integers")
  (assert element-type)
  (let ((dimensions (listify dimensions))
        (image-format (cepl.textures::calc-image-format element-type nil)))
    (cepl.context::if-gl-context
     (make-render-buffer-now %pre% dimensions image-format multisample)
     (make-uninitialized-render-buffer multisample))))

(defun+ make-render-buffer-now (render-buffer
                                dimensions
                                image-format
                                multisample)
  (let ((id (gl:gen-renderbuffer))
        (res (make-array (length dimensions) :element-type 'single-float
                         :initial-contents dimensions)))
    (setf (%render-buffer-id render-buffer) id
          (%render-buffer-resolution render-buffer) res
          (%render-buffer-multisample-p render-buffer) multisample
          (%render-buffer-image-format render-buffer) image-format)
    render-buffer))

;;----------------------------------------------------------------------

(defmethod print-object ((obj render-buffer) stream)
  (format stream "#<RENDER-BUFFER (~a) ~s ~a>"
          (%render-buffer-id obj)
          (%render-buffer-image-format obj)
          (render-buffer-dimensions obj)))

;;----------------------------------------------------------------------

(defn render-buffer-resolution ((render-buffer render-buffer)) vec2
  (%render-buffer-resolution render-buffer))

(defn render-buffer-multisample-p ((render-buffer render-buffer)) boolean
  (%render-buffer-multisample-p render-buffer))

(defn render-buffer-image-format ((render-buffer render-buffer)) symbol
  (%render-buffer-image-format render-buffer))

(defn render-buffer-dimensions ((render-buffer render-buffer)) list
  (let ((res (%render-buffer-resolution render-buffer)))
    (list (floor (aref res 0)) (floor (aref res 1)))))

(defn render-buffer-element-type ((render-buffer render-buffer)) symbol
  (%render-buffer-image-format render-buffer))

;;----------------------------------------------------------------------
