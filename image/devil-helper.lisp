(in-package :devil-helper)


(defun load-image-to-c-array (filename)
  (cl-devil:with-init
    (cl-devil:with-images ((a filename))
      (cl-devil:check-error)
      (let* ((height (cl-devil:image-height))
             (width (cl-devil:image-width))
             (data-cpy (cffi:foreign-alloc :ubyte-vec4 :count (* height width))))
        (cl-devil:copy-pixels 0 0 0 (cl-devil:image-width) (cl-devil:image-height) 1
                              :rgba :unsigned-byte data-cpy)
        (jungl:make-c-array-from-pointer (list width height)
                                       :ubyte-vec4 data-cpy)))))

(defun load-image-to-texture (filename)
  (let* ((array (load-image-to-c-array filename))
	 (texture (jungl:make-texture array)))
    (jungl:free-c-array array)
    texture))
