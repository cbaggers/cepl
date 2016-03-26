(in-package :cepl.samplers)

(defun lod-bias (sampler) (%sampler-lod-bias sampler))
(defun (setf lod-bias) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-lod-bias sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-lod-bias value))
    ((typep sampler 'texture)
     (cepl.textures:with-texture-bound sampler
       (%gl:tex-parameter-f (texture-type sampler) :texture-lod-bias value)))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun min-lod (sampler) (%sampler-min-lod sampler))
(defun (setf min-lod) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-min-lod sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-min-lod value))
    ((typep sampler 'texture)
     (cepl.textures:with-texture-bound sampler
       (%gl:tex-parameter-f (texture-type sampler) :texture-min-lod value)))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun max-lod (sampler) (%sampler-max-lod sampler))
(defun (setf max-lod) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-max-lod sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-max-lod value))
    ((typep sampler 'texture)
     (cepl.textures:with-texture-bound sampler
       (%gl:tex-parameter-f (texture-type sampler) :texture-max-lod value)))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun magnify-filter (sampler) (%sampler-magnify-filter sampler))
(defun (setf magnify-filter) (value sampler)
  (assert (member value '(:linear :nearest)))
  (cond
    ((sampler-p sampler)
     (setf (%sampler-magnify-filter sampler) value)
     (%gl::sampler-parameter-i (%sampler-id sampler) :texture-mag-filter
                               (%gl::foreign-enum-value '%gl:enum value)))
    ((typep sampler 'texture)
     (cepl.textures:with-texture-bound sampler
       (%gl::tex-parameter-i (texture-type sampler) :texture-mag-filter
                             (%gl::foreign-enum-value '%gl:enum value))))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun minify-filter (sampler) (%sampler-minify-filter sampler))
(defun (setf minify-filter) (value sampler)
  (cond
    ((sampler-p sampler)
     (when (member value '(:linear-mipmap-linear :nearest-mipmap-linear
                           :linear-mipmap-nearest :nearest-mipmap-nearest))
       (setf (%sampler-expects-mipmap sampler) t) )
     (setf (%sampler-minify-filter sampler) value)
     (%gl::sampler-parameter-i (%sampler-id sampler) :texture-min-filter
                               (%gl::foreign-enum-value '%gl:enum value)))
    ((typep sampler 'texture)
     (cepl.textures:with-texture-bound sampler
       (%gl::tex-parameter-i (texture-type sampler) :texture-min-filter
                             (%gl::foreign-enum-value '%gl:enum value))))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

;; remembering the gl names for the interpolation is a bit annoying so
;; this function does it for you, alas because it takes two arguments it
;; doesnt work well as a setf func.
(defun set-minify-filter (sampler for-level &key (between-levels nil))
  (setf (minify-filter sampler) (calc-minify-filter for-level between-levels)))

(defun calc-minify-filter (for-level between-levels)
  (assert (and (member for-level '(:linear :nearest))
               (member between-levels '(:linear :nearest))))
  (if between-levels
      (if (eq between-levels :linear)
          (if (eq for-level :linear)
              :linear-mipmap-linear
              :nearest-mipmap-linear)
          (if (eq for-level :linear)
              :linear-mipmap-nearest
              :nearest-mipmap-nearest))
      for-level))

(defun wrap (sampler) (%sampler-wrap sampler))
(defun (setf wrap) (value sampler)
  (let ((options '(:repeat :mirrored-repeat :clamp-to-edge :clamp-to-border
                   :mirror-clamp-to-edge))
	(value (if (keywordp value)
		   (vector value value value)
		   value)))
    (assert (and (vectorp value)
		 (= (length value) 3)
		 (every (lambda (x) (member x options)) value)))
    (cond
      ((sampler-p sampler)
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-s
                                 (%gl::foreign-enum-value '%gl:enum (aref value 0)))
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-t
                                 (%gl::foreign-enum-value '%gl:enum (aref value 1)))
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-r
                                 (%gl::foreign-enum-value '%gl:enum (aref value 2)))
       (setf (%sampler-wrap sampler) value))
      ((typep sampler 'texture)
       (cepl.textures:with-texture-bound sampler
         (%gl::tex-parameter-i (texture-type sampler) :texture-wrap-s
                               (%gl::foreign-enum-value '%gl:enum (aref value 0)))
         (%gl::tex-parameter-i (texture-type sampler) :texture-wrap-t
                               (%gl::foreign-enum-value '%gl:enum (aref value 1)))
         (%gl::tex-parameter-i (texture-type sampler) :texture-wrap-r
                               (%gl::foreign-enum-value '%gl:enum (aref value 2)))))
      (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler))))
  sampler)

(defun compare (sampler) (%sampler-compare sampler))
(defun (setf compare) (value sampler)
  (cond ((sampler-p sampler)
         (setf (%sampler-compare sampler)
               (or value :none))
         (if (and value (not (eq :none value)))
             (progn
               (%gl:sampler-parameter-i
                (%sampler-id sampler) :texture-compare-mode
                (%gl::foreign-enum-value '%gl:enum :compare-ref-to-texture))
               (%gl:sampler-parameter-i
                (%sampler-id sampler) :texture-compare-func
                (%gl::foreign-enum-value
                 '%gl:enum
                 (case value
                   ((:never nil) :never)
                   ((:always t) :always)
                   ((:equal := =) :equal)
                   ((:not-equal :/= /=) :not-equal)
                   ((:less :< <) :less)
                   ((:greater :> >) :greater)
                   ((:lequal :<= <=) :lequal)
                   ((:gequal :>= >=) :gequal)
                   (otherwise (error "Invalid compare func for sampler ~a" value))))))
             (%gl:sampler-parameter-i
              (%sampler-id sampler) :texture-compare-mode
              (%gl::foreign-enum-value '%gl:enum :none))))
        ((typep sampler 'texture)
         (cepl.textures:with-texture-bound sampler
           (if value
               (progn
                 (%gl:tex-parameter-i
                  (texture-type sampler) :texture-compare-mode
                  (%gl::foreign-enum-value
                   '%gl:enum :compare-ref-to-texture))
                 (%gl:tex-parameter-i
                  (texture-type sampler) :texture-compare-func
                  (%gl::foreign-enum-value
                   '%gl:enum
                   (case value
                     ((:never nil) :never)
                     ((:always t) :always)
                     ((:equal := =) :equal)
                     ((:not-equal :/= /=) :not-equal)
                     ((:less :< <) :less)
                     ((:greater :> >) :greater)
                     ((:lequal :<= <=) :lequal)
                     ((:gequal :>= >=) :gequal)
                     (otherwise (error "Invalid compare func for sampler ~a" value))))))
               (%gl:tex-parameter-i
                (texture-type sampler) :texture-compare-mode
                (%gl::foreign-enum-value '%gl:enum :none)))))
        (t (error "Invalid type ~a of ~a for lod-bias"
                  (type-of sampler) sampler)))
  sampler)

;; This is how you use samplers.
(defmacro with-sampling (bindings-pairs &body body)
  (let* ((tex-syms (cepl-utils:n-of* (gensym "texture")
				     (length bindings-pairs)))
         (revert-syms (cepl-utils:n-of* (gensym "original-id")
					(length bindings-pairs)))
         (letting (loop for b in bindings-pairs
                     for ts in tex-syms
                     for rs in revert-syms
                     do (assert (symbolp (first b)))
                     append `((,ts ,(first b))
                              (,rs (texture-sampler-object-id ,ts)))))
         (setting (loop for b in bindings-pairs
                     for ts in tex-syms
                     collect `(setf (texture-sampler-object-id ,ts)
                                    (%sampler-id ,(second b)))))
         (reverting (loop for ts in tex-syms
                       for rs in revert-syms
                       collect `(setf (texture-sampler-object-id ,ts) ,rs))))
    `(let* ,letting
       ,@setting
       ,@body
       ,@reverting)))

(defvar *sampler-types*
  '(:isampler-1d :isampler-1d-array :isampler-2d :isampler-2d-array
    :isampler-2d-ms :isampler-2d-ms-array :isampler-2d-rect
    :isampler-3d :isampler-buffer :isampler-cube
    :isampler-cube-array :sampler-1d :sampler-1d-array
    :sampler-1d-array-shadow :sampler-1d-shadow :sampler-2d
    :sampler-2d-array :sampler-2d-array-shadow :sampler-2d-ms
    :sampler-2d-ms-array :sampler-2d-rect :sampler-2d-rect-shadow
    :sampler-2d-shadow :sampler-3d :sampler-buffer :sampler-cube
    :sampler-cube-array :sampler-cube-array-shadow
    :sampler-cube-shadow :usampler-1d :usampler-1d-array
    :usampler-2d :usampler-2d-array :usampler-2d-ms
    :usampler-2d-ms-array :usampler-2d-rect :usampler-3d
    :usampler-buffer :usampler-cube :usampler-cube-array
    :isampler-1d-arb :isampler-1d-array-arb :isampler-2d-arb
    :isampler-2d-array-arb
    :isampler-2d-ms-arb :isampler-2d-ms-array-arb :isampler-2d-rect-arb
    :isampler-3d-arb :isampler-buffer-arb :isampler-cube-arb
    :isampler-cube-array-arb :sampler-1d-arb :sampler-1d-array-arb
    :sampler-1d-array-shadow-arb :sampler-1d-shadow-arb :sampler-2d-arb
    :sampler-2d-array-arb :sampler-2d-array-shadow-arb :sampler-2d-ms-arb
    :sampler-2d-ms-array-arb :sampler-2d-rect-arb :sampler-2d-rect-shadow-arb
    :sampler-2d-shadow-arb :sampler-3d-arb :sampler-buffer-arb :sampler-cube-arb
    :sampler-cube-array-arb :sampler-cube-array-shadow-arb
    :sampler-cube-shadow-arb :usampler-1d-arb :usampler-1d-array-arb
    :usampler-2d-arb :usampler-2d-array-arb :usampler-2d-ms-arb
    :usampler-2d-ms-array-arb :usampler-2d-rect-arb :usampler-3d-arb
    :usampler-buffer-arb :usampler-cube-arb :usampler-cube-array-arb))

(defun sampler-typep (type)
  (or (member type *sampler-types*)
      (varjo:v-typep type 'v-sampler)))
