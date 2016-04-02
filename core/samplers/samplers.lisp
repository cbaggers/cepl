(in-package :cepl.samplers)

;; Sampler types

;; There are a number of sampler types. The various sampler types are separated
;;  into 3 categories, based on the basic data type of the Image Format of the
;; texture that they sample from. These are floating-point, signed integer, and
;; unsigned integer. Floating-point also covers normalized integer formats.

;; The name of the sampler type in GLSL reflects this grouping. The names are
;; very similar to the names of the vector types in GLSL. Floating-point vectors
;; do not have a prefix; they are just "vec". Signed integer vectors are "ivec",
;; and unsigned integer vectors are "uvec".

;; So for samplers, floating-point samplers begin with "sampler". Signed integer
;; samplers begin with "isampler", and unsigned integer samplers begin with
;; "usampler". If you attempt to read from a sampler where the texture's Image
;; Format doesn't match the sampler's basic format (usampler2D with a GL_R8I,
;; or sampler1D with GL_R8UI, for example), all reads will produce undefined
;; values.

;; Depth-component textures are treated as one-component floating-point
;; textures. Stencil-component textures are treated as one-component unsigned
;; integer textures.

;; For the sake of clarity, when you see a g preceding "sampler" in a sampler
;; name, it represents any of the 3 possible prefixes (nothing for float, i for
;; signed integer, and u for unsigned integer).

;; The rest of the sampler's name refers to the texture type that it is a
;; sampler of. The names map as follows:
;; GLSL sampler       OpenGL texture enum               Texture type
;; gsampler1D         GL_TEXTURE_1D                     1D texture
;; gsampler2D         GL_TEXTURE_2D                     2D texture
;; gsampler3D         GL_TEXTURE_3D                     3D texture
;; gsamplerCube       GL_TEXTURE_CUBE_MAP               Cubemap Texture
;; gsampler2DRect     GL_TEXTURE_RECTANGLE              Rectangle Texture
;; gsampler1DArray    GL_TEXTURE_1D_ARRAY               1D Array Texture
;; gsampler2DArray    GL_TEXTURE_2D_ARRAY               2D Array Texture
;; gsamplerCubeArray  GL_TEXTURE_CUBE_MAP_ARRAY         Cubemap Array Texture (requires GL 4.0 or ARB_texture_cube_map_array)
;; gsamplerBuffer     GL_TEXTURE_BUFFER                 Buffer Texture
;; gsampler2DMS       GL_TEXTURE_2D_MULTISAMPLE         Multisample Texture
;; gsampler2DMSArray  GL_TEXTURE_2D_MULTISAMPLE_ARRAY   Multisample Array Texture

;; Shadow samplers
;; If a texture has a depth or depth-stencil image format and has the depth
;; comparison activated, it cannot be used with a normal sampler. Attempting to
;; do so results in undefined behavior. Such textures must be used with a
;; shadow sampler. This type changes the texture lookup functions (see below),
;; adding an additional component to the textures' usual texture coordinate
;; vector. This extra value is used to compare against the value sampled from
;; the texture.

;; Because cubemap arrays normally take 4D texture coordinates, the texture
;; lookup function overloads that take a cubemap array take an additional
;; parameter, instead of expanding the vector texture coordinate size.

;; The result of accessing a shadow texture is always a single float value.
;; This value is on the range [0, 1], which is proportional to the number of
;; samples in the shadow texture that pass the comparison. Therefore, if the
;; resulting value is 0.25, then only 1 out of the 4 values sampled by the
;; comparison operation passed.

;; Notice that none of these types have the g prefix. This is because shadow
;; samplers can only be used with textures with depth components. And those are
;; all floating-point (actual floats or unsigned Normalized Integers) image
;; formats. Furthermore, the result of the comparison is always a single float
;; value, since depth formats only provide one component of data.
;;
;; GLSL sampler            OpenGL texture enum
;; sampler1DShadow         GL_TEXTURE_1D
;; sampler2DShadow         GL_TEXTURE_2D
;; samplerCubeShadow       GL_TEXTURE_CUBE_MAP
;; sampler2DRectShadow     GL_TEXTURE_RECTANGLE
;; sampler1DArrayShadow    GL_TEXTURE_1D_ARRAY
;; sampler2DArrayShadow    GL_TEXTURE_2D_ARRAY
;; samplerCubeArrayShadow  GL_TEXTURE_CUBE_MAP_ARRAY


;; [TODO] Add shadow samplers
;; [TODO] does cl-opengl use multisample instead of ms?
(defun calc-sampler-type (texture-type image-format &optional shadow-sampler)
  "Makes the keyword that names the sampler-type for the given texture-type and format"
  (cepl-utils:kwd
   (case image-format
     ((:r8 :r8-snorm :r16 :r16-snorm :rg8 :rg8-snorm :rg16 :rg16-snorm
           :r3-g3-b2 :rgb4 :rgb5 :rgb8 :rgb8-snorm :rgb10 :rgb12
           :rgb16-snorm :rgba2 :rgba4
           :rgb5-a1 :rgba8 :rgba8-snorm :rgb10-a2 :rgba12 :rgba16 :srgb8
           :srgb8-alpha8 :r16f :rg16f :rgb16f :rgba16f :r32f :rg32f :rgb32f
           :rgba32f :r11f-g11f-b10f :rgb9-e5
           :depth-component16 :depth-component24 :depth-component32
           :depth-component32f) "")
     ((:r8i :r16i :r32i :rg8i :rg16i :rg32i :rgb8i :rgb16i :rgb32i :rgba8i
            :rgba32i :rgba16i) :i)
     ((:rg8ui :rg16ui :rg32ui :rgb8ui :rgb16ui :rgb32ui :rgba8ui :rgba16ui
              :rgba32ui :rgb10-a2ui :r8ui :r16ui :r32ui
              :depth24-stencil8 :depth32f-stencil8) :u)
     (t (error "CEPL: Unable to calculate the sampler type for ~s. Mapping missing."
	       image-format)))
   (if shadow-sampler
       (case texture-type
         (:texture-1d :sampler-1d-shadow)
         (:texture-2d :sampler-2d-shadow)
         (:texture-cube-map :sampler-cube-shadow)
         (:texture-rectangle :sampler-2drect-shadow)
         (:texture-1d-array :sampler-1d-array-shadow)
         (:texture-2d-array :sampler-2d-array-shadow)
         (:texture-cube-map-array :sampler-cube-array-shadow)
         (t (error "CEPL: Unable to calculate the sampler type for ~s with texture type ~s. Mapping missing"
		   image-format texture-type)))
       (case texture-type
         (:texture-1d :sampler-1d) (:texture-2d :sampler-2d)
         (:texture-3d :sampler-3d) (:texture-cube-map :sampler-cube)
         (:texture-rectangle :sampler-2drect)
         (:texture-1d-array :sampler-1d-array)
         (:texture-2d-array :sampler-2d-array)
         (:texture-cube-map-array :sampler-cube-array)
         (:texture-buffer :sampler-buffer)
         (:texture-2d-multisample :sampler-2d-ms)
         (:texture-2d-multisample-array :sampler-2d-ms-array)
         (t (error "CEPL: Unable to calculate the sampler type for ~s with texture type ~s. Mapping missing"
		   image-format texture-type))))))

(defun %delete-sampler (sampler)
  (gl::delete-sampler (%sampler-id sampler)))

(defun %delete-samplers (&rest samplers)
  (let ((ids (mapcar #'%sampler-id samplers)))
    (gl::with-opengl-sequence (array '%gl:uint ids)
      (%gl:delete-samplers (length ids) array))))


(defun sample (texture &key (lod-bias 0.0) (min-lod -1000.0) (max-lod 1000.0)
			 (minify-filter :linear) (magnify-filter :linear)
			 (wrap #(:repeat :repeat :repeat)) (compare :none))
  (cepl.memory::if-context
   (make-sampler-now %pre% lod-bias min-lod max-lod minify-filter
		     magnify-filter wrap compare)
   (make-uninitialized-sampler texture)
   (list texture)))

(let ((cached nil)
      (has nil))
  (defun has-sampler-support-p ()
    (if cached
	has
	(setf cached t
	      has (has-feature "GL_ARB_sampler_objects")))))

(defvar *fake-sampler-id* 0)
(defun get-sampler-id-box (lod-bias min-lod max-lod minify-filter
			   magnify-filter wrap compare)
  ;; this will have the lovely logic for deduping sampler-ids
  (declare (ignore lod-bias min-lod max-lod minify-filter
		   magnify-filter wrap compare))
  (make-sampler-id-box :id (if (has-sampler-support-p)
			       (first (gl::gen-samplers 1))
			       (incf *fake-sampler-id*))))

(defun make-sampler-now (sampler-obj lod-bias min-lod max-lod minify-filter
			 magnify-filter wrap compare)
  (let* ((texture (%sampler-texture sampler-obj))
	 (sampler-type (cepl.samplers::calc-sampler-type
			(texture-type texture)
			(texture-image-format texture))))
    (setf (%sampler-id sampler-obj) (get-sampler-id-box
				     lod-bias min-lod max-lod minify-filter
				     magnify-filter wrap compare)
	  (%sampler-type sampler-obj) sampler-type
	  (lod-bias sampler-obj) lod-bias
	  (min-lod sampler-obj) min-lod
	  (max-lod sampler-obj) max-lod
	  (minify-filter sampler-obj) minify-filter
	  (magnify-filter sampler-obj) magnify-filter
	  (wrap sampler-obj) (if (keywordp wrap)
				 (vector wrap wrap wrap)
				 wrap)
	  (compare sampler-obj) compare))
  sampler-obj)

(defmethod print-object ((object sampler) stream)
  (if (initialized-p object)
      ;;(call-next-method object stream)
      (format stream "#<~s ~s>"
	      (%sampler-type object)
	      (%sampler-texture object))
      (format stream "#<SAMPLER :UNINITIALIZED>")))

;;----------------------------------------------------------------------

(defun lod-bias (sampler) (%sampler-lod-bias sampler))
(defun (setf lod-bias) (value sampler)
  (setf (%sampler-lod-bias sampler) value)
  (%gl:sampler-parameter-f (%sampler-id sampler) :texture-lod-bias value)
  sampler)

(defun min-lod (sampler) (%sampler-min-lod sampler))
(defun (setf min-lod) (value sampler)
  (setf (%sampler-min-lod sampler) value)
  (%gl:sampler-parameter-f (%sampler-id sampler) :texture-min-lod value)
  sampler)

(defun max-lod (sampler) (%sampler-max-lod sampler))
(defun (setf max-lod) (value sampler)
  (setf (%sampler-max-lod sampler) value)
  (%gl:sampler-parameter-f (%sampler-id sampler) :texture-max-lod value)
  sampler)

(defun magnify-filter (sampler) (%sampler-magnify-filter sampler))
(defun (setf magnify-filter) (value sampler)
  (assert (member value '(:linear :nearest)))
  (setf (%sampler-magnify-filter sampler) value)
  (%gl::sampler-parameter-i (%sampler-id sampler) :texture-mag-filter
			    (%gl::foreign-enum-value '%gl:enum value))
  sampler)

(defun minify-filter (sampler) (%sampler-minify-filter sampler))
(defun (setf minify-filter) (value sampler)
  (when (member value '(:linear-mipmap-linear :nearest-mipmap-linear
			:linear-mipmap-nearest :nearest-mipmap-nearest))
    (setf (%sampler-expects-mipmap sampler) t))
  (setf (%sampler-minify-filter sampler) value)
  (%gl::sampler-parameter-i (%sampler-id sampler) :texture-min-filter
			    (%gl::foreign-enum-value '%gl:enum value))
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
    (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-s
			      (%gl::foreign-enum-value '%gl:enum (aref value 0)))
    (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-t
			      (%gl::foreign-enum-value '%gl:enum (aref value 1)))
    (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-r
			      (%gl::foreign-enum-value '%gl:enum (aref value 2)))
    (setf (%sampler-wrap sampler) value))
  sampler)

(defun compare (sampler) (%sampler-compare sampler))
(defun (setf compare) (value sampler)
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
       (%gl::foreign-enum-value '%gl:enum :none)))
  sampler)

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
