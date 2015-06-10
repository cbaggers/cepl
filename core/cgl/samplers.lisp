(in-package :cgl)

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
(defun calc-sampler-type (texture-type internal-format &optional shadow-sampler)
  "Makes the keyword that names the sampler-type for the given texture-type and format"
  (utils:kwd
   (case internal-format
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
     (t (error "internal-format unknown")))
   (if shadow-sampler
       (case texture-type
         (:texture-1d :sampler-1d-shadow)
         (:texture-2d :sampler-2d-shadow)
         (:texture-cube-map :sampler-cube-shadow)
         (:texture-rectangle :sampler-2drect-shadow)
         (:texture-1d-array :sampler-1d-array-shadow)
         (:texture-2d-array :sampler-2d-array-shadow)
         (:texture-cube-map-array :sampler-cube-array-shadow)
         (t (error "shadow texture type not known")))
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
         (t (error "texture type not known"))))))

;; {TODO} border-color
(defstruct (sampler (:constructor %make-sampler)
                    (:conc-name %sampler-))
  (id -1 :type fixnum)
  (lod-bias 0.0 :type single-float)
  (min-lod -1000.0 :type single-float)
  (max-lod 1000.0 :type single-float)
  (expects-mipmap nil :type boolean)
  (minify-filter :linear :type keyword)
  (magnify-filter :linear :type keyword)
  (wrap #(:repeat :repeat :repeat) :type vector)
  (expects-depth nil :type boolean)
  (compare nil :type symbol))

(defun %delete-sampler (sampler)
  (gl::delete-sampler (%sampler-id sampler)))

(defun %delete-samplers (&rest samplers)
  (let ((ids (mapcar #'%sampler-id samplers)))
    (gl::with-opengl-sequence (array '%gl:uint ids)
      (%gl:delete-samplers (length ids) array))))


(defun make-sampler (&key (lod-bias 0.0) (min-lod -1000.0) (max-lod 1000.0)
                       (minify-filter :linear) (magnify-filter :linear)
                       (wrap #(:repeat :repeat :repeat)) (compare :none))
  (let ((self (%make-sampler :id (first (gl::gen-samplers 1)))))
    (setf (lod-bias self) lod-bias
          (min-lod self) min-lod
          (max-lod self) max-lod
          (minify-filter self) minify-filter
          (magnify-filter self) magnify-filter
          (wrap self) wrap
          (compare self) compare)
    self))

(defun lod-bias (sampler) (%sampler-lod-bias sampler))
(defun (setf lod-bias) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-lod-bias sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-lod-bias value))
    ((typep sampler 'gl-texture)
     (with-texture-bound (sampler)
       (%gl:tex-parameter-f (texture-type sampler) :texture-lod-bias value)))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun min-lod (sampler) (%sampler-min-lod sampler))
(defun (setf min-lod) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-min-lod sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-min-lod value))
    ((typep sampler 'gl-texture)
     (with-texture-bound (sampler)
       (%gl:tex-parameter-f (texture-type sampler) :texture-min-lod value)))
    (t (error "Invalid type ~a of ~a for lod-bias" (type-of sampler) sampler)))
  sampler)

(defun max-lod (sampler) (%sampler-max-lod sampler))
(defun (setf max-lod) (value sampler)
  (cond
    ((sampler-p sampler)
     (setf (%sampler-max-lod sampler) value)
     (%gl:sampler-parameter-f (%sampler-id sampler) :texture-max-lod value))
    ((typep sampler 'gl-texture)
     (with-texture-bound (sampler)
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
    ((typep sampler 'gl-texture)
     (with-texture-bound (sampler)
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
    ((typep sampler 'gl-texture)
     (with-texture-bound (sampler)
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
                   :mirror-clamp-to-edge)))
    (assert (and (vectorp value) (every (lambda (x) (member x options)) value)))
    (cond
      ((sampler-p sampler)
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-s
                                 (%gl::foreign-enum-value '%gl:enum (aref value 0)))
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-t
                                 (%gl::foreign-enum-value '%gl:enum (aref value 1)))
       (%gl::sampler-parameter-i (%sampler-id sampler) :texture-wrap-r
                                 (%gl::foreign-enum-value '%gl:enum (aref value 2)))
       (setf (%sampler-wrap sampler) value))
      ((typep sampler 'gl-texture)
       (with-texture-bound (sampler)
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
        ((typep sampler 'gl-texture)
         (with-texture-bound (sampler)
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
  (let* ((tex-syms (loop for i in bindings-pairs collect (gensym "texture")))
         (revert-syms (loop for i in bindings-pairs collect
                           (gensym "original-id")))
         (letting (loop for b in bindings-pairs
                     for ts in tex-syms
                     for rs in revert-syms
                     do (assert (symbolp (first b)))
                     append `((,ts ,(first b))
                              (,rs (slot-value ,ts 'sampler-object-id)))))
         (setting (loop for b in bindings-pairs
                     for ts in tex-syms
                     collect `(setf (slot-value ,ts 'sampler-object-id)
                                    (%sampler-id ,(second b)))))
         (reverting (loop for ts in tex-syms
                       for rs in revert-syms
                       collect `(setf (slot-value ,ts 'sampler-object-id) ,rs))))
    `(let* ,letting
       ,@setting
       ,@body
       ,@reverting)))

;; Sampling parameters

;; Filtering
;; ---------
;; Filtering is the process of accessing a particular sample from a
;; texture. There are two cases for filtering: minification and
;; magnification. Magnification means that the area of the fragment in texture
;; space is smaller than a texel, and minification means that the area of the
;; fragment in texture space is larger than a texel. Filtering for these two
;; cases can be set independently.

;; The magnification filter is controlled by the GL_TEXTURE_MAG_FILTER texture
;; parameter. This value can be GL_LINEAR or GL_NEAREST. If GL_NEAREST is used,
;; then the implementation will select the texel nearest the texture coordinate;
;; this is commonly called "point sampling"). If GL_LINEAR is used, the
;; implementation will perform a weighted linear blend between the nearest
;; adjacent samples.


;;         '(:texture-mag-filter (:linear :nearest))

;; The minification filter is controlled by the GL_TEXTURE_MIN_FILTER texture
;; parameter. To understand these values better, it is important to discuss what
;; the particular options are.

;;         '(:texture-min-filter (:nearest :linear :nearest-mipmap-nearest
;;                                :linear-mipmap-nearest :nearest-mipmap-linear
;;                                :linear-mipmap-linear))

;; When doing minification, you can choose to use mipmapping or not. Using
;; mipmapping means selecting between multiple mipmaps based on the angle and
;; size of the texture relative to the screen. Whether you use mipmapping or
;; not, you can still select between linear blending of the particular layer or
;; nearest. And if you do use mipmapping, you can choose to either select a
;; single mipmap to sample from, or you can sample the two adjacent mipmaps and
;; linearly blend the resulting values to get the final result.

;; The OpenGL minification settings for these are as follows:
;; Param Setting            Lin within mip-level   Has mipmapping   Linear between mip-levels
;; :nearest                 No                     No               -
;; :linear                  Yes                    No               -
;; :nearest-mipmap-nearest  No                     Yes              No
;; :linear-mipmap-nearest   Yes                    Yes              No
;; :nearest-mipmap-linear   No                     Yes              Yes
;; :linear-mipmap-linear    Yes                    Yes              Yes

;;         ((:between-mip-levels (:nearest :linear))
;;          (:between-within-mip-level (:nearest :linear)))

;;         ((:nearest :linear) (:between-within-mip-level (:nearest :linear)))

;; Anisotropic
;; -----------
;; filtering Note: This is not core functionality; it is governed by the
;; extension GL_EXT_texture_filter_anisotropic. However, this extension is
;; available virtually everywhere.

;; Anisotropic filtering is an advanced filtering technique that takes more than
;; one sample point and blends them together. Exactly how this is done is
;; implementation-dependent, but the control is a specific value: the maximum
;; number of samples that can be taken of the texture. More samples may slow
;; down performance, but increase image quality. Then again, it may not,
;; depending on the angle you're looking at the surface. Implementations only
;; take extra samples when needed.

;; To use anisotropic filtering, set the GL_TEXTURE_MAX_ANISOTROPY_EXT
;; parameter. This parameter is floating-point, and can be set between 1.0f and
;; an implementation-defined maximum anisotropy (queried with
;; GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT). Any value greater than 1.0f counts as a
;; use of anisotropic filtering.

;; Anisotropic filtering is not a replacement for mipmaps or mipmap
;; filtering. For best results, combine a anisotropic filtering with a
;; GL_LINEAR_MIPMAP_LINEAR minification filter.

;; LOD range
;; ---------
;; There is a pair of sampling parameters that affect the mipmap image
;; selection: GL_TEXTURE_MAX_LOD and GL_TEXTURE_MIN_LOD (floating-point
;; values). The way these work in mipmap selection is quite complicated; the
;; specification goes into full detail about it. These selection clamping
;; parameters will not cause sampling outside of the texture's mipmap range
;; specified by GL_TEXTURE_BASE_LEVEL and GL_TEXTURE_MAX_LEVEL.

;;         (:texture-base-level :texture-max-level)

;; LOD bias:
;; The mipmap image selection process can be adjusted coarsely by using the
;; GL_TEXTURE_LOD_BIAS sampling parameter. This bias will be added to the mipmap
;; LOD calculation (as well as added to the bias specified in one of the texture
;; accessing functions in GLSL), which is used to select the image. A positive
;; bias means that larger mipmaps will be selected even when the texture is
;; viewed from farther away. This can cause visual aliasing, but in small
;; quantities it can make textures a bit more sharp.

;; These selection clamping parameters will not cause sampling outside of the
;; texture's mipmap range specified by GL_TEXTURE_BASE_LEVEL and
;; GL_TEXTURE_MAX_LEVEL.

;;         :texture-lod-bias

;; Comparison mode:
;; Depth textures (textures that have a depth component image format) can be
;; sampled in one of two ways. They can be sampled as a normal texture, which
;; simply retrieves the depth value (with filtering applied). This will return a
;; vec4 containing a single floating-point value.

;; They can also be fetched in comparison mode. This means that sampling from
;; the texture requires a value to compare to those pulled from the texture;
;; this value is called the reference value. The result of the comparison
;; depends on the comparison function set in the texture. If the function
;; succeeds, the resulting value is 1.0f; if it fails, it is 0.0f. Swizzling can
;; be used, but only the R component of the swizzled result will be returned. So
;; it's not very useful.

;; When linear filtering is used, the actual returned value is
;; implementation-defined. However, the value will be on the range [0, 1] and
;; will be proportional to the number of neighboring texels that pass the
;; comparison based on the single given value.

;; If the texture is a normalized integer depth format, then the reference value
;; is clamped to [0, 1], to match the values from the texture. Otherwise, the
;; value is not clamped.

;; Using this mode requires two special settings. First, the sampler used in
;; GLSL must be a shadow sampler. Second, the texture used in that sampler must
;; have activated depth comparison mode. Attempting to use a texture without
;; comparison with a shadow sampler, or vice-versa, will result in an error upon
;; rendering.

;;         depth only, must use shadow sampler, texture must have comparison mode

;;         (set :texture-compare-mode :compare-ref-to-texture)
;;         (set :texture-compare-func
;;              (:never :always :less :lequal :equal :not-equal :gequal :greater
;;                      nil t #'< #'<= #'= #'/= #'>= #'> :< :<= := :/= :>= :>))

;; To set the texture to comparison mode, set the GL_TEXTURE_COMPARE_MODE
;; texture parameter to GL_COMPARE_REF_TO_TEXTURE. The comparison function to
;; use when comparing the reference to the texture is set with the
;; GL_TEXTURE_COMPARE_FUNC texture parameter. Acceptable values are GL_NEVER
;; (always fails), GL_ALWAYS (always succeeds), GL_LESS, GL_LEQUAL, GL_EQUAL,
;; GL_NOT_EQUAL, GL_GEQUAL, and GL_GREATER. The comparison works as follows:

;; (ref operator texture)
;; Where ref is the reference value given to the texture lookup function by
;; GLSL, and texture is the value fetched from the texture. So GL_LESS will be
;; true if the reference value is strictly less than the value pulled from the
;; texture.


;; Edge value sampling:
;; Normalized texture coordinates are not limited to values between 0.0 and
;; 1.0. They can be any floating-point number. When a texture coordinate is not
;; within the [0, 1] range, a heuristic must be employed to decide what the
;; color value will be.

;;         ((:texture-wrap-s (:repeat :mirrored-repeat :clamp-to-edge :clamp-to-border
;;                                    :mirror-clamp-to-edge))
;;          (:texture-wrap-t (:repeat :mirrored-repeat :clamp-to-edge :clamp-to-border
;;                                    :mirror-clamp-to-edge))
;;          (:texture-wrap-r (:repeat :mirrored-repeat :clamp-to-edge :clamp-to-border
;;                                    :mirror-clamp-to-edge)))

;; Each dimension of a texture can have a different heuristic. These are set by
;; setting the texture parameters GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T, and
;; GL_TEXTURE_WRAP_R, where S, T, and R are the first 3 texture coordinates in
;; order. The possible heuristics are:

;;     GL_REPEAT: the texture coordinate wraps around the texture. So a texture
;;                coordinate of -0.2 becomes the equivalent of 0.8.
;;     GL_MIRRORED_REPEAT: the texture coordinate wraps around like a mirror.
;;                         -0.2 becomes 0.2, -1.2 becomes 0.8, etc.
;;     GL_CLAMP_TO_EDGE: the texture coordinate is clamped to the [0, 1] range.
;;     GL_CLAMP_TO_BORDER: the texture coordinate is clamped to the [0, 1]
;;                         range, but the edge texels are blended with a
;;                         constant border color.
;;     GL_MIRROR_CLAMP_TO_EDGE: (only available with OpenGL 4.4 or
;;                              ARB_texture_mirror_clamp_to_edge) the texture
;;                              is clamped to the [-1, 1] range, but mirrors the
;;                              negative direction with the positive. Basically,
;;                              it acts as GL_CLAMP_TO_EDGE, except that it
;;                              takes the absolute value of the texture
;;                              coordinates before clamping.

;; This also applies to Rectangle Textures, except that the range at which they
;; apply edge sampling is based on the texel width/height of the texture, not
;; the normalized [0, 1] range. This does not apply to Buffer Textures, as they
;; must use the texelFetch sampling functions and thus cannot sample outside of
;; the texel range of the texture.

;; Border color:
;; The GL_CLAMP_TO_BORDER defines a color that edge texels are blended when
;; texture coordinates fall outside of the valid area of the texture. When this
;; this edge mode is used, a border color must be set with GL_BORDER_COLOR.

;; The border color can be provided in floating-point values, normalized
;; integers, or non-normalized integers, using the various forms of
;; glSamplerParameter/glTexParameter.

;; When using the fv function, the color will be stored as a float. When using
;; iv, the color will be converted to a float via signed normalization. Since
;; the components are GLint, the range is from [-231, 231). When using the Ii or
;; Iui forms, the color will be stored as signed or unsigned integers, as
;; appropriate.

;; The border color will then be converted to a value appropriate for the Image
;; Format of the texture when it is actually used.

;; Note that the border color is a 4-component color, so you must use the v
;; version of the function to provide all four components.
