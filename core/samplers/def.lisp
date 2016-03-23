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
(defun calc-sampler-type (texture-type internal-format &optional shadow-sampler)
  "Makes the keyword that names the sampler-type for the given texture-type and format"
  (cepl-utils:kwd
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
