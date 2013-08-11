;;===============================================================
;; Image Formats 
;;===============

;; Remember: The pixel format is the format in which you provide the
;; texture data, the internal format is how OpenGL will store it internally

;; three basic kinds of image formats: color, depth, and depth/stencil
;; for now I will ignore stencil-only

;; Colors in OpenGL are stored in RGBA format. Image formats do not have
;; to store each component. When the shader samples such a texture, 
;; it will resolve to a 4-value vector

;; Stored 3 ways: normalized integers, floating-point, or integral.
;; normalized integer & floating-point resolves to fvector
;; Normalized integer have 2 kinds: unsigned and signed
;; integral formats resolves to ivector

;; |---------bits--------------------------| |--type---| (rgtc,bptc,
;; red green blue alpha shared depth stencil compression


;; ^^-- above is not unique. Need better system
(defun image-format (&rest args)
  (declare (ignore args))
  (error "Have not implemented this yet as I need a friendly user experience."))

(defclass image-format ()
  ((num-of-components :initform 4) 
   (component-order :initform :rgba) ;
   (component-type :int) ; unsigned-normalized-integer 
                                        ; signed-normalized-integer float signed-integral
                                        ; unsigned-integral
   (component-byte-sizes :initform '(8 8 8 8))))


;; normalized integer and floating-point formats resolve to :vec* values
;; integral formats resolve to :ivec*

;;internal format is how textures and renderbuffers store data
(defun internal-format (component-type &key s r g b a depth stencil 
                                         order compressed 
                                         (guaranteed-only t))
  (cond ((and (or depth stencil) (not (or s r g b a)))
         (print "stencil"))
        ()
        (t (error "That combination of parameters in invalid"))))

;; "": No type suffix means unsigned normalized integer format.
;; "_SNORM": Signed normalized integer format.
;; "F": Floating-point. Thus, GL_RGBA32F is a floating-point format where 
;;      each component is a 32-bit IEEE floating-point value.

;; "I": Signed integral format. Thus GL_RGBA8I gives a signed integer format 
;; where each of the four components is an integer on the range [-128, 127].

;; "UI": Unsigned integral format. The values go from [0, MAX_INT] for the 
;; integer size.



