(in-package :cglsl)

(defun gl+ (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '+ (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl- (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '- (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot subtract them from each other")))

(defun gl* (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '* (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot subtract them from each other")))

(defun gl/ (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '/ (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl% (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '% (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl> (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '> (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl< (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '< (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl>> (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '>> (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl<< (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '<< (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl>= (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '>= (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl<= (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '<= (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl= (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '= (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl== (&rest args)
  (if (types-match args)
      (make-instance 
       'gl-bool
       :code (cepl-utils:intersperse '== (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl& (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '& (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl&& (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '&& (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl^^ (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '^^ (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl|| (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '|| (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl? (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '?\: (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

;;------------------------------------------------------------------

(defgeneric gl-radians (degrees)
  (:documentation ""))

(defmethod gl-radians ((degrees gl-gen))
  (make-instance 
   (class-name-sym degrees)
   :code `(radians (,( code degrees)))
   :percolate-to-block (percolate-to-block degrees)
   :percolate-to-top (percolate-to-top degrees)))

;;------------------------------------------------------------------

(defgeneric gl-degrees (radians)
  (:documentation ""))

(defmethod gl-degrees ((radians gl-gen))
  (make-instance 
   (class-name-sym radians)
   :code `(degrees (,( code radians)))
   :percolate-to-block (percolate-to-block radians)
   :percolate-to-top (percolate-to-top radians)))

;;------------------------------------------------------------------

(slquickdef sin ((angle gl-gen)))

(slquickdef cos ((angle gl-gen)))

(slquickdef tan ((angle gl-gen)))

(slquickdef asin ((x gl-gen)))

(slquickdef acos ((x gl-gen)))

(slquickdef atan ((y gl-gen) (x gl-gen)))

(slquickdef sinh ((x gl-gen)))

(slquickdef cosh ((x gl-gen)))

(slquickdef tanh ((x gl-gen)))

(slquickdef asinh ((x gl-gen)))

(slquickdef acosh ((x gl-gen)))

(slquickdef atanh ((x gl-gen)))

(slquickdef pow ((base gl-gen) (power gl-gen)))

(slquickdef exp ((x gl-gen)))

(slquickdef log ((x gl-gen)))

(slquickdef exp2 ((x gl-gen)))

(slquickdef log2 ((x gl-gen)))

(slquickdef sqrt ((x gl-gen)))

(slquickdef inversesqrt ((x gl-gen)))

(slquickdef abs ((x gl-gen gl-igen)))

(slquickdef sign ((x gl-gen gl-igen)))

(slquickdef floor ((x gl-gen)))

(slquickdef trunc ((x gl-gen)))

(slquickdef round ((x gl-gen)))

(slquickdef roundeven ((x gl-gen)))

(slquickdef ceil ((x gl-gen)))

(slquickdef fract ((x gl-gen)))

(slquickdef mod ((x gl-gen) (y gl-float gl-gen)))

;; [TODO] Look into modf

(slquickdef min (((x gl-gen) (y gl-gen gl-float))
		 ((x gl-igen) (y gl-igen gl-int)) 
		 ((x gl-ugen) (y gl-ugen gl-uint)))
	    :multi-type-arg t ) 

(slquickdef max (((x gl-gen) (y gl-gen gl-float))
		 ((x gl-igen) (y gl-igen gl-int)) 
		 ((x gl-ugen) (y gl-ugen gl-uint)))
	    :multi-type-arg t) 

(slquickdef clamp 
	    (((x gl-gen) (min-val gl-gen) (max-val gl-gen))
	     ((x gl-gen) (min-val gl-float) (max-val gl-float))
	     ((x gl-igen) (min-val gl-igen) (max-val gl-igen))
	     ((x gl-igen) (min-val gl-int) (max-val gl-int))
	     ((x gl-ugen) (min-val gl-ugen) (max-val gl-ugen))
	     ((x gl-ugen) (min-val gl-uint) (max-val gl-uint)))
	    :multi-type-arg t)

(slquickdef mix ((x gl-gen) (y gl-gen) (a gl-gen gl-float gl-bgen)))

(slquickdef step ((edge gl-gen gl-float) (x gl-gen)))

(slquickdef smoothstep (((edge0 gl-gen) 
			 (edge1 gl-gen) 
			 (x gl-gen gl-float gl-bvec))
			((edge0 gl-float) 
			 (edge1 gl-float) 
			 (x gl-gen gl-float gl-bvec))) 
	    :multi-type-arg t)

;; can these return bool vectors?
(slquickdef isnan ((x gl-gen)) :out-type 'gl-bool)
(slquickdef isinf ((x gl-gen)) :out-type 'gl-bool)
(slquickdef floatbitstoint ((x gl-gen)) :out-type 'gl-int)
(slquickdef floatbitstouint ((x gl-gen)) :out-type 'gl-uint)
(slquickdef "intBitsToFloat" ((x gl-gen)) :out-type 'gl-int)
(slquickdef "uintBitsToFloat" ((x gl-gen)) :out-type 'gl-uint)

(slquickdef length ((x gl-gen)) :out-type 'gl-uint)
(slquickdef distance ((x gl-gen) (y gl-gen)) :out-type 'gl-uint)
(slquickdef dot ((x gl-gen) (y gl-gen)) :out-type 'gl-uint)
(slquickdef cross ((x gl-vec3) (y gl-vec3)) :out-type 'gl-vec3)
(slquickdef normalize ((x gl-gen)))
;;need to add frtransform and faceforward
(slquickdef reflect ((i gl-gen) (n gl-gen)))
(slquickdef refract ((i gl-gen) (n gl-gen) (eta gl-float)))
(slquickdef "matrixCompMult" ((x gl-mgen) (y gl-mgen)))

(slquickdef "outerProduct" ((c gl-vec2) (r gl-vec2)) 
	    :out-type 'gl-mat2) 
(slquickdef "outerProduct" ((c gl-vec3) (r gl-vec3)) 
	    :out-type 'gl-mat3
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec4) (r gl-vec4)) 
	    :out-type 'gl-mat4
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec3) (r gl-vec2)) 
	    :out-type 'gl-mat2x3
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec2) (r gl-vec3)) 
	    :out-type 'gl-mat3x2
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec4) (r gl-vec2)) 
	    :out-type 'gl-mat4x2
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec2) (r gl-vec4)) 
	    :out-type 'gl-mat2x4
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec4) (r gl-vec3)) 
	    :out-type 'gl-mat3x4
	    :dont-write-generic t)
(slquickdef "outerProduct" ((c gl-vec3) (r gl-vec4)) 
	    :out-type 'gl-mat4x3
	    :dont-write-generic t)

(slquickdef transpose ((m gl-mgen)))
(slquickdef determinant (((a gl-mat2)) ((b gl-mat3)) ((c gl-mat4)))
	    :out-type 'gl-float
	    :multi-type-arg t)
(slquickdef inverse ((m gl-mgen)))
(slquickdef "lessThan" (((x gl-vec) (y gl-vec))
		      ((x gl-ivec) (y gl-ivec))
		      ((x gl-uvec) (y gl-uvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t)
(slquickdef "greaterThan" (((x gl-vec) (y gl-vec))
			 ((x gl-ivec) (y gl-ivec))
			 ((x gl-uvec) (y gl-uvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t)
(slquickdef "lessThanEqual" (((x gl-vec) (y gl-vec))
			   ((x gl-ivec) (y gl-ivec))
			   ((x gl-uvec) (y gl-uvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t)
(slquickdef "greaterThanEqual" (((x gl-vec) (y gl-vec))
			      ((x gl-ivec) (y gl-ivec))
			      ((x gl-uvec) (y gl-uvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t )
(slquickdef equal (((x gl-vec) (y gl-vec))
		   ((x gl-ivec) (y gl-ivec))
		   ((x gl-uvec) (y gl-uvec))
		   ((x gl-bvec) (y gl-bvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t )
(slquickdef "notEqual" (((x gl-vec) (y gl-vec))
			((x gl-ivec) (y gl-ivec))
			((x gl-uvec) (y gl-uvec))
			((x gl-bvec) (y gl-bvec)))
	    :out-type 'gl-bvec
	    :multi-type-arg t )
(slquickdef any ((x gl-bvec))
	    :out-type 'gl-bool)
(slquickdef all ((x gl-bvec))
	    :out-type 'gl-bool)
(slquickdef not ((x gl-bvec))
	    :out-type 'gl-bool)

;;these are only for fragment shaders
(slquickdef "dFdx" ((p gl-gen)))
(slquickdef "dFdy" ((p gl-gen)))
(slquickdef fwidth ((p gl-gen)))

;;availabel to all
(slquickdef noise1 ((x gl-gen))
	    :out-type 'gl-float)
(slquickdef noise2 ((x gl-gen))
	    :out-type 'gl-vec2)
(slquickdef noise3 ((x gl-gen))
	    :out-type 'gl-vec2)
(slquickdef noise4 ((x gl-gen))
	    :out-type 'gl-vec2)
