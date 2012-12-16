(in-package :cglsl)

;;------------------------------------------------------------------
;; GLSL Core -
;;------------

;; These are core bits of the language. For the sake of tidyness
;; the bulk of the language is moved into other files.
;; Actually plenty of this is homeless code that will get sorted 
;; into another file later but hey ho!

(def-gl-types ((gen float (vec vec2 vec3 vec4)) 
	       (igen int (ivec ivec2 ivec3 ivec4))
	       (ugen uint (uvec uvec2 uvec3 uvec4)) 
	       (bgen bool (bvec bvec2 bvec3 bvec4))
	       (mgen (mat2 mat2x2) (mat3 mat3x3) 
		     (mat4 mat4x4) mat2x3 mat2x4
		     mat3x2 mat3x4 mat4x2 mat4x3) 
	       (sampler
		(fsampler sampler1D sampler2D sampler3D
			  samplerCube sampler2DRect
			  sampler1DShadow sampler2DShadow 
			  sampler2DRectShadow
			  sampler1DArray sampler2DArray
			  sampler1DArrayShadow 
			  sampler2DArrayShadow
			  samplerBuffer sampler2DMS
			  sampler2DMSArray) 
		(isampler isampler1D isampler2D isampler3D
			  isamplerCube isampler2DRect
			  isampler1DArray isampler2DArray
			  isamplerBuffer isampler2DMS
			  isampler2DMSArray) 
		(usampler usampler1D usampler2D usampler3D
			  usamplerCube usampler2DRect
			  usampler1DArray usampler2DArray
			  usamplerBuffer usampler2DMS
			  usampler2DMSArray) )
	       number))


(defun gl-type (obj)
  (class-name (class-of obj)))


(defun gl-percolate-to-block (&rest args)
  (let ((last-index (- (length args) 1)))
    (make-instance 
     (gl-type (elt args last-index))
     :code (code (elt args last-index))
     :percolate-to-block (append 
			  (mapcar #'code (subseq args 0 last-index))
			  (mapcan #'percolate-to-block args))
     :percolate-to-top (mapcan #'percolate-to-top args))))

(defun gl-setf (var val)
  (make-instance 
   (gl-type var)
   :code `(,(code var) = ,(code val))
   :percolate-to-block (append (percolate-to-block var)
			       (percolate-to-block val))
   :percolate-to-top (append (percolate-to-top var)
			     (percolate-to-top val))))

(defmacro gl-let* (bindings &body body)
  (sublis 
   *symbol-map*
   (let ((name-map (loop for binding in bindings
			 collect (cons (caar binding)
				       `(make-instance 
					 ',(cadar binding)
					 :code ',(glsym))))))
     (sublis
      name-map
      `(gl-percolate-to-block
	,@(loop for binding in bindings
		collect `(gl-instan ,(list 'setf 
					(caar binding) 
					(cadr binding))))
	,@body)))))

(defun gl-instan (x)
  (make-instance 
   (class-name-sym x)
   :code `(,(class-name-sym x) ,@(code x))
   :percolate-to-block (percolate-to-block x)
   :percolate-to-top (percolate-to-top x)))


;; type converting constructors
(slquickdef toint ((x gl-bool gl-float gl-uint)))
(slquickdef tofloat ((x gl-bool gl-int gl-uint)))
(slquickdef tobool ((x gl-float gl-int gl-uint)))
(slquickdef tounit ((x gl-bool gl-float gl-int)))

;;------------------------------------------------------------------
;; More type stuff ;;
;;-----------------;;

;; Figure 4-2. Standardized Atomic Type Specifiers

;; If a type specifier is a list, the car of the list is a symbol, 
;; and the rest of the list is subsidiary type information. Such a
;; type specifier is called a compound type specifier. Except as
;; explicitly stated otherwise, the subsidiary items can be
;; unspecified. The unspecified subsidiary items are indicated by
;; writing *. For example, to completely specify a vector, the type
;; of the elements and the length of the vector must be present.
;;
;;  (vector double-float 100)
;;
;; The following leaves the length unspecified:
;;
;;  (vector double-float *)
;;
;; The following leaves the element type unspecified:
;;
;;  (vector * 100)                    
;;
;; (simple-array single-float (3))
