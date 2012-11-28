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

(defun gl> (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '> (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl< (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '< (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl>= (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '>= (mapcar #'code args))
       :percolate-to-block (mapcan #'percolate-to-block args)
       :percolate-to-top (mapcan #'percolate-to-top args))
      (error "Types do not match, cannot add them together")))

(defun gl<= (&rest args)
  (if (types-match args)
      (make-instance 
       (class-name-sym (first args))
       :code (cepl-utils:intersperse '<= (mapcar #'code args))
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

;; [TODO] Can we generalise these into the macro?
(slquickdef min ((x gl-gen) (y gl-gen gl-float)))
(slquickdef min ((x gl-igen) (y gl-igen gl-int)) 
	    :dont-write-generic t)
(slquickdef min ((x gl-ugen) (y gl-ugen gl-uint)) 
	    :dont-write-generic t)

(slquickdef max ((x gl-gen) (y gl-gen gl-float)))
(slquickdef max ((x gl-igen) (y gl-igen gl-int)) 
	    :dont-write-generic t)
(slquickdef max ((x gl-ugen) (y gl-ugen gl-uint)) 
	    :dont-write-generic t)

(slquickdef clamp ((x gl-gen) (min-val gl-gen) (max-val gl-gen)))
(slquickdef clamp ((x gl-gen) (min-val gl-float) (max-val gl-float))
	    :dont-write-generic t)
(slquickdef clamp ((x gl-igen) (min-val gl-igen) (max-val gl-igen))
	    :dont-write-generic t)
(slquickdef clamp ((x gl-igen) (min-val gl-int) (max-val gl-int))
	    :dont-write-generic t)
(slquickdef clamp ((x gl-ugen) (min-val gl-ugen) (max-val gl-ugen))
	    :dont-write-generic t)
(slquickdef clamp ((x gl-ugen) (min-val gl-uint) (max-val gl-uint))
	    :dont-write-generic t)

;; arghh
(slquickdef mix ((x gl-gen) (y gl-gen) (a gl-gen gl-float gl-bvec)))

(slquickdef step ((edge gl-gen gl-float) (x gl-gen)))

(slquickdef smoothstep ((edge0 gl-gen) 
			(edge1 gl-gen) 
			(x gl-gen gl-float gl-bvec)))
(slquickdef smoothstep ((edge0 gl-float) 
			(edge1 gl-float) 
			(x gl-gen gl-float gl-bvec)) 
	    :dont-write-generic t)

