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


