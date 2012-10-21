(in-package :base-vectors)

(defmacro v! (&rest components)
  (let ((dimen (length components)))
    (if (or (> dimen 4)
            (< dimen 2))
        (error "Incorrect number of components for a vector")
        `(make-array 
          ,dimen
          :element-type 'single-float
          :initial-contents (list 
                             ,@(loop for i in components
                                  collect
                                    (if (numberp i)
                                        (coerce i 'single-float)
                                        `(coerce ,i 'single-float))))))))

;----------------------------------------------------------------

;; These have been defined as macros as it want to guarantee they
;; are 'inlined' 
(defmacro v-x (vec)
  "Returns the x component of the vector"
  `(aref ,vec 0))

(defmacro v-y (vec)
  "Returns the y component of the vector"
  `(aref ,vec 1))

(defmacro v-z (vec)
  "Returns the z component of the vector"
  `(aref ,vec 2))

(defmacro v-w (vec)
  "Returns the w component of the vector"  
  `(aref ,vec 3))
