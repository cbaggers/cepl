(in-package :base-vectors)

(defun v!make (type components)
  (let ((dimen (length components)))
    (if (or (> dimen 4)
            (< dimen 2))
        (error "Incorrect number of components for a vector")
        `(make-array 
          ,dimen
          :element-type ',type
          :initial-contents (list 
                             ,@(loop for i in components
                                  collect
                                    (if (numberp i)
                                        (coerce i type)
                                        `(coerce ,i ',type))))))))

(defmacro v! (&rest components)
  (v!make 'single-float components))

(defmacro v!int (&rest components)
  (v!make 'fixnum components))

(defmacro v!ubyte (&rest components)
  (v!make '(unsigned-byte 8) components))

(defmacro v!byte (&rest components)
  (v!make '(signed-byte 8) components))

;----------------------------------------------------------------

;; These have been defined as macros as it want to guarantee they
;; are 'inlined' [TODO] This is a crap idea, trust the compiler 
;;                      and inline them properly. 
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
