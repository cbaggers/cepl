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

(defmacro v!ushort (&rest components)
  (v!make '(unsigned-byte 16) components))

(defmacro v!short (&rest components)
  (v!make '(signed-byte 16) components))

;----------------------------------------------------------------

;; {TODO} inline these
(defun v-x (vec)
  "Returns the x component of the vector"
  (aref vec 0))
(defun v-y (vec)
  "Returns the y component of the vector"
  (aref vec 1))
(defun v-z (vec)
  "Returns the z component of the vector"
  (aref vec 2))
(defun v-w (vec)
  "Returns the w component of the vector"  
  (aref vec 3))

(defun (setf v-x) (value vec)
  "Returns the x component of the vector"
  (setf (aref vec 0) (float value)))
(defun (setf v-y) (value vec)
  "Returns the y component of the vector"
  (setf (aref vec 1) (float value)))
(defun (setf v-z) (value vec)
  "Returns the z component of the vector"
  (setf (aref vec 2) (float value)))
(defun (setf v-w) (value vec)
  "Returns the w component of the vector"  
  (setf (aref vec 3) (float value)))
