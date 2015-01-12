(in-package :base-vectors)

(defmacro def-v! (name type)
  `(progn
     (defun ,name (&rest components)
       (let* ((components (loop :for c :in components 
                             :if (typep c 'array)
                             :append (loop :for e :across c :collect
                                        (coerce e ',type))
                             :else :collect (coerce c ',type)))
              (len (length components)))
         (when (or (> len 4) (< len 2))
           (error "Incorrect number of components for a vector: ~a ~a"
                  len components))
         (make-array (length components)
                     :element-type ',type
                     :initial-contents components)))
     (define-compiler-macro ,name (&whole form &rest components)
       (if (every #'numberp components)
           (let ((components (loop :for c :in components :collect
                                (coerce c ',type)))
                 (len (length components)))
             (when (or (> len 4) (< len 2))
               (error "Incorrect number of components for a vector: ~a ~a"
                      len components))
             (list 'make-array len :element-type '',type
                   :initial-contents (list 'quote components)))
           form))))

(def-v! v! single-float)
(def-v! v!int fixnum)
(def-v! v!ubyte (unsigned-byte 8))
(def-v! v!byte (signed-byte 8))
(def-v! v!short (unsigned-byte 16))
(def-v! v!ushort (signed-byte 16))

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
