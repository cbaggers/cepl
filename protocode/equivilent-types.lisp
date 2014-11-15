
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gl-equivilent-type-data* (make-hash-table)))

(defmacro def-gl-equivilent (lisp-type &body body)
  (unless lisp-type
       (error "lisp-type may not be null"))
  (multiple-value-bind (spec shader-macros)
      (if (symbolp (first body))
          (apply #'process-direct lisp-type body)
          (process-compound lisp-type body))
    `(progn (eval-when (:compile-toplevel :load-toplevel :execute)     
              (setf (gethash ,lisp-type *gl-equivilent-type-data*) ,spec))
            ,@shader-macros)))

(defun process-direct (lisp-type &key type converter)
  (unless type
    (error "type may be nil in the fields of a compound equivilent type but not in this as it is a direct equivilent type" ))
  (values
   `(:direct ,lisp-type ,type ,converter)
   nil))

(defun process-compound (lisp-type fields)
  (values
   `(:compound
     ,lisp-type
     ,@(loop :for (name &key type converter) :in fields :collect
          `(,name ,type ,converter)))
   (loop :for (name &key type converter) :in fields :collect
      (if type
          `(defsmacro ,name (x)
             `(assert :valid-variable ,x
                      ,,(format nil "Varjo: Accessor ~a is not valid for this argument" name)))
          (if converter
              (error "You cannot specify a converter for a nil typed field")
              `(defsmacro ,name (x)
                 `(assert :always-fail ,x
                          ,,(format nil "Varjo: Accessor ~a is not valid inside shaders" name))))))))

(defun valid-type (x)
  (handler-case (progn (typep t x) t) (error () nil)))

;; --- ideas ---

(defclass camera () 
  ((cam->clip :type (simple-array single-float (16)) :reader cam->clip)
   (cam->clip-func :initform nil :initarg :cam->clip-func )
   (frame-size :reader frame-size :initarg :frame-size)
   (near :type single-float :reader near :initarg :near)
   (far :type single-float :reader far :initarg :far)
   (fov :type single-float :reader fov :initarg :fov)))

(def-gl-equivilent camera
  (cam->clip :type :mat4) ;; no converter means just pass to uniform uploader
  (clip->cam :type :mat4 :converter #'invert-matrix) ;; can have lambda here
  (cam->clip-func :type nil) ;; this means access will cause error
  (frame-size :type :vec2)
  (near :type :float)
  (far :type :float)
  (fov :type :float))

;; if no fields do this
(def-gl-equivilent string :type :int :converter #'sxhash)

;; would like this but not sure how to do unknown length arrays
(def-gl-equivilent string :type (:int *) :converter
                   (lambda (x) (loop for i across x collect (char-code i))))

;; also can handle complex types
(def-gl-equivilent (simple-array single-float (3)) :type :vec3)

;; varjo needs assert special function where if the assert is applied to the
;; body and if the result is true the body is injected, else there is an error.
(assert (:type :float) 1.3 "form not of type float")

;; hmm, really I just need type that varjo will allow, maybe this should go into
;; varjo itself.

(defclass camera () (v-type)
  ())

;; ok so the issue is that macros are global so making the accessors for them is
;; fine but checking the param type is hard
;; ...actually by allowing assert to control the error message this is fine.
(defvshader meh (stream &uniform (x camera))
  (out (x) (cam->clip x)))

(defvshader meh (stream &uniform (x--cam->clip :mat4) (x--clip->cam :mat4)
                        (x--frame-size :vec2) (x--near :float) (x--far :float)
                        (x--fov :float))
  (out (x) (assert (:compiles) x--cam->clip "argument x is not of type camera")))

;; We will use packages rather than name mangling to hide the symbols,
;; it's gonna rock!



;; We should also add bake-pipeline which looks at what things are actually used
;; and recompiles the pipeline function with optimizations
