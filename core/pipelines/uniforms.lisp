(in-package :cepl.pipelines)

;;;--------------------------------------------------------------
;;; UNIFORMS ;;;
;;;----------;;;

(defun uniform-1i (location value)
  (gl:uniformi location value))

(defun uniform-sampler (location image-unit)
  (gl:uniformi location image-unit))

(defun uniform-2i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-2iv location 1 ptr)))

(defun uniform-3i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-3iv location 1 ptr)))

(defun uniform-4i (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-4iv location 1 ptr)))

(defun uniform-1f (location value)
  (gl:uniformf location value))

(defun uniform-2f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-2fv location 1 ptr)))

(defun uniform-3f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-3fv location 1 ptr)))

(defun uniform-4f (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-4fv location 1 ptr)))

(defun uniform-matrix-2ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-2fv location 1 nil ptr)))

(defun uniform-matrix-3ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-3fv location 1 nil ptr)))

(defun uniform-matrix-4ft (location value)
  (cffi-sys:with-pointer-to-vector-data (ptr value)
    (%gl:uniform-matrix-4fv location 1 nil ptr)))

(defun uniform-matrix-2fvt (location count value)
  (%gl:uniform-matrix-2fv location count nil value))

(defun uniform-matrix-3fvt (location count value)
  (%gl:uniform-matrix-3fv location count nil value))

(defun uniform-matrix-4fvt (location count value)
  (%gl:uniform-matrix-4fv location count nil value))

;; [TODO] HANDLE DOUBLES
(defun get-foreign-uniform-function (type)
  (symbol-function (get-foreign-uniform-function-name type)))

(defun get-uniform-function (type)
  (symbol-function (get-uniform-function-name type)))

(defun get-foreign-uniform-function-name (type)
  "Used when uploading from a foreign data source (like a c-array)
   This lets your uploading from an offset the source"
  (case type
    ((:int :int-arb :bool :bool-arb) '%gl:uniform-1iv)
    ((:float :float-arb) '%gl:uniform-1fv)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) '%gl:uniform-2iv)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) '%gl:uniform-3iv)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) '%gl:uniform-4iv)
    ((:vec2 :float-vec2 :float-vec2-arb) '%gl:uniform-2fv)
    ((:vec3 :float-vec3 :float-vec3-arb) '%gl:uniform-3fv)
    ((:vec4 :float-vec4 :float-vec4-arb) '%gl:uniform-4fv)
    ((:mat2 :float-mat2 :float-mat2-arb) 'uniform-matrix-2fvt)
    ((:mat3 :float-mat3 :float-mat3-arb) 'uniform-matrix-3fvt)
    ((:mat4 :float-mat4 :float-mat4-arb) 'uniform-matrix-4fvt)
    (t (if (cepl.samplers::sampler-typep (type-spec->type type)) nil
           (error "Sorry cepl doesnt handle that type yet")))))

(defun get-uniform-function-name (type)
  "Used when uploading lisp data"
  (case type
    ((:int :int-arb :bool :bool-arb) 'uniform-1i)
    ((:float :float-arb) 'uniform-1f)
    ((:int-vec2 :int-vec2-arb :bool-vec2 :bool-vec2-arb) 'uniform-2i)
    ((:int-vec3 :int-vec3-arb :bool-vec3 :bool-vec3-arb) 'uniform-3i)
    ((:int-vec4 :int-vec4-arb :bool-vec4 :bool-vec4-arb) 'uniform-4i)
    ((:vec2 :float-vec2 :float-vec2-arb) 'uniform-2f)
    ((:vec3 :float-vec3 :float-vec3-arb) 'uniform-3f)
    ((:vec4 :float-vec4 :float-vec4-arb) 'uniform-4f)
    ((:mat2 :float-mat2 :float-mat2-arb) 'uniform-matrix-2ft)
    ((:mat3 :float-mat3 :float-mat3-arb) 'uniform-matrix-3ft)
    ((:mat4 :float-mat4 :float-mat4-arb) 'uniform-matrix-4ft)
    (t (if (cepl.samplers::sampler-typep (type-spec->type type)) 'uniform-sampler
           (error "Sorry cepl doesnt handle that type yet")))))
