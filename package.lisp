;;;; package.lisp

(defpackage :base-maths
  (:use :cl)
  (:export :+float-threshold+
	   :+one-degree-in-radians+
	   :+pi+
	   :float-zero
	   :float-zero-sq
	   :c-sqrt
	   :c-inv-sqrt))

(defpackage :base-macros
  (:use :cl)
  (:export :defun-memobind
	   :defmemo
	   :continuable))

(defpackage :cepl-time
  (:use :cl)
  (:export :make-time-buffer
	   :make-stepper
	   :on-step-call))

(defpackage :cepl-utils
  (:use :cl)
  (:nicknames :utils)
  (:export :walk-replace
	   :file-to-string
	   :flatten
	   :mkstr
	   :symb
	   :make-keyword
	   :group))

(defpackage :cepl-gl
  (:use :cl :cl-opengl :cffi)
  (:nicknames :cgl)
  (:shadow :buffer-data
	   :bind-buffer
	   :enable-vertex-attrib-array
	   :vertex-attrib-pointer
	   :uniform-matrix
	   :define-gl-array-format
	   :alloc-gl-array
	   :bind-vertex-array
	   :use-program
	   :make-program)
  (:export :cgl-compile-shader-error 
	   :enable-vertex-attrib-array
	   :vertex-attrib-pointer 
	   :uniform-matrix
	   :gen-buffer
	   :buffer-data
	   :bind-buffer
	   :multi-populate-buffer
	   :define-gl-array-format
	   :destructuring-populate
	   :alloc-array-gl
	   :bind-vertex-attrib-pointer
	   :bind-vao
	   :bind-vertex-array
	   :make-vao
	   :gl-stream
	   :make-gl-stream
	   :program-attrib-count
	   :program-attributes
	   :program-uniform-count
	   :program-uniforms
	   :get-uniforms
	   :use-program
	   :shader-type-from-path
	   :make-shader
	   :link-shaders
	   :make-program
	   :set-program-uniforms))


(defpackage :math-macros
  (:use :cl)
  (:export :apply-across-elements
	   :v-x :v-y :v-z :v-w))

(defpackage :vector2
  (:use :cl)
  (:nicknames :v2)
  (:export :make-vector2 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :perp-dot
	   :*unit-x* :*unit-y* :*unit-scale*)
  (:import-from :base-maths :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y))

(defpackage :vector3
  (:use :cl)
  (:nicknames :v3)
  (:export :make-vector3 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-scale*)
  (:import-from :base-maths :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z))

(defpackage :vector4
  (:use :cl)
  (:nicknames :v4)
  (:export :make-vector4 :v= :v/= :v+ :v+1 :v- :v-1 :v*
	   :v*vec :v/ :v/vec :negate :vlength-squared
	   :vlength :distance-squared :distance :dot
	   :absolute-dot :normalize :cross
	   :*unit-x* :*unit-y* :*unit-z* :*unit-w* :*unit-scale*)
  (:import-from :base-maths :float-zero
		      :float-zero-sq
		      :c-sqrt
		      :c-inv-sqrt)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

(defpackage :matrix3
  (:use :cl)
  (:nicknames :m3)
  (:export :melm :identity-matrix3 :zero-matrix3 
	   :make-matrix3 :make-from-rows :get-rows
	   :get-row :make-from-columns :get-columns
	   :get-column :determinate-cramer :inverse
	   :mzerop :identityp :meql :transpose :adjoint
	   :mtrace :make-rotation-matrix-euler 
	   :make-rotation-mat-aa :make-scale-matrix-vec
	   :make-scale-matrix :make-xrotation-matrix 
	   :make-yrotation-matrix :make-zrotation-matrix
	   :get-fixed-angles :get-axis-angle :m+ :m- :negate
	   :m* :m*vec)
  (:import-from :base-maths :float-zero
		      :c-sqrt)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

;;[TODO] why does adding :vector3 in the :use cause conflicts?
(defpackage :matrix4
  (:use :cl)
  (:nicknames :m4)
  (:export :melm :identity-matrix4 :zero-matrix4 
	   :2dclipspace-to-imagespace-matrix4 :make-matrix4
	   :mzerop :identityp :meql :minor :adjoint 
	   :determinant :affine-inverse :transpose 
	   :translation :rotation-from-matrix
	   :rotation-from-euler :rotation-from-axis-angle
	   :scale :rotation-x :rotation-y
	   :rotation-z :get-fixed-angles :mtrace
	   :get-axis-angle :m+ :m- :negate :m*scalar
	   :mcol*vec4 :mrow*vec4 :m* :transform)
  (:import-from :base-maths :float-zero
		      :c-sqrt)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4)
  (:import-from :math-macros
		:apply-across-elements :v-x :v-y :v-z :v-w))

(defpackage :cepl
  (:use :cl)
  (:export :draw-elements-base-vertex
	   :calculate-frustrum-scale
	   :with-init-cepl
	   :make-cam-clip-matrix)
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4))

(defpackage :defunct
  (:use :cl)
  (:export :restartable :with-bind-buffer :with-bind-vao
	   :with-use-program :make-gl-array-from-array
	   :setup-buffer :sub-buffer :draw-elements-base-vertex
	   :file-to-string :make-shader :shader-type-from-path
	   :make-program :calculate-frustrum-scale)
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4))

(defpackage :arc-tuts
  (:use :cl :defunct )
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4)
  (:import-from :math-macros
		:v-x :v-y :v-z :v-w))

(defpackage :cepl-examples
  (:use :cl)
  (:import-from :vector2
		:make-vector2)
  (:import-from :vector3 
		:make-vector3)
  (:import-from :vector4
		:make-vector4)
  (:import-from :math-macros
		:v-x :v-y :v-z :v-w))
