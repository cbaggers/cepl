(in-package :cepl)

(varjo:add-equivalent-name 'varjo-lang:v! 'cl-game-math.base-vectors:v!)
(varjo:add-equivalent-name 'varjo-lang:s~ 'cl-game-math.vectors:s~)
(varjo:add-equivalent-name 'varjo-lang:m! 'cl-game-math.base-matrices:m!)
(varjo:add-equivalent-name 'varjo-lang:swizzle 'cl-game-math.vectors:swizzle)
(varjo:add-equivalent-name 'varjo-lang:s~ 'cl-game-math.vectors:s~)
(varjo:add-equivalent-name 'cl:length 'cl-game-math.vectors:length)
(varjo:add-equivalent-name 'varjo-lang:dot 'cl-game-math.vectors:dot)
(varjo:add-equivalent-name 'varjo-lang:normalize 'cl-game-math.vectors:normalize)
(varjo:add-equivalent-name 'varjo-lang:normalize 'cl-game-math.vectors:cross)

;;

(varjo:v-defun cl-game-math.vectors:x (a) "~a.x" (varjo:v-vector) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:y (a) "~a.y" (varjo:v-vector) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-vec3)  (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-bvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-ivec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-uvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-dvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-vec4)  (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (varjo:v-dvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (varjo:v-vec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (varjo:v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (varjo:v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (varjo:v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (varjo:v-dvec4) (:element 0)
	       :glsl-spec-matching t)
