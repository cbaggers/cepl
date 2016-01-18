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

(varjo:v-defun cl-game-math.vectors:x (a) "~a.x" (v-vector) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:y (a) "~a.y" (v-vector) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-vec3)  (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-bvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-ivec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-uvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-dvec3) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-vec4)  (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:z (a) "~a.z" (v-dvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (v-vec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (v-bvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (v-ivec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (v-uvec4) (:element 0)
	       :glsl-spec-matching t)

(varjo:v-defun cl-game-math.vectors:w (a) "~a.w" (v-dvec4) (:element 0)
	       :glsl-spec-matching t)
